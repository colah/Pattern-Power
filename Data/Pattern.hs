{-# LANGUAGE ViewPatterns, ScopedTypeVariables, TemplateHaskell, GADTs #-}

module Data.Pattern (PrePat (..), ListPatMods (..), (~$), finishPat, finishPatHelper) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Data
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Control.Monad (guard)

-- The data structures that will be used to externally represent
-- the desired patterns.

-- Note regarding (Exp, a) type things:
-- The former should be a template haskell representation of the second.

-- It is a very sad necessity because we need to be able to run 
-- the functions both at compile and run time.

data PrePat a b where
	Wild       :: PrePat a b
	Free       :: String -> PrePat a a
	Guard      :: (Exp, a -> Bool) -> PrePat a b -> PrePat a b
	Const      :: (Eq a) => (Exp, a) -> PrePat a b
	PreProcess :: (Exp, a -> Maybe b) -> PrePat b c -> PrePat a c
	ListPat    :: (Eq b) => [ListPatMods a] -> [PrePat a b] -> PrePat [a] b

data ListPatMods a = Commutative | FillMissing (Exp, a) | CompressExtra (Exp, [a] -> a)

-- Our internal representation:

data Pattern matched bound = Pattern 
	[String]                  -- ^ Variables
	(matched -> [[bound]])    -- ^ produce a list of possible values to correspond with vars

-- convert a PrePat into an Exp of a PrePat

toExp :: PrePat a b -> Q Exp
toExp Wild = 
	[| Wild |]
toExp (Free name) = 
	[| Free $(litE $ StringL name) |]
toExp (Guard (exp, _) child) = 
	[| Guard ( $(dataToExpQ (const Nothing) exp), $(return $ exp) ) $(toExp child) |]
toExp (Const (exp, _) ) = 
	[| Const ( $(dataToExpQ (const Nothing) exp), $(return $ exp) ) |]
toExp (PreProcess (exp, _) child) = 
	[| PreProcess ( $(dataToExpQ (const Nothing) exp), $(return $ exp) ) $(toExp child) |]
toExp (ListPat mods child) = 
	[| ListPat $(listE $ map patModToExp mods) $(listE $ map toExp child) |]
	where
		patModToExp Commutative = 
			[| Commutative |]
		patModToExp 	(FillMissing (exp, _)) = 
			[| FillMissing ( $(dataToExpQ (const Nothing) exp), $(return $ exp) )  |]
		patModToExp (CompressExtra (exp, _)) = 
			[| CompressExtra ( $(dataToExpQ (const Nothing) exp), $(return $ exp) ) |]


-- Convert an external PrePat into our internal representation, Pat

toPat :: PrePat a b -> Pattern a b
toPat Wild = wild
toPat (Free name) = free name
toPat (Guard (_, rule) child ) = mustPass rule (toPat child)
toPat (Const (_,a) ) = constPat a
toPat (PreProcess (_, preprocessor) child ) = preprocess preprocessor (toPat child)
toPat (ListPat mods prepats) =
	let
		pats = map toPat prepats
		isC Commutative = True
		isC _ = False
		isFill (FillMissing a) = True
		isFill _ = False
		isCompress a = not $ (isFill a || isC a)
		listPatUsed = if null $ filter isC mods then listPat else listPatC
	in case (filter isFill mods, filter isCompress mods) of
		((FillMissing (_, fill)):_, (CompressExtra (_,compress)):_ ) -> 
			requireEqVarsEq $ flexifyListPat listPatUsed (Just fill) (Just compress) pats
		([], (CompressExtra (_, compress) ):_ ) ->
			requireEqVarsEq $ flexifyListPat listPatUsed (Nothing) (Just compress) pats
		((FillMissing (_, fill)):_, []) -> 
			requireEqVarsEq $ flexifyListPat listPatUsed (Just fill) (Nothing) pats
		([],[]) -> requireEqVarsEq $ listPatUsed pats

		
-- For testing patterns we are designing, one can use PrePat

(requireEqVarsEq . toPat -> Pattern vars destructor) ~$ (destructor -> (vals : _ )) | length vars == length vals = 
		Just (zip vars vals)
(requireEqVarsEq . toPat -> Pattern vars destructor) ~$ (destructor ->      []    ) = Nothing
_ ~$ _ = error "bad Pattern"


-- Building blocks for internal patterns; these actually contain the functionality of the desired pattern.

wild :: Pattern matched bound
wild = Pattern [] (\_ -> [[]])

free name = Pattern [name] (\a -> [[a]])

mustPass test (Pattern vars destructor) = Pattern vars testedDestructor
	where
		testedDestructor a | test a = destructor a
		testedDestructor a          = []
	
constPat a = Pattern [] (\b -> if b == a then [[]] else [])

preprocess preprocessor (Pattern vars childFunc) = Pattern vars matchFunc
	where 
		matchFunc (preprocessor -> Just vals) = childFunc vals
		matchFunc               _             = []

listPat :: [Pattern a b] -> Pattern [a] b
listPat patterns = Pattern (concat varListLists) (combineFuncs funcList)
	where 
		varListLists = map (\(Pattern varList _) -> varList) patterns
		funcList     = map (\(Pattern _    func) -> func   ) patterns
		combineFuncs :: [ a -> [[b]] ] -> [a] -> [[b]]
		combineFuncs (func:  [] ) (a:[]) = func a
		combineFuncs (func:funcs) (a:as) = do
			b <- func a
			b2 <- combineFuncs funcs as
			return $ b ++ b2

listPatC :: 
	[Pattern a b] -- Things we want to match
	-> Pattern [a] b   -- Resulting pattern
listPatC patterns =
	let
		patternVars  = concat $ map getVars patterns
		patternFuncs = map getFunc patterns

		matchFunc (pat1:pats) vals | length vals == length (pat1:pats) = do
			([solPart1], solRest) <- subOfLenAndOthers 1 [] vals
			ansPart1 <- pat1 solPart1
			ansRest <- matchFunc pats solRest
			return (ansPart1 ++ ansRest)
		matchFunc [] [] = [[]]
		matchFunc _ _ = []
			
	in Pattern patternVars (matchFunc patternFuncs)


flexifyListPat :: 
	(                          -- the listPat we're making flexible
		[Pattern a b]       -- Things we want to match
		-> Pattern [a] b       -- Resulting pattern
	)-> Maybe a            -- fallback a, for when to few inputs to match pat
	->  Maybe ([a] -> a)   -- builds an a from as, for when there are too many
	-> [Pattern a b]       -- Things we want to match
	-> Pattern [a] b       -- Resulting pattern

flexifyListPat listPat posFallback posMerger pats =
	let
		childPat = listPat pats
		childMatchFunc = getFunc childPat
		matchFunc vals = 
			case (compare (length vals) (length pats), posFallback, posMerger) of
				(LT, Just fallback, _) -> 
					childMatchFunc $ vals ++ replicate (length pats - length vals) fallback
				(GT, _, Just merger) ->  do
					(most, extra) <- subOfLenAndOthers (length pats -1) [] vals
					childMatchFunc $ most ++ [merger extra]
				(EQ, _, _) -> childMatchFunc vals
				_          -> []

	in Pattern (getVars childPat) (matchFunc)

transformBefore :: (a -> b) -> Pattern b c -> Pattern a c
transformBefore transform (Pattern vars matchFunc) = 
	Pattern vars (matchFunc . transform)

tryTransformBefore :: (a -> Maybe b) -> Pattern b c -> Pattern a c
tryTransformBefore transform (Pattern vars childMatchFunc) = 
	Pattern vars matchFunc
		where
			matchFunc (transform -> Just input) = childMatchFunc input
			matchFunc            _              = []


requireEqVarsEq :: (Eq b) => Pattern a b -> Pattern a b
requireEqVarsEq (Pattern vars childFunc) = Pattern (List.nub vars) matchFunc
	where
		
		allSame :: Eq a => [a] -> Bool
		allSame (x1:x2:xs) = x1 == x2 && allSame (x2:xs)
		allSame      _     = True
		
		varCopies = filter ((>1) . length . snd) $ collectStringCopies vars
		
		matchFunc (childFunc -> possibleVals) = do
			vals <- possibleVals
			guard $ all allSame [ map (vals !!) poses | (_, poses) <- varCopies]
			return $ foldl (.) id [foldl (.) id [dropPos pos | pos <- tail poses] | (_,poses) <- varCopies] $ vals


finishPat :: PrePat a b -> Q Pat
finishPat prepat = 
	let
		pat = toPat prepat
		vars = getVars pat
		varpat = ListP $ map ( VarP . mkName) vars
	in 
		viewP [| finishPatHelper $(toExp prepat)|]  (return $ InfixP varpat (mkName ":") WildP)

finishPatHelper :: (Eq b) => PrePat a b -> a -> [[b]]
finishPatHelper = getFunc . requireEqVarsEq . toPat


subOfLenAndOthers :: Int -> [a] -> [a] -> [([a],[a])]

subOfLenAndOthers 0 oth   l    = []
subOfLenAndOthers 1 oth   l    = [(\(a,b) -> (a,oth++b)) $ sep n l | (_,n) <- zip l [0,1..] ]
	where sep n (splitAt n -> (start, center:end)) = ([center], start++end)
subOfLenAndOthers _ oth   []   = []
subOfLenAndOthers n oth (x:xs) = 
	(map (\(a,b) -> (x:a,b)) $ subOfLenAndOthers (n-1) oth xs) 
	++ subOfLenAndOthers n (x:oth) xs

getVars (Pattern vars _ ) = vars
getFunc (Pattern _ func) = func


dropPos n (splitAt n -> (a, _:b)) =  a ++ b
dropPos _ l = l

collectStringCopies :: [String] -> [(String, [Int])]
collectStringCopies vars = map (\var -> (var, List.elemIndices var vars )) $ List.nub vars

