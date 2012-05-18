Pattern Power!!
================

Design powerful patterns for matching and data extraction. Then splice them into your program with Template Haskell.

Example Usage
-------------

We build patterns with the constructors for PrePat. The following are examples:

```haskell
Wild
Free "a"
ListPat [Commutative] [Free "a", Const 1, Free "b", Wild]
```

You can easily test the properties of a pattern using the `(~$)` operator. For example:

```haskell
*Data.Pattern> Wild ~$ "test"
Just []
*Data.Pattern> Free "a" ~$ "test"
Just [("a","test")]
*Data.Pattern> Const 1 ~$ 2
Nothing
*Data.Pattern> Const 1 ~$ 1
Just []
*Data.Pattern> let a = ListPat [Commutative] [Free "a", Const 1, Free "b", Wild]
*Data.Pattern> a ~$ [1,2,3,4]
Just [("a",2),("b",3)]
*Data.Pattern> a ~$ [2,2,3,4]
Nothing
*Data.Pattern> let b = ListPat [] [Free "a", Const 1, Free "b", Wild]
*Data.Pattern> b ~$ [1,1,2,3]
Just [("a",1),("b",2)]
*Data.Pattern> b ~$ [1,4,2,3]
Nothing
```

Then you can inject your pattern as a real Haskell Pattern with `$(finishPat ...)`, for example `$(finishPat Wild)`. This requires TemplateHaskell and ViewPatterns.
