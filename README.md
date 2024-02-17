This project requires the `antlr-haskell` package, for example installed through cabal.
```cabal install antlr-haskell```
To run the Main code, you need to instruct the packages to be available, namely:
```ghci AbcSemantic.hs -package antlr-haskell -package prettify```
then run the `main` function.

Strings of the obtained results can be found in the `results` folder.
Notably, the way the parser is designed tries every single possible combination when the grammar is ambiguous. In this specific case, the last two lines of the example are comments which return empty lists. The difference between `result1` and `result2` exemplifies when those comments are considered the same `abc_tune` or when each of the comments is considered a different `abc_tune`.

The grammar used is not complete, and it does not take in consideration, among others:

 - Repetitions
 - File titles