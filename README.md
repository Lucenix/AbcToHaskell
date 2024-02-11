This project requires the `antlr-haskell` package, for example installed through cabal.
```cabal install antlr-haskell```
To run the Main code, you need to instruct the packages to be available, namely:
```ghci AbcSemantic.hs -package antlr-haskell -package prettify```
