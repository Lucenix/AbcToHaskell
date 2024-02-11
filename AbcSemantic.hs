module Main where
import Language.ANTLR4
import AbcG4
import AbcParser
import Abc
import qualified Text.ANTLR.Set as S

getAST (ResultAccept ast) = ast
getAST _ = error "non-AST in ResultSet"

main =
  case glrParse false "" of
    (ResultAccept ast) -> print $ ast2abc ast
    (ResultSet xs)     -> mapM_ (print . ast2abc . getAST) (S.toList xs)

false _ = False
