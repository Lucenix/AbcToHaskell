{-# LANGUAGE DeriveAnyClass, DeriveGeneric, TypeFamilies, QuasiQuotes
    , DataKinds, ScopedTypeVariables, OverloadedStrings, TypeSynonymInstances
    , FlexibleInstances, UndecidableInstances, TemplateHaskell, FlexibleContexts #-}
module AbcParser where
import Language.ANTLR4
import AbcG4
import Abc

$(g4_parsers abcAST abcGrammar)