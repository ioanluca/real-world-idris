{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE PatternSynonyms #-}

module IRTS.OCamlFFI where

import           Malfunction.AST
import           IRTS.Lang
import           Idris.Core.TT                  ( Name(UN) )
import           Data.List.Split                ( splitOn )

pattern OCaml_FunT f <- FApp (UN "OCaml_FnT") [_, f]
pattern OCaml_Fn t f   <- FApp (UN "OCaml_Fn") [_, _, t, f]
pattern OCaml_FnBase t <- FApp (UN "OCaml_FnBase") [_, t]
pattern OCaml_FnIO t   <- FApp (UN "OCaml_FnIO") [_, _, t]

makeForeignCall :: FDesc -> FDesc -> [FDesc] -> [MlfExp] -> MlfExp
makeForeignCall ret (FStr fn) []  []   = stdLib (splitOn "." fn)
makeForeignCall ret (FStr fn) fds args = stdLibCall splits
  $ zipWith cgForeignArg fds args
  where splits = splitOn "." fn

cgForeignArg :: FDesc -> MlfExp -> MlfExp
cgForeignArg (OCaml_FunT f) arg = go f 0
  where go (OCaml_Fn t fn) n = go fn (n + 1)
        go (OCaml_FnBase t) n = arg
        go (OCaml_FnIO t) n = 
          let as = map textShow [1..n]
              body = MlfApp arg ((map MlfVar as) ++ [MlfNothing])
          in MlfLam as body
        go _ n = arg
cgForeignArg _ arg = arg
