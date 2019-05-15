{-# LANGUAGE OverloadedStrings #-}
module IRTS.OCamlFFI where

import           Malfunction.AST
import           IRTS.Lang
import           Idris.Core.TT                  ( Name(UN) )
import           Data.List.Split                ( splitOn )

makeForeignCall :: FDesc -> FDesc -> [FDesc] -> [MlfExp] -> MlfExp
makeForeignCall ret (FStr fn) []  []   = stdLib (splitOn "." fn)
makeForeignCall ret (FStr fn) fds args = stdLibCall splits
  $ zipWith cgForeignArg fds args
  where splits = splitOn "." fn


cgForeignArg :: FDesc -> MlfExp -> MlfExp
cgForeignArg (FApp (UN "OCaml_FnT") [_, FApp (UN "OCaml_Fn") [_, _, a, r]]) f =
  let x    = cgForeignArg a (MlfVar "x")
      body = MlfApp f [x]
  in  MlfLam ["x"] body
cgForeignArg (FApp (UN "OCaml_FnT") [_, FApp (UN "OCaml_Fn") [_, _, a, FApp (UN "OCaml_FnIO") [_, _, b]]]) f
  = let x    = cgForeignArg a (MlfVar "x")
        body = MlfApp f [x, MlfNothing]
    in  MlfLam ["x"] body
cgForeignArg _ arg = arg
