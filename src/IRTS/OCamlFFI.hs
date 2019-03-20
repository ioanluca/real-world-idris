module IRTS.OCamlFFI where

import           Malfunction.AST
import           IRTS.Lang
import           Data.List.Split                ( splitOn )

makeForeignCall :: FDesc -> FDesc -> [FDesc] -> [MlfExp] -> MlfExp
makeForeignCall ret (FStr fn) fds args = if null args
  then stdLib splits
  else stdLibCall splits args
  where splits = splitOn "." fn
