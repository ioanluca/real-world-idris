module IRTS.OCamlFFI where

import           Malfunction.AST
import           IRTS.Lang
import           Data.List
import           Data.List.Split                ( splitOn )

makeForeignCall :: FDesc -> FDesc -> [FDesc] -> [MlfExp] -> MlfExp
makeForeignCall ret (FStr fn) fds args = if null args
  then stdLib mn f
  else stdLibCall mn f args
 where
  splits = splitOn "." fn
  mn     = intercalate "." $ init splits
  f      = last splits
