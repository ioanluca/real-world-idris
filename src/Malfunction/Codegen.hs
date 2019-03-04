module Malfunction.Codegen
  ( generateMlfProgram
  )
where

import           Idris.Core.TT
import           IRTS.CodegenCommon
import           IRTS.Lang
import           Malfunction.AST

import           Data.List
import           Data.Char
import           Data.Ord
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Graph                    as Graph
import           Data.Maybe                     ( mapMaybe )
import           Data.Function                  ( on )
import           Control.Exception
import           Control.Monad                  ( mapM )

import           System.Process
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe               ( unsafePerformIO )


generateMlfProgram :: [(Name, LDecl)] -> MlfExp
generateMlfProgram decls = undefined

