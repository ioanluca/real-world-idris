module Malfunction.Codegen
  ( generateMlfProgram
  )
where

import           Idris.Core.TT
import           IRTS.CodegenCommon
import           IRTS.CodegenUtils
import           IRTS.Lang
import           Malfunction.AST
import           Malfunction.TranslateMonad

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
 where
  m = getNameTagArityMap $ map snd decls

  generateBindings :: [Graph.SCC LDecl] -> [MlfBinding]
  generateBindings [] = []
  generateBindings (Graph.AcyclicSCC d : ds) =
    case runTranslate (cgDecl d) m of
      Right (Just b) -> b : generateBindings ds
      Right Nothing  -> generateBindings ds
      Left  err      -> error err
  generateBindings (Graph.CyclicSCC ds : dss) =
    let go d = case runTranslate (cgDecl d) m of
          Right mb  -> mb
          Left  err -> error err
    in  RecBinding (mapMaybe go ds) : generateBindings dss

cgDecl :: LDecl -> Translate (Maybe MlfBinding)
cgDecl (LFun inline n as b) = do
  body <- cgExp b
  let args   = map showCG as
  let name   = showCG n
  let isMain = showCG n == "Main.main"
  if isMain && null as
    then pure $ Just $ RegBinding name body
    else pure $ Just $ RegBinding name $ MlfLam args body
cgDecl _ = pure Nothing

cgName :: Name -> MlfExp
cgName n = MlfVar $ go $ showCG n
 where
  okChar c =
    (isAscii c && isAlpha c) || isDigit c || c `elem` ".&|$+-!@#^*~?<>=_"
  go [] = []
  go (c : cs) | okChar c  = c : go cs
              | otherwise = "%" ++ show (ord c) ++ "%" ++ go cs

cgExp :: LExp -> Translate MlfExp
cgExp e = undefined



