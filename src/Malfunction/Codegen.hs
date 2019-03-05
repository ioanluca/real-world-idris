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
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
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
  let args   = map cgName as
  let name   = cgName n
  let isMain = name == T.pack "Main.main"
  if isMain && null as
    then pure $ Just $ RegBinding name body
    else pure $ Just $ RegBinding name $ MlfLam args body
cgDecl _ = pure Nothing

cgName :: Name -> MlfName
cgName = T.pack . showCG
-- cgName = T.pack . go . showCG
 where
  okChar c =
    (isAscii c && isAlpha c) || isDigit c || c `elem` ".&|$+-!@#^*~?<>=_"
  go [] = []
  go (c : cs) | okChar c  = c : go cs
              | otherwise = "%" ++ show (ord c) ++ "%" ++ go cs

cgExp :: LExp -> Translate MlfExp
cgExp (LV n             ) = pure $ MlfVar $ cgName n
cgExp (LApp e1 e2 e3    ) = undefined
cgExp (LLazyApp e1 e2   ) = undefined
cgExp (LLazyExp e       ) = undefined
cgExp (LForce   e       ) = undefined
cgExp (LLet e1 e2 e3    ) = undefined
cgExp (LLam  e1 e2      ) = undefined
cgExp (LProj e1 e2      ) = undefined
cgExp (LCon e1 e2 e3 e4 ) = undefined
cgExp (LCase e1 e2 e3   ) = undefined
cgExp (LConst e         ) = undefined
cgExp (LForeign e1 e2 e3) = undefined
cgExp (LOp e1 e2        ) = undefined
cgExp LNothing            = undefined
cgExp (LError e)          = undefined



