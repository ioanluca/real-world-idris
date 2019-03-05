{-# LANGUAGE LambdaCase #-}

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
-- cgName = T.pack . go . showCG
cgName = T.pack . showCG
 where
  okChar c =
    (isAscii c && isAlpha c) || isDigit c || c `elem` ".&|$+-!@#^*~?<>=_"
  go [] = []
  go (c : cs) | okChar c  = c : go cs
              | otherwise = "%" ++ show (ord c) ++ "%" ++ go cs

cgExp :: LExp -> Translate MlfExp
cgExp (LV name                ) = pure $ MlfVar $ cgName name
cgExp (LApp isTailCall fn []  ) = cgExp fn
cgExp (LApp isTailCall fn args) = MlfApp <$> cgExp fn <*> mapM cgExp args
cgExp (LLazyApp name args     ) = do
  args <- mapM cgExp args
  let n    = cgName name
  let lapp = MlfApp (MlfVar n) args
  pure $ MlfLazy lapp
cgExp (LLazyExp e        ) = crashWith "LLazyExp!" --FIXME lifted
cgExp (LForce   e        ) = MlfForce <$> cgExp e
cgExp (LLet name exp body) = do
  e <- cgExp exp
  b <- cgExp body
  pure $ MlfLet [RegBinding (cgName name) e] b
cgExp (LLam  args body) = crashWith "LLam???" -- FIXME lifted
cgExp (LProj e    idx ) = do
  a <- cgExp e
  pure $ MlfProjection idx a
cgExp (LCon maybeName tag name []) =
  if tag > 198 then crashWith "tag > 198" else pure $ MlfLiteral $ MlfInt tag
cgExp (LCon maybeName tag name args) = if tag > 198
  then crashWith "tag > 198"
  else MlfBlock <$> pure tag <*> mapM cgExp args
cgExp (LCase _ e cases                   ) = cgSwitch e cases
cgExp (LConst k                          ) = cgConst k
cgExp (LForeign (FCon ret) (FStr fn) args) = unsafePerformIO $ do
  print fn
  print ret
  print args
  pure $ do
    as <- mapM (cgExp . snd) args
    if null as
      then pure $ pervasiveCall $ T.pack fn
      else pure $ MlfApp (pervasiveCall $ T.pack fn) as

cgExp (LOp prim args) = cgOp prim args
cgExp LNothing        = pure MlfNothing
cgExp (LError err)    = pure $ failWith err

cgSwitch :: LExp -> [LAlt] -> Translate MlfExp
cgSwitch x (LConstCase (BI n) y : cases) = do
  e    <- cgExp x
  exp  <- cgExp y
  rest <- cgSwitch x cases
  let sw = MlfOp MlfEq BigIntArith [MlfLiteral $ MlfBigInt n, e]
  pure $ MlfSwitch sw [([IntSel 1], exp), defaultCase rest]
cgSwitch e cases = do
  a   <- cgExp e
  ts  <- taggroups
  tgs <- mapM cgTagGroup ts
  -- ntgs <- concatMapM cgNonTagCase cases
  let ntgs = []
  pure $ MlfLet [RegBinding matchOn a] (MlfSwitch matchOnExp $ tgs ++ ntgs)
 where
  matchOn    = T.pack "MATCH_ME"
  matchOnExp = MlfVar matchOn

  getTag n m = case Map.lookup n m of
    Just (tag, arity) -> tag
    Nothing           -> error "This should never happen"

  tagcases :: Translate [(Int, ([Name], LExp), Bool)]
  tagcases = do
    m <- ask
    pure $ mapMaybe
      (\case
        (LConCase _ n [] b) -> Just (getTag n m, ([], b), False)
        (LConCase _ n fs b) -> Just (getTag n m, (fs, b), True)
        _                   -> Nothing
      )
      cases

  taggroups :: Translate [(Int, [([Name], LExp)], Bool)]
  taggroups =
    map
        (\cs@((tag, c, isBlock) : _) ->
          (tag, map (\(_, snd, _) -> snd) cs, isBlock)
        )
      .   groupBy ((==) `on` (\(fst, _, _) -> fst))
      .   sortOn (\(fst, _, _) -> fst)
      <$> tagcases

  cgTagGroup :: (Int, [([Name], LExp)], Bool) -> Translate MlfCase
  cgTagGroup (tagmod, cases, isBlock) = do
    tgs <- mapM cgProjections cases
    pure $ ([(if isBlock then Tag else IntSel) tagmod], head tgs) --fixme

  cgProjections :: ([Name], LExp) -> Translate MlfExp
  cgProjections (fields, body) = do
    let fieldBinds = zipWith
          (\i n -> RegBinding (cgName n) (MlfProjection i matchOnExp))
          [0 ..]
          fields
    exp <- cgExp body
    if null fieldBinds then pure exp else pure $ MlfLet fieldBinds exp

  cgNonTagCase :: LAlt -> Translate (Maybe MlfCase)
  cgNonTagCase LConCase{}           = pure Nothing
  cgNonTagCase (LConstCase (I n) e) = do
    a <- cgExp e
    pure $ Just ([IntSel n], a)
  cgNonTagCase (LConstCase (Ch c) e) = do
    a <- cgExp e
    pure $ Just ([IntSel (ord c)], a)
  cgNonTagCase (LConstCase k e) =
    crashWith $ "unsupported constant selector: " ++ show k
  cgNonTagCase (LDefaultCase e) = do
    a <- cgExp e
    pure $ Just $ defaultCase a

cgConst :: Const -> Translate MlfExp
cgConst c = undefined

cgOp :: PrimFn -> [LExp] -> Translate MlfExp
cgOp fn args = undefined



