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
import           Data.Maybe                     ( mapMaybe
                                                , catMaybes
                                                )
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
generateMlfProgram decls =
  let bindings = generateBindings $ asConnectedComponents . map snd $ decls
      runMainApplication = MlfApp (MlfVar $ cgName $ sMN 0 "runMain") []
  in  MlfProg (reverseStringMlf : bindings) runMainApplication
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
cgName = T.pack . go . showCG
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
    if null as then pure $ pervasive fn else pure $ pervasiveCall fn as

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
  a    <- cgExp e
  ts   <- taggroups
  tgs  <- mapM cgTagGroup ts
  ntgs <- mapM cgNonTagCase cases
  pure $ MlfLet [RegBinding matchOn a]
                (MlfSwitch matchOnExp $ tgs ++ catMaybes ntgs)
 where
  matchOn    = T.pack "%MATCH_ME"
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
    pure ([(if isBlock then Tag else IntSel) tagmod], head tgs) --fixme

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
cgConst (I   n) = pure $ MlfLiteral $ MlfInt n
cgConst (BI  n) = pure $ MlfLiteral $ MlfBigInt n
cgConst (Fl  f) = pure $ MlfLiteral $ MlfFloat f
cgConst (Ch  c) = pure $ MlfLiteral $ MlfInt (ord c)
cgConst (Str c) = pure $ MlfLiteral $ MlfString $ T.pack c
cgConst c       = crashWith $ "unimplemented constant " ++ show c

-- cgArithType :: ArithTy -> Translate MlfArithType
-- cgArithType (ATInt (ITFixed IT8)) = pure IntArith
-- cgArithType (ATInt (ITFixed IT16)) = pure IntArith
-- cgArithType (ATInt (ITFixed IT32)) = pure Int32Arith
-- cgArithType (ATInt (ITFixed IT64)) = pure Int64Arith
-- cgArithType (ATInt ITNative) = pure IntArith
-- cgArithType (ATInt ITBig) = pure BigIntArith
-- cgArithType (ATInt ITChar) = pure IntArith
-- cgArithType ATFloat = pure FloatArith

cgArithType :: ArithTy -> MlfArithType
cgArithType (ATInt it) = cgIntType it
cgArithType ATFloat    = FloatArith

cgIntType :: IntTy -> MlfArithType
cgIntType (ITFixed IT8 ) = IntArith
cgIntType (ITFixed IT16) = IntArith
cgIntType (ITFixed IT32) = Int32Arith
cgIntType (ITFixed IT64) = Int64Arith
cgIntType ITNative       = IntArith
cgIntType ITBig          = BigIntArith
cgIntType ITChar         = IntArith

-- careful with what is being parsed in args
-- is a source of bugs, some things receive the world as a param
-- mostly the ones that do IO prob
cgOp :: PrimFn -> [LExp] -> Translate MlfExp
cgOp (LPlus  at    ) args = MlfOp MlfPlus (cgArithType at) <$> mapM cgExp args
cgOp (LMinus at    ) args = MlfOp MlfMinus (cgArithType at) <$> mapM cgExp args
cgOp (LTimes at    ) args = MlfOp MlfTimes (cgArithType at) <$> mapM cgExp args
-- cgOp (LUDiv  at     )   args = undefined
cgOp (LSDiv  at    ) args = MlfOp MlfDiv (cgArithType at) <$> mapM cgExp args
-- cgOp (LURem  at     )   args = undefined
cgOp (LSRem  at    ) args = MlfOp MlfMod (cgArithType at) <$> mapM cgExp args
cgOp (LAnd   at    ) args = MlfOp MlfBitAnd (cgIntType at) <$> mapM cgExp args
cgOp (LOr    at    ) args = MlfOp MlfBitOr (cgIntType at) <$> mapM cgExp args
cgOp (LXOr   at    ) args = MlfOp MlfBitXOr (cgIntType at) <$> mapM cgExp args
cgOp (LCompl at    ) args = MlfOp MlfBitAnd (cgIntType at) <$> mapM cgExp args
cgOp (LSHL   at    ) args = MlfOp MlfBitAnd (cgIntType at) <$> mapM cgExp args
cgOp (LLSHR at) args = MlfOp MlfBitRShift (cgIntType at) <$> mapM cgExp args
cgOp (LASHR at) args = MlfOp MlfBitRShiftExt (cgIntType at) <$> mapM cgExp args
cgOp (LEq    at    ) args = MlfOp MlfEq (cgArithType at) <$> mapM cgExp args
-- cgOp (LLt    at     )   args = undefined
-- cgOp (LLe    at     )   args = undefined
-- cgOp (LGt    at     )   args = undefined
-- cgOp (LGe    at     )   args = undefined
cgOp (LSLt   at    ) args = MlfOp MlfLT (cgArithType at) <$> mapM cgExp args
cgOp (LSLe   at    ) args = MlfOp MlfLTEq (cgArithType at) <$> mapM cgExp args
cgOp (LSGt   at    ) args = MlfOp MlfGT (cgArithType at) <$> mapM cgExp args
cgOp (LSGe   at    ) args = MlfOp MlfGTEq (cgArithType at) <$> mapM cgExp args
cgOp (LSExt at1 at2) [e]  = cgExp e --fixme use ocaml figure direction
-- cgOp (LZExt  at1 at2)   args = undefined
-- cgOp (LTrunc at1 at2)   args = undefined
cgOp LStrConcat      args = pervasiveCall "^" <$> mapM cgExp args
-- cgOp LStrLt             args = undefined
cgOp LStrEq          args = stdLibCall "String" "equal" <$> mapM cgExp args
cgOp LStrLen         [e]  = MlfVecLen Byte <$> cgExp e
cgOp (LIntFloat at)  [e]  = MlfConvert (cgIntType at) FloatArith <$> cgExp e
cgOp (LFloatInt at)  [e]  = MlfConvert FloatArith (cgIntType at) <$> cgExp e
cgOp (LIntStr   at)  args = pervasiveCall "string_of_int" <$> mapM cgExp args
cgOp (LStrInt   at)  args = pervasiveCall "int_of_string" <$> mapM cgExp args
cgOp LFloatStr       args = pervasiveCall "string_of_float" <$> mapM cgExp args
cgOp LStrFloat       args = pervasiveCall "float" <$> mapM cgExp args
cgOp (LChInt at)     [e]  = cgExp e
cgOp (LIntCh at)     [e]  = cgExp e
-- cgOp (LBitCast at1 at2) args = undefined
cgOp LFExp           args = pervasiveCall "exp" <$> mapM cgExp args
cgOp LFLog           args = pervasiveCall "log" <$> mapM cgExp args
cgOp LFSin           args = pervasiveCall "sin" <$> mapM cgExp args
cgOp LFCos           args = pervasiveCall "cos" <$> mapM cgExp args
cgOp LFTan           args = pervasiveCall "tan" <$> mapM cgExp args
cgOp LFASin          args = pervasiveCall "asin" <$> mapM cgExp args
cgOp LFACos          args = pervasiveCall "acos" <$> mapM cgExp args
cgOp LFATan          args = pervasiveCall "atan" <$> mapM cgExp args
cgOp LFATan2         args = pervasiveCall "atan2" <$> mapM cgExp args
cgOp LFSqrt          args = pervasiveCall "sqrt" <$> mapM cgExp args
cgOp LFFloor         args = pervasiveCall "floor" <$> mapM cgExp args
cgOp LFCeil          args = pervasiveCall "ceil" <$> mapM cgExp args
cgOp LFNegate        args = MlfOp MlfNeg IntArith <$> mapM cgExp args --fixme
cgOp LStrHead        [e]  = do
  let fstIdx = MlfLiteral $ MlfInt 0
  s <- cgExp e
  pure $ MlfVecLoad Byte s fstIdx
cgOp LStrTail [s] = do
  str <- cgExp s
  len <- cgOp LStrLen [s]
  pure $ stdLibCall
    "String"
    "sub"
    [ str
    , MlfLiteral $ MlfInt 1
    , MlfOp MlfMinus IntArith [len, MlfLiteral $ MlfInt 1]
    ]
cgOp LStrCons [c, s] = do
  ch  <- cgExp c
  str <- cgExp s
  pure $ pervasiveCall
    "^"
    [stdLibCall "String" "make" [MlfLiteral $ MlfInt 1, ch], str]
    --safety???? fixme
    -- todo maybe use makevec builtin
cgOp LStrIndex  [s, idx] = MlfVecLoad Byte <$> cgExp s <*> cgExp idx
cgOp LStrRev    args     = MlfApp (MlfVar reverseName) <$> mapM cgExp args
cgOp LStrSubstr args     = stdLibCall "String" "sub" <$> mapM cgExp args --todo test
cgOp LReadStr   args     = pervasiveCall "read_line" <$> mapM cgExp args
cgOp LWriteStr (world : args) =
  pervasiveCall "print_string" <$> mapM cgExp args
-- cgOp LSystemInfo        args = undefined
-- cgOp LFork              args = undefined
-- cgOp LPar               args = undefined
-- cgOp (LExternal at)     args = undefined
-- cgOp LCrash             args = undefined
-- cgOp LNoOp              args = undefined
cgOp p _ = pure $ failWith $ "unimplemented primitive: " ++ show p

reverseName :: MlfName
reverseName = T.pack "%STRREV"

reverseStringMlf :: MlfBinding
reverseStringMlf = RegBinding reverseName $ MlfLam [T.pack "s"] $ MlfLet
  [ RegBinding
      (T.pack "n")
      (MlfOp MlfMinus
             IntArith
             [MlfVecLen Byte (MlfVar $ T.pack "s"), MlfLiteral $ MlfInt 1]
      )
  ]
  (stdLibCall
    "String"
    "mapi"
    [ MlfLam
      [T.pack "i", T.pack "c"]
      (MlfVecLoad
        Byte
        (MlfVar $ T.pack "s")
        (MlfOp MlfMinus IntArith [MlfVar $ T.pack "n", MlfVar $ T.pack "i"])
      )
    , MlfVar $ T.pack "s"
    ]
  )




