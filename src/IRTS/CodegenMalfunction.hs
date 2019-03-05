module IRTS.CodegenMalfunction
  ( codegenMalfunction
  )
where

import           Idris.Core.TT
import           IRTS.CodegenCommon
import           IRTS.Lang
import           IRTS.CodegenUtils
import           Malfunction.TranslateMonad

import           Data.List
import           Data.Char
import           Data.Ord
import qualified Data.Map.Strict               as Map
import qualified Data.Graph                    as Graph
import           Data.Maybe                     ( mapMaybe )
import           Data.Function                  ( on )
import           Control.Exception
import           Control.Monad                  ( mapM
                                                , ap
                                                )

import           System.Process
import           System.Directory
import           System.FilePath
import           System.IO.Unsafe               ( unsafePerformIO )

data Sexp = S [Sexp] | A String | KInt Int
            | KBigInt Integer | KStr String
             deriving (Eq)

instance Show Sexp where
  show sexp = render sexp ""   where
    render :: Sexp -> String -> String
    render (S       s) k = "(" ++ foldr render (") " ++ k) s
    render (A       s) k = s ++ " " ++ k
    render (KInt    n) k = show n ++ " " ++ k
    render (KStr    s) k = show s ++ " " ++ k
    render (KBigInt s) k = show s ++ ".ibig " ++ k

codegenMalfunction :: CodeGenerator
codegenMalfunction ci = do
  writeFile langFile $ stringify langDeclarations
  writeFile tmp $ show $ if interfaces ci then evalExp else compileExp
  callCommand fmtCommand
  catch (callCommand $ if interfaces ci then evalCommand else compileCommand)
        handler
  removeFile tmp
 where
  langDeclarations = liftDecls ci

  outFile          = outputFile ci
  mlfFile          = replaceExtensionIf outFile ".o" ".x.mlf"
  langFile         = replaceExtensionIf outFile ".o" ".lang"
  tmp              = "idris_malfunction_output.mlf"

  fmtCommand       = "malfunction fmt " ++ tmp ++ " > " ++ mlfFile
  evalCommand      = "cat " ++ mlfFile ++ " | malfunction eval"
  compileCommand   = "malfunction compile -o " ++ outFile ++ " " ++ mlfFile

  runMain          = mlfApp (cgName (sMN 0 "runMain")) [KStr "RUNMAIN_EATME"]

  compileExp =
    S
      ( A "module"
      : shuffle langDeclarations [S [A "_", runMain], S [A "export"]]
      )

  evalExp = S (A "let" : shuffle langDeclarations [runMain])

  handler :: SomeException -> IO ()
  handler ex = putStrLn $ "Caught exception: " ++ show ex

  stringify :: [(Name, LDecl)] -> String
  stringify = unwords . map (\decl -> show decl ++ "\n\n")

replaceExtensionIf :: FilePath -> String -> String -> FilePath
replaceExtensionIf file curr new = case stripExtension curr file of
  Just fileNoExt -> fileNoExt <.> new
  _              -> file <.> new



shuffle :: [(Name, LDecl)] -> [Sexp] -> [Sexp]
shuffle decls rest = prelude
  : toBindings (asConnectedComponents . map snd $ decls)
 where
  m = getNameTagArityMap $ map snd decls
  toBindings :: [Graph.SCC LDecl] -> [Sexp]
  toBindings [] = rest
  toBindings (Graph.AcyclicSCC decl : comps) =
    case runTranslate (cgDecl decl) m of
      Right (Just sexp) -> sexp : toBindings comps
      Right Nothing     -> toBindings comps
      Left  err         -> error err
  toBindings (Graph.CyclicSCC dcls : comps) = S (A "rec" : mapMaybe go dcls)
    : toBindings comps
   where
    go decl = case runTranslate (cgDecl decl) m of
      Right maybeSexp -> maybeSexp
      Left  err       -> error err



  prelude :: Sexp
  prelude = S
    [ A "$%strrev"
    , S
      [ A "lambda"
      , S [A "$s"]
      , S
        [ A "let"
        , S [A "$n", S [A "-", S [A "length.byte", A "$s"], KInt 1]]
        , S
          [ A "apply"
          , S [A "global", A "$String", A "$mapi"]
          , S
            [ A "lambda"
            , S [A "$i", A "$c"]
            , S [A "load.byte", A "$s", S [A "-", A "$n", A "$i"]]
            ]
          , A "$s"
          ]
        ]
      ]
    ]




okChar :: Char -> Bool
okChar c =
  (isAscii c && isAlpha c) || isDigit c || c `elem` ".&|$+-!@#^*~?<>=_"



cgSym :: String -> Sexp
cgSym s = A ('$' : chars s)
 where
  chars :: String -> String
  chars [] = []
  chars (c : cs) | okChar c  = c : chars cs
                 | otherwise = "%" ++ show (ord c) ++ "%" ++ chars cs



cgName :: Name -> Sexp
cgName = cgSym . showCG



cgDecl :: LDecl -> Translate (Maybe Sexp)
cgDecl (LFun _ name args body) = do
  b <- cgExp body
  if showCG name == "Main.main" && null args
    then pure $ Just $ S [cgName name, b]
    else pure $ Just $ S [cgName name, mlfLam (map cgName args) b]
cgDecl _ = pure Nothing



cgExp :: LExp -> Translate Sexp
cgExp (LV name                ) = pure $ cgName name
cgExp (LApp isTailCall fn []  ) = cgExp fn
cgExp (LApp isTailCall fn args) = mlfApp <$> cgExp fn <*> mapM cgExp args
cgExp (LLazyApp name args     ) = do
  args <- mapM cgExp args
  let n    = cgName name
  let lapp = mlfApp n args
  pure $ S [A "lazy", lapp]
cgExp (LLazyExp e        ) = crashWith "LLazyExp!" --FIXME lifted
cgExp (LForce   e        ) = cgExp e >>= (\e -> pure $ S [A "force", e])
cgExp (LLet name exp body) = do
  e <- cgExp exp
  b <- cgExp body
  pure $ S [A "let", S [cgName name, e], b]
cgExp (LLam  args body) = crashWith "LLam???" -- FIXME lifted
cgExp (LProj e    idx ) = do
  a <- cgExp e
  pure $ S [A "field", KInt idx, a]
cgExp (LCon maybeName tag name []) =
  if tag > 198 then crashWith "tag > 198" else pure $ KInt tag
cgExp (LCon maybeName tag name args) =
  if tag > 198 then crashWith "tag > 198" else mlfBlock tag <$> mapM cgExp args
cgExp (LCase _ e cases                   ) = cgSwitch e cases
cgExp (LConst k                          ) = cgConst k
cgExp (LForeign (FCon ret) (FStr fn) args) = unsafePerformIO $ do
  print fn
  print ret
  print args
  pure $ do
    let fname = '$' : fn
    as <- mapM (cgExp . snd) args
    if as == []
      then pure $ S [A "global", A "$Pervasives", A fname]
      else
        pure $ S $ [A "apply", S [A "global", A "$Pervasives", A fname]] ++ as

cgExp (LOp prim args) = cgOp prim args
cgExp LNothing        = pure $ KStr "NOTHING"
cgExp (LError s)      = pure $ S
  [ A "apply"
  , S [A "global", A "$Pervasives", A "$failwith"]
  , KStr $ "error: " ++ show s
  ]



concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f []       = pure []
concatMapM f (a : as) = do
  bs <- f a
  rs <- concatMapM f as
  pure $ bs ++ rs



cgSwitch :: LExp -> [LAlt] -> Translate Sexp
cgSwitch x (LConstCase (BI n) y : cases) = do
  e    <- cgExp x
  exp  <- cgExp y
  rest <- cgSwitch x cases
  let sw = S [A "==.ibig", e, KBigInt n]
  pure $ mlfSwitch sw [mlfSel [mlfIntCase 1] exp, mlfSel mlfDefaultCase rest]
cgSwitch e cases = do
  a    <- cgExp e
  ts   <- taggroups
  tgs  <- mapM cgTagGroup ts
  ntgs <- concatMapM cgNonTagCase cases
  pure $ mlfLet [mlfBind scr a] $ mlfSwitch scr $ tgs ++ ntgs
 where
  scr :: Sexp
  scr = A "$%matchOn"

  getTag n m = case Map.lookup n m of
    Just (tag, arity) -> tag
    Nothing           -> error "This should never happen"

  tagcases :: Translate [(Int, LAlt, Bool)]
  tagcases = do
    m <- ask
    pure $ mapMaybe
      (\c -> case c of
        (LConCase _ n [] _) -> Just (getTag n m, c, False)
        (LConCase _ n _  _) -> Just (getTag n m, c, True)
        _                   -> Nothing
      )
      cases

  taggroups :: Translate [(Int, [LAlt], Bool)]
  taggroups =
    map
        (\cases@((tag, c, isBlock) : _) ->
          (tag, map (\(_, snd, _) -> snd) cases, isBlock)
        )
      .   groupBy ((==) `on` (\ fst, _, _) -> fst)
      .   sortOn (\ (fst, _, _) -> fst )
      <$> tagcases

  cgTagGroup :: (Int, [LAlt], Bool) -> Translate Sexp
  cgTagGroup (tagmod, cases, isBlock) = do
    tgs <- cgTagClass cases
    if isBlock
      then pure $ S $ S [A "tag", KInt tagmod] : tgs
      else pure $ S $ KInt tagmod : tgs

  cgTagClass :: [LAlt] -> Translate [Sexp]
  cgTagClass cases = do
    let fcs = [ c | c@(LConCase tag n _ _) <- cases ]
    mapM cgProjections fcs

  cgProjections :: LAlt -> Translate Sexp
  cgProjections (LConCase tag name args body) = do
    let fields =
          zipWith (\i n -> S [cgName n, S [A "field", KInt i, scr]]) [0 ..] args
    exp <- cgExp body
    if null fields then pure exp else pure $ S $ [A "let"] ++ fields ++ [exp]

  cgNonTagCase :: LAlt -> Translate [Sexp]
  cgNonTagCase LConCase{}           = mapM pure []
  cgNonTagCase (LConstCase (I n) e) = do
    a <- cgExp e
    pure [S [KInt n, a]]
  -- cgNonTagCase (LConstCase (BI n) e) = do
  --   a <- cgExp e
  --   pure [S [KInt (fromInteger n), a]]
  cgNonTagCase (LConstCase (BI n) e) = crashWith "Const bigInteger case"
  cgNonTagCase (LConstCase (Ch c) e) = do
    a <- cgExp e
    pure [S [KInt (ord c), a]]
  cgNonTagCase (LConstCase k e) =
    crashWith $ "unsupported constant selector: " ++ show k
  cgNonTagCase (LDefaultCase e) = do
    a <- cgExp e
    pure [S [A "_", S [A "tag", A "_"], a]]



arithSuffix :: ArithTy -> String
arithSuffix (ATInt ITNative) = ""
arithSuffix (ATInt ITChar  ) = ""
arithSuffix (ATInt ITBig   ) = ".ibig"
arithSuffix s                = error $ "unsupported arithmetic type: " ++ show s



stdlib :: [String] -> [LExp] -> Translate Sexp
stdlib path args =
  mlfApp (S (A "global" : map (A . ('$' :)) path)) <$> mapM cgExp args



pervasive :: String -> [LExp] -> Translate Sexp
pervasive f = stdlib ["Pervasives", f]



cgOp :: PrimFn -> [LExp] -> Translate Sexp
cgOp LStrConcat [l, r] = pervasive "^" [l, r]
cgOp LStrCons   [c, r] = do
  cc <- cgExp c
  rr <- cgExp r
  pure $ S
    [ A "apply"
    , S [A "global", A "$Pervasives", A "$^"]
    , S [A "apply", S [A "global", A "$String", A "$make"], KInt 1, cc]
    , rr
    ] -- fixme safety
cgOp LWriteStr [world, str] = pervasive "print_string" [str]
cgOp LReadStr  [world]      = pervasive "read_line" []
cgOp (LPlus t) args         = do
  as <- mapM cgExp args
  pure $ S $ A ("+" ++ arithSuffix t) : as
cgOp (LMinus t) args = do
  as <- mapM cgExp args
  pure $ S $ A ("-" ++ arithSuffix t) : as
cgOp (LTimes t) args = do
  as <- mapM cgExp args
  pure $ S $ A ("*" ++ arithSuffix t) : as
cgOp (LSRem t) args = do
  as <- mapM cgExp args
  pure $ S $ A ("%" ++ arithSuffix t) : as
cgOp (LEq t) args = do
  as <- mapM cgExp args
  pure $ S $ A ("==" ++ arithSuffix t) : as
cgOp (LSLt t) args = do
  as <- mapM cgExp args
  pure $ S $ A ("<" ++ arithSuffix t) : as
cgOp (LSGt t) args = do
  as <- mapM cgExp args
  pure $ S $ A (">" ++ arithSuffix t) : as
cgOp (LSLe t) args = do
  as <- mapM cgExp args
  pure $ S $ A ("<=" ++ arithSuffix t) : as
cgOp (LSGe t) args = do
  as <- mapM cgExp args
  pure $ S $ A (">=" ++ arithSuffix t) : as
cgOp (LIntStr ITNative) args = pervasive "string_of_int" args
cgOp (LIntStr ITBig   ) args = stdlib ["Z", "to_string"] args
cgOp (LChInt  _       ) [x]  = cgExp x
cgOp (LIntCh  _       ) [x]  = cgExp x
cgOp (LSExt  _ _      ) [x]  = cgExp x -- FIXME use ocaml, figure direction
cgOp (LTrunc _ _      ) [x]  = cgExp x -- FIXME
cgOp (LStrInt ITNative) [x]  = pervasive "int_of_string" [x]
cgOp LStrEq             args = stdlib ["String", "equal"] args
cgOp LStrLen            [x]  = do
  e <- cgExp x
  pure $ S [A "length.byte", e]
cgOp LStrHead [x] = do
  e <- cgExp x
  pure $ S [A "load.byte", e, KInt 0]
cgOp LStrIndex args = do
  as <- mapM cgExp args
  pure $ S $ A "store.byte" : as
cgOp LStrTail [x] = do
  e <- cgExp x
  o <- cgOp LStrLen [x]
  pure $ S
    [ A "apply"
    , S [A "global", A "$String", A "$sub"]
    , e
    , KInt 1
    , S [A "-", o, KInt 1]
    ]
cgOp LStrRev [s] = do
  e <- cgExp s
  pure $ mlfApp (A "$%strrev") [e]
cgOp p _ = pure $ S
  [ A "apply"
  , S [A "global", A "$Pervasives", A "$failwith"]
  , KStr $ "unimplemented: " ++ show p
  ]



cgConst :: Const -> Translate Sexp
cgConst (I   n) = pure $ KInt n
cgConst (BI  n) = pure $ KBigInt n
cgConst (Fl  x) = crashWith "no floats"
cgConst (Ch  i) = pure $ KInt (ord i)
cgConst (Str s) = pure $ KStr s
cgConst k       = crashWith $ "unimplemented constant " ++ show k



mlfApp :: Sexp -> [Sexp] -> Sexp
mlfApp fn args = S $ A "apply" : fn : singletonIfEmpty args (KStr "APP_EATME")

mlfLam :: [Sexp] -> Sexp -> Sexp
mlfLam args body = S [A "lambda", S $ singletonIfEmpty args (A "$EATME"), body]

mlfLet :: [Sexp] -> Sexp -> Sexp
mlfLet bindings body = S $ A "let" : bindings ++ [body]

mlfBind :: Sexp -> Sexp -> Sexp
mlfBind name e = S [name, e]

mlfBlock :: Int -> [Sexp] -> Sexp
mlfBlock tag fs = S $ A "block" : S [A "tag", KInt tag] : fs

mlfField :: Int -> Sexp -> Sexp
mlfField n block = S [A "field", KInt n, block]

mlfSel :: [Sexp] -> Sexp -> Sexp
mlfSel cases e = S $ cases ++ [e]

mlfTagCase :: Int -> Sexp
mlfTagCase tag = S [A "tag", KInt tag]

mlfIntCase :: Int -> Sexp
mlfIntCase = KInt

mlfSwitch :: Sexp -> [Sexp] -> Sexp
mlfSwitch sw sels = S $ A "switch" : sw : sels

mlfIntRangeCase :: Int -> Int -> Sexp
mlfIntRangeCase x y = S [KInt x, KInt y]

mlfDefaultCase :: [Sexp]
mlfDefaultCase = [A "_", S [A "tag", A "_"]]



singletonIfEmpty :: [a] -> a -> [a]
singletonIfEmpty [] a = [a]
singletonIfEmpty as _ = as
