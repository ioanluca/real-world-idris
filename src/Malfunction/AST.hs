{-# LANGUAGE OverloadedStrings #-}

module Malfunction.AST
    ( defaultCase
    )
where

import qualified Data.Text                     as T

-- data MlfProgram
--     = Interpreted [MlfBinding] MlfExp
--     | Compiled [MlfBinding] MlfExp
--     deriving (Eq, Show)

type MlfName = String

data MlfBinding
    = RegBinding MlfName MlfExp
    | ExecBinding MlfExp
    | RecBinding [MlfBinding]
    deriving (Eq, Show)

data MlfArithType
    = Int32Arith
    | Int64Arith
    | IntArith
    | BigIntArith
    | FloatArith
    deriving (Eq, Show)

data MlfVecType
    = Normal
    | Byte
    deriving (Eq, Show)

data MlfSel
    = IntSel Int
    | IntRangeSel Int Int
    | DefaultInt
    | Tag Int
    | DefaultTag
    deriving (Eq, Show)

type MlfCase = ([MlfSel], MlfExp)

defaultCase :: MlfExp -> MlfCase
defaultCase exp = ([DefaultInt, DefaultTag], exp)


data MlfExp
    = MlfProg [MlfBinding] MlfExp
    | MlfVar MlfName

    | MlfInt32 Int --fixme extract to diff data type reduce code duplication
    | MlfInt64 Int --fixme
    | MlfInt Int
    | MlfBigInt Integer
    | MlfFloat Float
    | MlfPosInf
    | MlfNegInf
    | MlfNaN
    | MlfConvert MlfArithType MlfArithType MlfExp

    | MlfString String --they return vec of bytes

    | MlfPlus MlfArithType MlfExp MlfExp
    | MlfMinus MlfArithType MlfExp MlfExp
    | MlfTimes MlfArithType MlfExp MlfExp
    | MlfDiv MlfArithType MlfExp MlfExp
    | MlfMod MlfArithType MlfExp MlfExp
    | MlfNeg MlfArithType MlfExp

    | MlfEq MlfArithType MlfExp MlfExp
    | MlfLT MlfArithType MlfExp MlfExp
    | MlfLTEq MlfArithType MlfExp MlfExp
    | MlfGT MlfArithType MlfExp MlfExp
    | MlfGTEq MlfArithType MlfExp MlfExp

    -- not available for floats
    | MlfBitAnd MlfArithType MlfExp MlfExp
    | MlfBitOr MlfArithType MlfExp MlfExp
    | MlfBitXOr MlfArithType MlfExp MlfExp
    | MlfBitLShift MlfArithType MlfExp MlfExp
    | MlfBitRShift MlfArithType MlfExp MlfExp
    | MlfBitRShiftExt MlfArithType MlfExp MlfExp

    | MlfLam [MlfExp] MlfExp
    | MlfApp MlfExp [MlfExp]
    | MlfLet [MlfBinding] MlfExp
    | MlfSeq [MlfExp]

    | MlfBlock Int [MlfExp]
    | MlfProjection Int MlfExp

    | MlfVec MlfVecType MlfExp MlfExp
    | MlfVecLoad MlfVecType MlfExp MlfExp
    | MlfVecStore MlfVecType MlfExp MlfExp MlfExp
    | MlfVecLen MlfVecType MlfExp

    | MlfLazy MlfExp
    | MlfForce MlfExp

    | MlfSwitch MlfExp [MlfCase]
    | MlfIf MlfExp MlfExp MlfExp

    | MlfOCaml MlfExp MlfExp

    | MlfComment T.Text
    deriving (Eq, Show)

textShow :: Show a => a -> T.Text
textShow = T.pack . show

ip :: T.Text -> T.Text
ip e = T.concat ["(", e, ")"]

arithType2Text :: MlfArithType -> T.Text
arithType2Text Int32Arith  = ".int32"
arithType2Text Int64Arith  = ".int64"
arithType2Text IntArith    = T.empty
arithType2Text BigIntArith = ".ibig"
arithType2Text FloatArith  = ".f64"

vecType2text :: MlfVecType -> T.Text
vecType2text Normal = T.empty
vecType2text Byte   = ".byte"

name2Text :: MlfName -> T.Text
name2Text name = '$' `T.cons` T.pack name

binding2Text :: MlfBinding -> T.Text
binding2Text (RegBinding name e) =
    ip $ T.concat [name2Text name, " ", mlfAST2Text e]
binding2Text (ExecBinding e) = ip $ T.concat ["_", " ", mlfAST2Text e]
binding2Text (RecBinding es) =
    ip $ T.concat ["rec", " "] `T.append` T.concat (map binding2Text es)

sel2Text :: MlfSel -> T.Text
sel2Text (IntSel n       ) = textShow n
sel2Text (IntRangeSel n m) = ip $ T.concat [textShow n, " ", textShow m]
sel2Text DefaultInt        = "_"
sel2Text (Tag tag)         = ip $ T.concat ["tag", " ", textShow tag]
sel2Text DefaultTag        = ip $ T.concat ["tag", " ", "_"]

case2Text :: MlfCase -> T.Text
case2Text (sels, e) =
    ip $ T.concat (map sel2Text sels) `T.append` mlfAST2Text e

mlfAST2Text :: MlfExp -> T.Text
mlfAST2Text (MlfProg binds e) =
    ip
        $  T.concat
        $  [ip "module", " "]
        ++ map binding2Text binds
        ++ [ip "export"]
mlfAST2Text (MlfVar    e)          = name2Text e
mlfAST2Text (MlfInt32 e) = textShow e `T.append` arithType2Text Int32Arith
mlfAST2Text (MlfInt64 e) = textShow e `T.append` arithType2Text Int64Arith
mlfAST2Text (MlfInt    e)          = textShow e
mlfAST2Text (MlfBigInt e) = textShow e `T.append` arithType2Text BigIntArith
mlfAST2Text (MlfFloat  e)          = textShow e
mlfAST2Text MlfPosInf              = "infinity"
mlfAST2Text MlfNegInf              = "neg_infinity"
mlfAST2Text MlfNaN                 = "nan"
mlfAST2Text (MlfConvert from to e) = ip $ T.concat
    ["convert", arithType2Text from, arithType2Text to, " ", mlfAST2Text e]
mlfAST2Text (MlfString e   ) = T.pack e
mlfAST2Text (MlfPlus at e f) = ip $ T.concat
    ["+", arithType2Text at, " ", mlfAST2Text e, " ", mlfAST2Text f]
mlfAST2Text (MlfMinus at e f) = ip $ T.concat
    ["-", arithType2Text at, " ", mlfAST2Text e, " ", mlfAST2Text f]
mlfAST2Text (MlfTimes at e f) = ip $ T.concat
    ["*", arithType2Text at, " ", mlfAST2Text e, " ", mlfAST2Text f]
mlfAST2Text (MlfDiv at e f) = ip $ T.concat
    ["/", arithType2Text at, " ", mlfAST2Text e, " ", mlfAST2Text f]
mlfAST2Text (MlfMod at e f) = ip $ T.concat
    ["%", arithType2Text at, " ", mlfAST2Text e, " ", mlfAST2Text f]
mlfAST2Text (MlfNeg at e) =
    ip $ T.concat ["neg", arithType2Text at, " ", mlfAST2Text e]
mlfAST2Text (MlfEq at e f) = ip $ T.concat
    ["==", arithType2Text at, " ", mlfAST2Text e, " ", mlfAST2Text f]

-- todo
mlfAST2Text (MlfLT           e1 e2 e3) = undefined
mlfAST2Text (MlfLTEq         e1 e2 e3) = undefined
mlfAST2Text (MlfGT           e1 e2 e3) = undefined
mlfAST2Text (MlfGTEq         e1 e2 e3) = undefined
mlfAST2Text (MlfBitAnd       e1 e2 e3) = undefined
mlfAST2Text (MlfBitOr        e1 e2 e3) = undefined
mlfAST2Text (MlfBitXOr       e1 e2 e3) = undefined
mlfAST2Text (MlfBitLShift    e1 e2 e3) = undefined
mlfAST2Text (MlfBitRShift    e1 e2 e3) = undefined
mlfAST2Text (MlfBitRShiftExt e1 e2 e3) = undefined

mlfAST2Text (MlfLam args body        ) = ip $ T.concat
    [ "lambda"
    , " "
    , ip . T.concat . map mlfAST2Text $ args
    , " "
    , mlfAST2Text body
    ]
mlfAST2Text (MlfApp fn args) = ip $ T.concat
    ["apply", " ", mlfAST2Text fn, " ", T.concat . map mlfAST2Text $ args]
mlfAST2Text (MlfLet binds e) = ip $ T.concat
    ["let", " ", T.concat $ map binding2Text binds, " ", mlfAST2Text e]
mlfAST2Text (MlfSeq es) =
    ip $ T.concat ["seq", " ", T.concat $ map mlfAST2Text es]
mlfAST2Text (MlfBlock tag fs) = ip $ T.concat
    [ "block"
    , " "
    , ip $ "tag " `T.append` textShow tag
    , " "
    , T.concat $ map mlfAST2Text fs
    ]
mlfAST2Text (MlfProjection n e) =
    ip $ T.concat ["field", " ", textShow n, " ", mlfAST2Text e]
mlfAST2Text (MlfVec vt len init) = ip $ T.concat
    ["makevec", vecType2text vt, " ", mlfAST2Text len, " ", mlfAST2Text init]
mlfAST2Text (MlfVecLoad vt vec idx) = ip $ T.concat
    ["load", vecType2text vt, " ", mlfAST2Text vec, " ", mlfAST2Text idx]
mlfAST2Text (MlfVecStore vt vec idx val) = ip $ T.concat
    [ "store"
    , vecType2text vt
    , " "
    , mlfAST2Text vec
    , " "
    , mlfAST2Text idx
    , " "
    , mlfAST2Text val
    ]
mlfAST2Text (MlfVecLen vt vec) =
    ip $ T.concat ["length", vecType2text vt, " ", mlfAST2Text vec]
mlfAST2Text (MlfLazy e) = ip $ T.concat ["lazy", " ", mlfAST2Text e]
mlfAST2Text (MlfForce e) = ip $ T.concat ["force", " ", mlfAST2Text e]
mlfAST2Text (MlfSwitch switch cases) = ip $ T.concat
    ["switch", " ", mlfAST2Text switch, " ", T.concat $ map case2Text cases]
mlfAST2Text (MlfIf cond true false) = ip $ T.concat
    ["if", " ", mlfAST2Text cond, " ", mlfAST2Text true, " ", mlfAST2Text false]
mlfAST2Text (MlfOCaml path fn) =
    ip $ T.concat ["global", " ", mlfAST2Text path, " ", mlfAST2Text fn]
mlfAST2Text (MlfComment c) = T.unlines $ map ("; " `T.append`) $ T.lines c
