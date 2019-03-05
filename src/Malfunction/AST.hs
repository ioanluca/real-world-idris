{-# LANGUAGE OverloadedStrings #-}

module Malfunction.AST where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T

-- data MlfProgram
--     = Interpreted [MlfBinding] MlfExp
--     | Compiled [MlfBinding] MlfExp
--     deriving (Eq, Show)

type MlfName = Text

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

data MlfPrim
    = MlfPlus | MlfMinus | MlfTimes | MlfDiv | MlfMod
    | MlfNeg | MlfEq | MlfLT | MlfLTEq | MlfGT | MlfGTEq
    -- not available for floats
    | MlfBitAnd | MlfBitOr | MlfBitXOr | MlfBitLShift
    | MlfBitRShift | MlfBitRShiftExt
    deriving (Eq, Show)

data MlfConst
    = MlfInt32 Int
    | MlfInt64 Int  --fixme
    | MlfInt Int
    | MlfBigInt Integer
    | MlfFloat Float
    | MlfPosInf
    | MlfNegInf
    | MlfNaN
    | MlfString String --they return vec of bytes
    deriving (Eq, Show)

data MlfExp
    = MlfProg [MlfBinding] MlfExp
    | MlfVar MlfName

    | MlfLiteral MlfConst
    | MlfConvert MlfArithType MlfArithType MlfExp

    | MlfOp MlfPrim MlfArithType [MlfExp]

    | MlfLam [MlfName] MlfExp
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

    | MlfComment Text
    deriving (Eq, Show)

textShow :: Show a => a -> Text
textShow = T.pack . show

ip :: Text -> Text
ip e = T.concat ["(", e, ")"]

arithType2Text :: MlfArithType -> Text
arithType2Text Int32Arith  = ".int32"
arithType2Text Int64Arith  = ".int64"
arithType2Text IntArith    = T.empty
arithType2Text BigIntArith = ".ibig"
arithType2Text FloatArith  = ".f64"

vecType2text :: MlfVecType -> Text
vecType2text Normal = T.empty
vecType2text Byte   = ".byte"

name2Text :: MlfName -> Text
name2Text name = '$' `T.cons` name

binding2Text :: MlfBinding -> Text
binding2Text (RegBinding name e) =
    ip $ T.concat [name2Text name, " ", mlfAST2Text e]
binding2Text (ExecBinding e) = ip $ T.concat ["_", " ", mlfAST2Text e]
binding2Text (RecBinding es) =
    ip $ T.concat ["rec", " "] `T.append` T.concat (map binding2Text es)

sel2Text :: MlfSel -> Text
sel2Text (IntSel n       ) = textShow n
sel2Text (IntRangeSel n m) = ip $ T.concat [textShow n, " ", textShow m]
sel2Text DefaultInt        = "_"
sel2Text (Tag tag)         = ip $ T.concat ["tag", " ", textShow tag]
sel2Text DefaultTag        = ip $ T.concat ["tag", " ", "_"]

case2Text :: MlfCase -> Text
case2Text (sels, e) =
    ip $ T.concat (map sel2Text sels) `T.append` mlfAST2Text e

prim2Text :: MlfPrim -> Text
prim2Text MlfPlus         = "+"
prim2Text MlfMinus        = "-"
prim2Text MlfTimes        = "*"
prim2Text MlfDiv          = "/"
prim2Text MlfMod          = "%"
prim2Text MlfNeg          = "neg"
prim2Text MlfEq           = "=="
prim2Text MlfLT           = "<"
prim2Text MlfLTEq         = "<="
prim2Text MlfGT           = ">"
prim2Text MlfGTEq         = ">="
prim2Text MlfBitAnd       = "&"
prim2Text MlfBitOr        = "|"
prim2Text MlfBitXOr       = "^"
prim2Text MlfBitLShift    = "<<"
prim2Text MlfBitRShift    = ">>"
prim2Text MlfBitRShiftExt = "a>>"

const2Text :: MlfConst -> Text
const2Text (MlfInt32  e) = textShow e `T.append` arithType2Text Int32Arith
const2Text (MlfInt64  e) = textShow e `T.append` arithType2Text Int64Arith
const2Text (MlfBigInt e) = textShow e `T.append` arithType2Text BigIntArith
const2Text (MlfInt    e) = textShow e
const2Text (MlfFloat  e) = textShow e
const2Text MlfPosInf     = "infinity"
const2Text MlfNegInf     = "neg_infinity"
const2Text MlfNaN        = "nan"
const2Text (MlfString e) = T.pack e

mlfAST2Text :: MlfExp -> Text
mlfAST2Text (MlfProg binds e) =
    ip
        $  T.concat
        $  [ip "module", " "]
        ++ map binding2Text binds
        ++ [ip "export"]
mlfAST2Text (MlfVar     e        ) = name2Text e
mlfAST2Text (MlfLiteral const    ) = const2Text const
mlfAST2Text (MlfConvert from to e) = ip $ T.concat
    ["convert", arithType2Text from, arithType2Text to, " ", mlfAST2Text e]
mlfAST2Text (MlfOp op at es) = ip $ T.concat
    [prim2Text op, arithType2Text at, " ", T.concat $ map mlfAST2Text es]
mlfAST2Text (MlfLam args body) = ip $ T.concat
    ["lambda", " ", ip . T.concat . map name2Text $ args, " ", mlfAST2Text body]
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

indent :: Text -> Text
indent x =
    let l  = T.lines x
        il = map (\y -> T.replicate 4 " " `T.append` y) l
    in  T.unlines il
