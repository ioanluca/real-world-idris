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

    | MlfInt32 Int --fixme
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

    | MlfComment String
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

name2Text :: MlfName -> T.Text
name2Text name = '$' `T.cons` T.pack name

binding2Text :: MlfBinding -> T.Text
binding2Text (RegBinding name e) =
    ip $ T.concat [name2Text name, " ", mlfAST2Text e]
binding2Text (ExecBinding e) = ip $ T.concat ["_", " ", mlfAST2Text e]
binding2Text (RecBinding es) =
    ip $ T.concat ["rec", " "] `T.append` T.concat (map binding2Text es)

mlfAST2Text :: MlfExp -> T.Text
mlfAST2Text (MlfProg binds e) =
    ip
        $  T.concat
        $  [ip "module", " "]
        ++ map binding2Text binds
        ++ [ip "export"]
mlfAST2Text (MlfVar    e)              = name2Text e
mlfAST2Text (MlfInt32  e)              = undefined
mlfAST2Text (MlfInt64  e)              = undefined
mlfAST2Text (MlfInt    e)              = undefined
mlfAST2Text (MlfBigInt e)              = undefined
mlfAST2Text (MlfFloat  e)              = undefined
mlfAST2Text MlfPosInf                  = undefined
mlfAST2Text MlfNegInf                  = undefined
mlfAST2Text MlfNaN                     = undefined
mlfAST2Text (MlfConvert e1 e2 e3     ) = undefined
mlfAST2Text (MlfString e             ) = undefined
mlfAST2Text (MlfPlus  e1 e2 e3       ) = undefined
mlfAST2Text (MlfMinus e1 e2 e3       ) = undefined
mlfAST2Text (MlfTimes e1 e2 e3       ) = undefined
mlfAST2Text (MlfDiv   e1 e2 e3       ) = undefined
mlfAST2Text (MlfMod   e1 e2 e3       ) = undefined
mlfAST2Text (MlfNeg e1 e2            ) = undefined
mlfAST2Text (MlfEq           e1 e2 e3) = undefined
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
mlfAST2Text (MlfLam e1 e2            ) = undefined
mlfAST2Text (MlfApp e1 e2            ) = undefined
mlfAST2Text (MlfLet e1 e2            ) = undefined
mlfAST2Text (MlfSeq e                ) = undefined
mlfAST2Text (MlfBlock      e1 e2     ) = undefined
mlfAST2Text (MlfProjection e1 e2     ) = undefined
mlfAST2Text (MlfVec     e1 e2 e3     ) = undefined
mlfAST2Text (MlfVecLoad e1 e2 e3     ) = undefined
mlfAST2Text (MlfVecStore e1 e2 e3 e4 ) = undefined
mlfAST2Text (MlfVecLen e1 e2         ) = undefined
mlfAST2Text (MlfLazy  e              ) = undefined
mlfAST2Text (MlfForce e              ) = undefined
mlfAST2Text (MlfSwitch e1 e2         ) = undefined
mlfAST2Text (MlfIf e1 e2 e3          ) = undefined
mlfAST2Text (MlfOCaml e1 e2          ) = undefined
mlfAST2Text (MlfComment e            ) = undefined
