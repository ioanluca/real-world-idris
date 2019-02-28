module Malfunction.AST
    ( defaultCase
    )
where

data MlfProgram =
    Interpreted MlfExp | Compiled [MlfBinding] MlfExp
    deriving (Eq, Show)

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
    = MlfVar MlfName

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
    deriving (Eq, Show)
