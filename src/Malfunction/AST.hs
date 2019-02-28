module Malfunction.AST
    ( defaultCase
    )
where

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
    deriving (Eq, Show)

mlfAST2Text :: MlfExp -> String
mlfAST2Text e = go e ""

ip :: String -> String
ip e = '(' : e ++ ") "

arithType2Text :: MlfArithType -> String
arithType2Text Int32Arith  = ".int32"
arithType2Text Int64Arith  = ".int64"
arithType2Text IntArith    = ""
arithType2Text BigIntArith = ".ibig"
arithType2Text FloatArith  = ".f64"

go :: MlfExp -> String -> String
go (MlfProg bindings e2) k = undefined
go (MlfVar    e        ) k = '$' : e ++ k

go (MlfInt32  e        ) k = show e ++ arithType2Text Int32Arith ++ k
go (MlfInt64  e        ) k = show e ++ arithType2Text Int64Arith ++ k
go (MlfInt    e        ) k = show e ++ k
go (MlfBigInt e        ) k = show e ++ arithType2Text BigIntArith ++ k
go (MlfFloat  e        ) k = show e ++ k
go MlfPosInf             k = "infinity" ++ k
go MlfNegInf             k = "neg_infinity" ++ k
go MlfNaN                k = "nan"

go (MlfConvert from to e) k =
    ip $ "convert" ++ arithType2Text from ++ arithType2Text to ++ go e k ++ k

go (MlfString e             ) k = undefined
go (MlfPlus  e1 e2 e3       ) k = undefined
go (MlfMinus e1 e2 e3       ) k = undefined
go (MlfTimes e1 e2 e3       ) k = undefined
go (MlfDiv   e1 e2 e3       ) k = undefined
go (MlfMod   e1 e2 e3       ) k = undefined
go (MlfNeg e1 e2            ) k = undefined
go (MlfEq           e1 e2 e3) k = undefined
go (MlfLT           e1 e2 e3) k = undefined
go (MlfLTEq         e1 e2 e3) k = undefined
go (MlfGT           e1 e2 e3) k = undefined
go (MlfGTEq         e1 e2 e3) k = undefined
go (MlfBitAnd       e1 e2 e3) k = undefined
go (MlfBitOr        e1 e2 e3) k = undefined
go (MlfBitXOr       e1 e2 e3) k = undefined
go (MlfBitLShift    e1 e2 e3) k = undefined
go (MlfBitRShift    e1 e2 e3) k = undefined
go (MlfBitRShiftExt e1 e2 e3) k = undefined
go (MlfLam e1 e2            ) k = undefined
go (MlfApp e1 e2            ) k = undefined
go (MlfLet e1 e2            ) k = undefined
go (MlfSeq e                ) k = undefined
go (MlfBlock      e1 e2     ) k = undefined
go (MlfProjection e1 e2     ) k = undefined
go (MlfVec     e1 e2 e3     ) k = undefined
go (MlfVecLoad e1 e2 e3     ) k = undefined
go (MlfVecStore e1 e2 e3 e4 ) k = undefined
go (MlfVecLen e1 e2         ) k = undefined
go (MlfLazy  e              ) k = undefined
go (MlfForce e              ) k = undefined
go (MlfSwitch e1 e2         ) k = undefined
go (MlfIf e1 e2 e3          ) k = undefined
go (MlfOCaml e1 e2          ) k = undefined

    -- show sexp = render sexp ""   where
    --     render :: Sexp -> String -> String
    --     render (S       s) k = "(" ++ foldr render (") " ++ k) s
    --     render (A       s) k = s ++ " " ++ k
    --     render (KInt    n) k = show n ++ " " ++ k
    --     render (KStr    s) k = show s ++ " " ++ k
    --     render (KBigInt s) k = show s ++ ".ibig " ++ k
