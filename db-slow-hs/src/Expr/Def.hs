module Expr.Def where

data SqlType
    = STUnknown
    | STString
    | STInt
    | STDouble
    | STBool
    deriving (Eq, Show)

data SqlVal
    = SVString String
    | SVInt Integer
    | SVDouble Double
    | SVBool Bool
    | SVNull
    deriving (Eq, Show)

data SqlExpr = SqlExpr
    { sExprType :: SqlType
    , sExprExpr :: SqlExpr'
    } deriving (Show)

newtype OpFun = OpFun ([SqlVal] -> SqlVal)

instance Show OpFun where
    show _ = "fun"

data SqlExpr'
    = SELit SqlVal
    | SECol String
    | SEApp Op OpFun [SqlExpr]
    deriving (Show)

-- # Expr Ops #

data Op
    = Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    | Exponential
    | Like
    | Is
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterThanEqual
    | LessThan
    | LessThanEqual
    | Not
    | And
    | Or
    | Fun String
    -- Cast TFrom TTo
    | Cast SqlType SqlType
    deriving (Eq, Show)

-- # OpContext API #

-- opCtxName [argType] retType fn
type SqlFnTuple = (String, [SqlType], SqlType, [SqlVal] -> SqlVal)

data OpContext = OpContext { opCtxName :: String, lookUpOp :: Op -> [SqlFnTuple] }

mergeOpContext :: OpContext -> OpContext -> OpContext
mergeOpContext ctx1 ctx2 = OpContext
    { opCtxName = "mergedCtx (" ++ (opCtxName ctx1) ++ "," ++ (opCtxName ctx2) ++ ")"
    , lookUpOp = \op -> lookUpOp ctx1 op ++ lookUpOp ctx2 op
    }
