module Expr.Types
    ( TypeTag(..)
    , TypedExpr
    , TypedExpr'(..)
    , Lit(..)
    , Val(..)
    , Op(..)
    , Expr(..)
    ) where

data TypeTag
    = UnknownType
    | StringType
    | IntType
    | BoolType
    deriving (Show)

-- the typed expression
data TypedExpr = TypedExpr
    { typeTag :: TypeTag
    , e :: TypedExpr'
    }
    deriving (Show)

data TypedExpr'
    = TLit Lit
    | TCol String
    | TApp Op [TypedExpr']
    deriving (Show)

data Lit
    = StringLit String
    | IntLit Integer
    | DoubleLit Double
    | BoolLit Bool
    deriving (Show)

data Val
    = StringVal String
    | IntVal Integer
    | DoubleVal Double
    | BoolVal Bool
    | NullVal
    deriving (Show)

data Op
    = Plus
    | Minus
    | Multiply
    | Divide
    | Equal
    | GreatThan
    | And
    | Or
    | Not
    | Fun String
    deriving (Show)

-- the untyped expression
data Expr
    = Lit Lit
    | Col String
    | App Op [Expr]
    deriving (Show)

