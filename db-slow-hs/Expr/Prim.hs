module Expr.Prim
    ( OpDef
    , OpPolyDef
    , opDefs
    , castDefs
    , FunDef
    , funDefs
    ) where

import Expr.Types
    ( TypeTag(..)
    , Val(..)
    , Op(..)
    )

data OpPolyDef = OpPolyDef
    { argTypes :: [TypeTag]
    , retType :: TypeTag
    , evalFun :: [Val] -> Val
    }

data OpDef = OpDef
    { op :: Op
    , priority :: Int
    , symbol :: String
    , polyDefs :: [OpPolyDef]
    }

mkMonoTypeBinOp :: Op -> Int -> String -> [(TypeTag, [Val] -> Val)] -> OpDef
mkMonoTypeBinOp op priority symbol typeAndFuns = OpDef
    { op = op
    , priority = priority
    , symbol = symbol
    , polyDefs =
        fmap $
            \(typeTag, opFun) -> OpPolyDef
                { argTypes = [typeTag, typeTag]
                , retType = typeTag
                , evalFun = opFun
                }
            typeAndFuns
    }

opDefs :: [OpDef]
opDefs =
    [ mkMonoTypeBinOp Plus 1 "+"
        [ (IntType, \x -> case x of
            [(IntVal i1), (IntVal i2)] -> (IntVal $ i1 + i2)
            _ -> NullVal)
        , (DoubleType, \x -> case x of
            [(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 + i2)
            _ -> NullVal)
        ]
    , mkMonoTypeBinOp Minus 1 "-"
        [ (IntType, \[(IntVal i1), (IntVal i2)] -> (IntVal $ i1 - i2))
        , (DoubleType, \[(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 - i2))
        ]
    , mkMonoTypeBinOp Multiply 2 "*"
        [ (IntType, \[(IntVal i1), (IntVal i2)] -> (IntVal $ i1 * i2))
        , (DoubleType, \[(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 * i2))
        ]
    , mkMonoTypeBinOp Divide 2 "/"
        [ (IntType, \[(IntVal i1), (IntVal i2)] -> (IntVal $ i1 / i2))
        , (DoubleType, \[(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 / i2))
        ]
    ]

-- for simplicity, maybe omit implicit casting
-- cast can only be triggered with CAST( AS )

castDef :: TypeTag -> TypeTag -> Maybe (Val -> Val)
castDef StringType IntType = Just $ \(StringVal v) -> (IntVal $ fromString v)
castDef IntType DoubleType = Just $ \(IntVal v) -> (DoubleVal $ ((fromIntegral v) :: Double))
