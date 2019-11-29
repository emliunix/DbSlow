{-# LANGUAGE LambdaCase #-}
module PrimOps
    ( primOpContext
    ) where

import Data.List (find)

import Expr.Def
    ( Op (..)
    , SqlType (..)
    , SqlVal (..)
    , OpContext (..)
    )

-- def: op, priority, symbol, association, variants
-- variant: [argType] retType fn
primOps :: [(Op, [([SqlType], SqlType, [SqlVal] -> SqlVal)])]
primOps =
    [ (Plus,
        [ ([STInt, STInt], STInt, \case
            [SVInt i1, SVInt i2] -> SVInt $ i1 + i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STDouble, \case
            [SVDouble d1, SVDouble d2] -> SVDouble $ d1 + d2
            _ -> SVNull)
        ])
    , (Minus,
        [ ([STInt, STInt], STInt, \case
            [SVInt i1, SVInt i2] -> SVInt $ i1 - i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STDouble, \case
            [SVDouble d1, SVDouble d2] -> SVDouble $ d1 - d2
            _ -> SVNull)
        ])
    , (Multiply,
        [ ([STInt, STInt], STInt, \case
            [SVInt i1, SVInt i2] -> SVInt $ i1 * i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STDouble, \case
            [SVDouble d1, SVDouble d2] -> SVDouble $ d1 * d2
            _ -> SVNull)
        ])
    , (Divide,
        [ ([STInt, STInt], STDouble, \case
            [SVInt i1, SVInt i2] | i2 /= 0 -> SVDouble $ (fromIntegral i1) / (fromIntegral i2)
            _ -> SVNull
            )
        , ([STDouble, STDouble], STDouble, \case
            [SVDouble d1, SVDouble d2] | d2 /= 0.0 -> SVDouble $ d1 / d2
            _ -> SVNull)
        ])
    , (Equal,
        [ ([STInt, STInt], STBool, \case
            [SVInt i1, SVInt i2] -> SVBool $ i1 == i2
            _ -> SVNull)
        , ([STDouble, STDouble], STBool, \case
            [SVDouble d1, SVDouble d2] -> SVBool $ d1 == d2
            _ -> SVNull)
        , ([STBool, STBool], STBool, \case
            [SVBool b1, SVBool b2] -> SVBool $ b1 == b1
            _ -> SVNull)
        , ([STString, STString], STBool, \case
            [SVString s1, SVString s2] -> SVBool $ s1 == s2
            _ -> SVNull)
        ])
    , (GreaterThan,
        [ ([STInt, STInt], STBool, \case
            [SVInt i1, SVInt i2] -> SVBool $ i1 > i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STBool, \case
            [SVDouble d1, SVDouble d2] -> SVBool $ d1 > d2
            _ -> SVNull)
        ])
    , (GreaterThanEqual,
        [ ([STInt, STInt], STBool, \case
            [SVInt i1, SVInt i2] -> SVBool $ i1 >= i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STBool, \case
            [SVDouble d1, SVDouble d2] -> SVBool $ d1 >= d2
            _ -> SVNull)
        ])
    , (LessThan,
        [ ([STInt, STInt], STBool, \case
            [SVInt i1, SVInt i2] -> SVBool $ i1 < i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STBool, \case
            [SVDouble d1, SVDouble d2] -> SVBool $ d1 < d2
            _ -> SVNull)
        ])
    , (LessThanEqual,
        [ ([STInt, STInt], STBool, \case
            [SVInt i1, SVInt i2] -> SVBool $ i1 <= i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STBool, \case
            [SVDouble d1, SVDouble d2] -> SVBool $ d1 <= d2
            _ -> SVNull)
        ])
    , (NotEqual,
        [ ([STInt, STInt], STBool, \case
            [SVInt i1, SVInt i2] -> SVBool $ i1 <= i2
            _ -> SVNull
            )
        , ([STDouble, STDouble], STBool, \case
            [SVDouble d1, SVDouble d2] -> SVBool $ d1 <= d2
            _ -> SVNull)
        ])
    , (And,
        [ ([STBool, STBool], STBool, \case
            [SVBool i1, SVBool i2] -> SVBool $ i1 && i2
            _ -> SVNull)
            ])
    , (Or,
        [ ([STBool, STBool], STBool, \case
            [SVBool i1, SVBool i2] -> SVBool $ i1 || i2
            _ -> SVNull)
        ])
    , (Cast STInt STDouble,
        [ ([STInt], STDouble, \case
            [SVInt i1] -> SVDouble $ fromInteger i1)
        ])
    ]

primOpContext :: OpContext
primOpContext = OpContext
    { opCtxName = "prim"
    , lookUpOp = \op ->
        case find (\opDef -> opOfOpDef opDef == op) primOps of
            Just (_, fns) -> fmap (\(argTypes, retType, fn) -> ("prim", argTypes, retType, fn)) fns
            Nothing -> []
    }

opOfOpDef :: (Op, [([SqlType], SqlType, [SqlVal] -> SqlVal)]) -> Op
opOfOpDef (op, _) = op
