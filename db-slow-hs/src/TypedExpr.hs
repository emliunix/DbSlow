{-# LANGUAGE LambdaCase #-}
module TypedExpr 
    ( TypeTag(..)
    , TypedExpr(..)
    , TypedExpr2(..)
    , Val(..)
    , Op(..)
    , Expr(..)
    , OpDef(..)
    , OpPolyDef(..)
    , opDefs
    , opDefLookup
    , castDefs
    , valType
    , typeCheck
    , evalTypedExpr
    ) where

import Data.List
import Data.Either
import Control.Monad

-- **** type definitions ****

data TypeTag
    = UnknownType
    | StringType
    | IntType
    | DoubleType
    | BoolType
    deriving (Eq, Show)

-- the typed expression
data TypedExpr = TypedExpr
    { typedExprTypeTag :: TypeTag
    , typedExprE :: TypedExpr2
    }
    deriving (Show)

newtype OpFun = OpFun ([Val] -> Val)
instance Show OpFun where
    show _ = "fun"

data TypedExpr2
    = TLit Val
    | TCol String
    | TApp Op [TypedExpr] OpFun
    deriving (Show)

data Val
    = StringVal String
    | IntVal Integer
    | DoubleVal Double
    | BoolVal Bool
    | NullVal
    deriving (Eq, Show)

data Op
    = Plus
    | Minus
    | Multiply
    | Divide
    | Equal
    | GreatThan
    | GreatThanEqual
    | LessThan
    | LessThanEqual
    | And
    | Or
    | Not
    | Fun String
    -- Cast TFrom TTo
    | Cast TypeTag TypeTag
    deriving (Eq, Show)

-- the untyped expression
data Expr
    = Lit Val
    | Col String
    | App Op [Expr]
    deriving (Show)

-- **** primitives ****

data OpPolyDef = OpPolyDef
    { opPolyDefArgTypes :: [TypeTag]
    , opPolyDefRetType :: TypeTag
    , opPolyDefEvalFun :: [Val] -> Val
    }

data OpDef = OpDef
    { opDefOp :: Op
    , opDefPolyDefs :: [OpPolyDef]
    }

primOps :: [(Op, Int, String, [([TypeTag], TypeTag, [Val] -> Val)])]
primOps =
    [ (Plus, 1, "+",
        [ ([IntType, IntType], IntType, \case
            [IntVal i1, IntVal i2] -> IntVal $ i1 + i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], DoubleType, \case
            [DoubleVal d1, DoubleVal d2] -> DoubleVal $ d1 + d2
            _ -> NullVal)
        ])
    , (Minus, 1, "-",
        [ ([IntType, IntType], IntType, \case
            [IntVal i1, IntVal i2] -> IntVal $ i1 - i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], DoubleType, \case
            [DoubleVal d1, DoubleVal d2] -> DoubleVal $ d1 - d2
            _ -> NullVal)
        ])
    , (Multiply, 2, "*",
        [ ([IntType, IntType], IntType, \case
            [IntVal i1, IntVal i2] -> IntVal $ i1 * i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], DoubleType, \case
            [DoubleVal d1, DoubleVal d2] -> DoubleVal $ d1 * d2
            _ -> NullVal)
        ])
    , (Divide, 2, "/",
        [ ([IntType, IntType], DoubleType, \case
            [IntVal i1, IntVal i2] | i2 /= 0 -> DoubleVal $ (fromIntegral i1) / (fromIntegral i2)
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], DoubleType, \case
            [DoubleVal d1, DoubleVal d2] | d2 /= 0.0 -> DoubleVal $ d1 / d2
            _ -> NullVal)
        ])
    , (Equal, 2, "=",
        [ ([IntType, IntType], BoolType, \case
            [IntVal i1, IntVal i2] -> BoolVal $ i1 == i2
            _ -> NullVal)
        , ([DoubleType, DoubleType], BoolType, \case
            [DoubleVal d1, DoubleVal d2] -> BoolVal $ d1 == d2
            _ -> NullVal)
        , ([BoolType, BoolType], BoolType, \case
            [BoolVal b1, BoolVal b2] -> BoolVal $ b1 == b1
            _ -> NullVal)
        , ([StringType, StringType], BoolType, \case
            [StringVal s1, StringVal s2] -> BoolVal $ s1 == s2
            _ -> NullVal)
        ])
    , (GreatThan, 2, ">",
        [ ([IntType, IntType], BoolType, \case
            [IntVal i1, IntVal i2] -> BoolVal $ i1 > i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], BoolType, \case
            [DoubleVal d1, DoubleVal d2] -> BoolVal $ d1 > d2
            _ -> NullVal)
        ])
    , (GreatThanEqual, 2, ">=",
        [ ([IntType, IntType], BoolType, \case
            [IntVal i1, IntVal i2] -> BoolVal $ i1 >= i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], BoolType, \case
            [DoubleVal d1, DoubleVal d2] -> BoolVal $ d1 >= d2
            _ -> NullVal)
        ])
    , (LessThan, 2, "<",
        [ ([IntType, IntType], BoolType, \case
            [IntVal i1, IntVal i2] -> BoolVal $ i1 < i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], BoolType, \case
            [DoubleVal d1, DoubleVal d2] -> BoolVal $ d1 < d2
            _ -> NullVal)
        ])
    , (LessThanEqual, 2, "<=",
        [ ([IntType, IntType], BoolType, \case
            [IntVal i1, IntVal i2] -> BoolVal $ i1 <= i2
            _ -> NullVal
            )
        , ([DoubleType, DoubleType], BoolType, \case
            [DoubleVal d1, DoubleVal d2] -> BoolVal $ d1 <= d2
            _ -> NullVal)
        ])
    , (And, 2, "and",
        [ ([BoolType, BoolType], BoolType, \case
            [BoolVal i1, BoolVal i2] -> BoolVal $ i1 && i2
            _ -> NullVal)
            ])
    , (Or, 2, "or",
        [ ([BoolType, BoolType], BoolType, \case
            [BoolVal i1, BoolVal i2] -> BoolVal $ i1 || i2
            _ -> NullVal)
        ])
    ]

primOps2 :: [(Op, Int, String, [([TypeTag], TypeTag, [Val] -> Val)])]
primOps2 =
    [
    ]

mkPrimOp :: (Op, Int, String, [([TypeTag], TypeTag, [Val] -> Val)]) -> OpDef
mkPrimOp (op, _, _, typeAndFuns) = OpDef
    { opDefOp = op
    , opDefPolyDefs =
        fmap
            (\(argTypes, retType, opFun) -> OpPolyDef
                { opPolyDefArgTypes = argTypes
                , opPolyDefRetType = retType
                , opPolyDefEvalFun = opFun
                })
            typeAndFuns
    }

opDefs :: [OpDef]
opDefs =
    (fmap mkPrimOp primOps) ++ (fmap mkCastOp castDefs)

opDefLookup :: Op -> Maybe OpDef
opDefLookup op =
    go opDefs
    where
        go [] = Nothing
        go (x:xs)
            | (opDefOp x) == op = Just x
            | otherwise = go xs

data CastKind
    = Implicit
    | Explicit

newtype CastDef = CastDef (CastKind, TypeTag, TypeTag, [Val] -> Val)

castDefs :: [CastDef]
castDefs =
    [ CastDef (Implicit, IntType, DoubleType, \case
        [(IntVal i1)] -> (DoubleVal $ ((fromIntegral i1) :: Double))
        _ -> NullVal)
    , CastDef (Explicit, DoubleType, IntType, \case
        [(IntVal i1)] -> (DoubleVal $ ((fromIntegral i1) :: Double))
        _ -> NullVal)
    ]

-- opDefs
-- implicitCastDefs
-- funDefs

mkCastOp :: CastDef -> OpDef
mkCastOp (CastDef (_, tf, tt, f)) = OpDef
    { opDefOp = Cast tf tt
    , opDefPolyDefs =
        [ OpPolyDef
            { opPolyDefArgTypes = [tf]
            , opPolyDefRetType = tt
            , opPolyDefEvalFun = f
            }
        ]
    }

-- for simplicity, maybe omit implicit casting
-- cast can only be triggered with CAST( AS )

implicitCastLookup :: TypeTag -> TypeTag -> Maybe (TypedExpr -> TypedExpr)
-- castDef StringType IntType = Just $ \(StrintVal v) -> (IntVal $ fromString v)
-- castDef IntType DoubleType = Just $ \(IntVal v) -> (DoubleVal $ ((fromIntegral v) :: Double))
implicitCastLookup tFrom tTo = do
    {
        CastDef (_, _, _, f) <- find
            (\case
                CastDef (Implicit, tF, tT, _) -> tF == tFrom && tT == tTo
                _ -> False)
            castDefs
        ;
        return (\e -> TypedExpr
            { typedExprTypeTag = tTo
            , typedExprE = TApp (Cast tFrom tTo) [e] (OpFun f)
            }
        )
    }

-- >>> Lit $ StringVal "xxxxx"
-- Lit (StringVal "xxxxx")
--

valType :: Val -> TypeTag
valType NullVal = UnknownType
valType (StringVal _) = StringType
valType (IntVal _) = IntType
valType (DoubleVal _) = DoubleType
valType (BoolVal _) = BoolType

-- **** typeCheck ****

addCast :: TypedExpr -> TypeTag -> Maybe TypedExpr
addCast expr toType
    | (typedExprTypeTag expr) == toType = Just expr
    | (typedExprTypeTag expr) /= toType =
        case implicitCastLookup (typedExprTypeTag expr) toType of
            Just f -> Just $ f expr
            Nothing -> Nothing
    | otherwise = Nothing


inferNullType :: TypeTag -> TypedExpr -> TypedExpr
inferNullType targetType expr = case expr of
    TypedExpr { typedExprE = TLit NullVal } -> expr { typedExprTypeTag = targetType }
    _ -> expr

typeCheck :: (String -> TypeTag) -> Expr -> Either String TypedExpr
typeCheck colTypeLookup expr =
    case expr of
        Lit v -> return $ TypedExpr
            { typedExprTypeTag = valType v
            , typedExprE = TLit v
            }
        Col n -> return $ TypedExpr
            { typedExprTypeTag = colTypeLookup n
            , typedExprE = TCol n
            }
        App op args -> do {
            typedArgs <- forM args (typeCheck colTypeLookup);
            opDef <- (case opDefLookup op of
                Just opDef -> Right opDef
                Nothing -> Left $ "Unknown op " ++ (show op));
            let matches = fmap (\opPoly -> (do {
                    (opArgTypes, opRetType, opFun) <- return $ (opPolyDefArgTypes opPoly, opPolyDefRetType opPoly, opPolyDefEvalFun opPoly);
                    -- add types to null values
                    typedArgs <- return $ fmap (\(t, e) -> inferNullType t e) (zip opArgTypes typedArgs);
                    -- typeCheck and add implicitCasts if neccessary
                    -- 1. typeCheck
                    -- 2. add implicit casts
                    -- 3. add OpFun
                    castedExprs <- addCasts opArgTypes typedArgs;
                    return $ TypedExpr
                        { typedExprTypeTag = opRetType
                        , typedExprE = TApp op castedExprs (OpFun opFun)
                        }
                })) (opDefPolyDefs opDef)
            in case find isRight matches of
                -- if any of the op variant matches
                Just m -> m
                Nothing -> Left $ "Type mimatch for op " ++ (show op) ++ " tried:\n" ++ (
                        intercalate "\n" $ fmap
                            (\(err, opPoly) ->
                                    "Candidate: " ++ (intercalate ", " $ fmap show $ opPolyDefArgTypes opPoly) ++ "\n"
                                    ++ err
                                )
                            (zip (lefts matches) (opDefPolyDefs opDef))
                    )
        }
    where
        splitErr :: [(Maybe TypedExpr, TypeTag, TypedExpr)] -> ([TypedExpr], [String])
        splitErr [] = ([], [])
        splitErr ((x, t, a):xs) =
            let (l, r) = splitErr xs in
                case x of
                    Just v -> (v:l, r)
                    Nothing ->
                        let errMsg = "Cast Error: " ++ (show $ typedExprTypeTag a) ++ " -> " ++ (show t) ++ " for " ++ (show $ typedExprE a) in
                            (l, errMsg:r)
        addCasts :: [TypeTag] -> [TypedExpr] -> Either String [TypedExpr]
        addCasts toTypes args =
            let res = fmap (\(t, a) -> (addCast a t, t, a)) (zip toTypes args)
                (vs, errs) = splitErr res
            in
                if length errs == 0 then
                    Right vs
                else
                    Left $ intercalate "\n" errs

evalTypedExpr :: (String -> Val) -> TypedExpr -> Val
evalTypedExpr colValLookup expr =
    case typedExprE expr of
        TLit v -> v
        TCol n -> colValLookup n
        TApp _ exprs (OpFun f) ->
            f $ fmap (evalTypedExpr colValLookup) exprs

unifyAppExpr op ctx args = do
    opFns <- return $ lookUpFns ctx op
    argTypes <- return $ _argTypes args
    case _tryDirectMatch opFns argTypes of
        Just fn -> return $ mkApp op fn args
        Nothing ->
            let argTypesCands = fmap (\(fnArgTypes, t -> intersect fnArgTypes (t:_getCasts t)) $ zip (getFnArgTypes opFns) argTypes

            in do
                _searchMatch opFns argTypesCands
    where
        _searchMatch fns typeCands =
            let maxIter = max $ fmap length typeCands
            in
                _searchMatch' [] typeCands 0
                where
                    _searchMatch' [] typeCands 0
            