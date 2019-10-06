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
    , castDef
    , valType
    , typeCheck
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
    , opDefPriority :: Int
    , opDefSymbol :: String
    , opDefPolyDefs :: [OpPolyDef]
    }

mkMonoTypeBinOp :: Op -> Int -> String -> [(TypeTag, [Val] -> Val)] -> OpDef
mkMonoTypeBinOp op priority symbol typeAndFuns = OpDef
    { opDefOp = op
    , opDefPriority = priority
    , opDefSymbol = symbol
    , opDefPolyDefs =
        fmap
            (\(typeTag, opFun) -> OpPolyDef
                { opPolyDefArgTypes = [typeTag, typeTag]
                , opPolyDefRetType = typeTag
                , opPolyDefEvalFun = opFun
                })
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
        [ (IntType, \[(IntVal i1), (IntVal i2)] ->v (IntVal $ i1 - i2))
        , (DoubleType, \[(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 - i2))
        ]
    , mkMonoTypeBinOp Multiply 2 "*"
        [ (IntType, \[(IntVal i1), (IntVal i2)] -> (IntVal $ i1 * i2))
        , (DoubleType, \[(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 * i2))
        ]
    , mkMonoTypeBinOp Divide 2 "/"
        [ (IntType, \[(IntVal i1), (IntVal i2)] -> (IntVal $ i1 `div` i2))
        , (DoubleType, \[(DoubleVal i1), (DoubleVal i2)] -> (DoubleVal $ i1 / i2))
        ]
    ] ++ (mkCasts allCasts)

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
    [ (Implicit, IntType, DoubleType, \(IntVal i1) -> (DoubleVal $ ((fromIntegral i1) :: Double)))
    , (Explicit, DoubleType, IntType, \(IntVal i1) -> (DoubleVal $ ((fromIntegral i1) :: Double)))
    ]

mkCastOp :: CastDef -> OpDef
mkCastOp (k, tf, tt, f) = OpDef
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

implicitCasts :: TypeTag -> TypeTag -> Maybe (TypedExpr -> TypedExpr)
-- castDef StringType IntType = Just $ \(StrintVal v) -> (IntVal $ fromString v)
-- castDef IntType DoubleType = Just $ \(IntVal v) -> (DoubleVal $ ((fromIntegral v) :: Double))
implicitCasts tFrom tTo =
    case find
        (\x -> case x of
            (Implicit, tF, tT, _) -> tF == tFrom && tT == tTo
            _ -> False)
        castDefs
    of
        Just (_, _, _, f) -> Just $ (\e -> TypedExpr
                { typedExprTypeTag = tTo
                , typedExprE = TApp (Cast tFrom tTo) [e] f
                }
            )

-- >>> Lit $ StringVal "xxxxx"

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
        case implicitCasts (typedExprTypeTag expr) toType of
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
                    (opArgTypes, opRetType) <- return $ (opPolyDefArgTypes opPoly, opPolyDefRetType opPoly);
                    -- add types to null values
                    typedArgs <- return $ fmap (\(t, e) -> inferNullType t e) (zip opArgTypes typedArgs);
                    -- typeCheck and add implicitCasts if neccessary
                    -- 1. typeCheck
                    -- 2. add implicit casts
                    -- 3. add OpFun
                    (castedExprs, opFun) <- addCasts opArgTypes typedArgs;
                    return $ TypedExpr
                        { typedExprTypeTag = opRetType
                        , typedExprE = TApp op castedExprs opFun
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
        addCasts :: [TypeTag] -> [TypedExpr] -> Either String ([TypedExpr], OpFun)
        addCasts toTypes args =
            let res = fmap (\(t, a) -> (addCast a t, t, a)) (zip toTypes args)
                (vs, errs) = splitErr res
            in
                if length errs == 0 then
                    Right vs
                else
                    Left $ intercalate "\n" errs
