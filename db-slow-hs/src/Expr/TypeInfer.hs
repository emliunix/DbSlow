{-# LANGUAGE LambdaCase #-}
module Expr.TypeInfer where

import Data.List
import Data.Either
import Control.Monad


-- implicitCastLookup :: TypeTag -> TypeTag -> Maybe (TypedExpr -> TypedExpr)
-- -- castDef StringType IntType = Just $ \(StrintVal v) -> (IntVal $ fromString v)
-- -- castDef IntType DoubleType = Just $ \(IntVal v) -> (DoubleVal $ ((fromIntegral v) :: Double))
-- implicitCastLookup tFrom tTo = do
--     {
--         CastDef (_, _, _, f) <- find
--             (\case
--                 CastDef (Implicit, tF, tT, _) -> tF == tFrom && tT == tTo
--                 _ -> False)
--             castDefs
--         ;
--         return (\e -> TypedExpr
--             { typedExprTypeTag = tTo
--             , typedExprE = TApp (Cast tFrom tTo) [e] (OpFun f)
--             }
--         )
--     }

-- -- >>> Lit $ StringVal "xxxxx"
-- -- Lit (StringVal "xxxxx")
-- --

-- valType :: Val -> TypeTag
-- valType NullVal = UnknownType
-- valType (StringVal _) = StringType
-- valType (IntVal _) = IntType
-- valType (DoubleVal _) = DoubleType
-- valType (BoolVal _) = BoolType

-- -- **** typeCheck ****

-- addCast :: TypedExpr -> TypeTag -> Maybe TypedExpr
-- addCast expr toType
--     | (typedExprTypeTag expr) == toType = Just expr
--     | (typedExprTypeTag expr) /= toType =
--         case implicitCastLookup (typedExprTypeTag expr) toType of
--             Just f -> Just $ f expr
--             Nothing -> Nothing
--     | otherwise = Nothing


-- inferNullType :: TypeTag -> TypedExpr -> TypedExpr
-- inferNullType targetType expr = case expr of
--     TypedExpr { typedExprE = TLit NullVal } -> expr { typedExprTypeTag = targetType }
--     _ -> expr

-- typeCheck :: (String -> TypeTag) -> Expr -> Either String TypedExpr
-- typeCheck colTypeLookup expr =
--     case expr of
--         Lit v -> return $ TypedExpr
--             { typedExprTypeTag = valType v
--             , typedExprE = TLit v
--             }
--         Col n -> return $ TypedExpr
--             { typedExprTypeTag = colTypeLookup n
--             , typedExprE = TCol n
--             }
--         App op args -> do {
--             typedArgs <- forM args (typeCheck colTypeLookup);
--             opDef <- (case opDefLookup op of
--                 Just opDef -> Right opDef
--                 Nothing -> Left $ "Unknown op " ++ (show op));
--             let matches = fmap (\opPoly -> (do {
--                     (opArgTypes, opRetType, opFun) <- return $ (opPolyDefArgTypes opPoly, opPolyDefRetType opPoly, opPolyDefEvalFun opPoly);
--                     -- add types to null values
--                     typedArgs <- return $ fmap (\(t, e) -> inferNullType t e) (zip opArgTypes typedArgs);
--                     -- typeCheck and add implicitCasts if neccessary
--                     -- 1. typeCheck
--                     -- 2. add implicit casts
--                     -- 3. add OpFun
--                     castedExprs <- addCasts opArgTypes typedArgs;
--                     return $ TypedExpr
--                         { typedExprTypeTag = opRetType
--                         , typedExprE = TApp op castedExprs (OpFun opFun)
--                         }
--                 })) (opDefPolyDefs opDef)
--             in case find isRight matches of
--                 -- if any of the op variant matches
--                 Just m -> m
--                 Nothing -> Left $ "Type mimatch for op " ++ (show op) ++ " tried:\n" ++ (
--                         intercalate "\n" $ fmap
--                             (\(err, opPoly) ->
--                                     "Candidate: " ++ (intercalate ", " $ fmap show $ opPolyDefArgTypes opPoly) ++ "\n"
--                                     ++ err
--                                 )
--                             (zip (lefts matches) (opDefPolyDefs opDef))
--                     )
--         }
--     where
--         splitErr :: [(Maybe TypedExpr, TypeTag, TypedExpr)] -> ([TypedExpr], [String])
--         splitErr [] = ([], [])
--         splitErr ((x, t, a):xs) =
--             let (l, r) = splitErr xs in
--                 case x of
--                     Just v -> (v:l, r)
--                     Nothing ->
--                         let errMsg = "Cast Error: " ++ (show $ typedExprTypeTag a) ++ " -> " ++ (show t) ++ " for " ++ (show $ typedExprE a) in
--                             (l, errMsg:r)
--         addCasts :: [TypeTag] -> [TypedExpr] -> Either String [TypedExpr]
--         addCasts toTypes args =
--             let res = fmap (\(t, a) -> (addCast a t, t, a)) (zip toTypes args)
--                 (vs, errs) = splitErr res
--             in
--                 if length errs == 0 then
--                     Right vs
--                 else
--                     Left $ intercalate "\n" errs

-- evalTypedExpr :: (String -> Val) -> TypedExpr -> Val
-- evalTypedExpr colValLookup expr =
--     case typedExprE expr of
--         TLit v -> v
--         TCol n -> colValLookup n
--         TApp _ exprs (OpFun f) ->
--             f $ fmap (evalTypedExpr colValLookup) exprs

-- unifyAppExpr op ctx args = do
--     opFns <- return $ lookUpFns ctx op
--     argTypes <- return $ _argTypes args
--     case _tryDirectMatch opFns argTypes of
--         Just fn -> return $ mkApp op fn args
--         Nothing ->
--             let argTypesCands = fmap (\(fnArgTypes, t -> intersect fnArgTypes (t:_getCasts t)) $ zip (getFnArgTypes opFns) argTypes

--             in do
--                 _searchMatch opFns argTypesCands
--     where
--         _searchMatch fns typeCands =
--             let maxIter = max $ fmap length typeCands
--             in
--                 _searchMatch' [] typeCands 0
--                 where
--                     _searchMatch' [] typeCands 0
