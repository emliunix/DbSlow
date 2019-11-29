{-# LANGUAGE LambdaCase
           , ExistentialQuantification
#-}
module Expr.TypeInfer where

import Data.List (intercalate, sortBy, find)
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import Control.Monad

import Expr.Def
import Def

typeCheck :: (Maybe String -> String -> SqlType) -> (Op -> [SqlFnTuple]) -> SqlSimpleExpr -> Either String SqlExpr
typeCheck lookUpColType lookUpOp expr =
    case expr of
        SSimpleLit v -> return $ SqlExpr
            { sExprType = typeOfVal v
            , sExprE = SELit v
            }
        SSimpleCol ns col -> return $ SqlExpr
            { sExprType = lookUpColType ns col
            , sExprE = SECol ns col
            }
        SSimpleApp op args ->
            case partitionEithers $ fmap (typeCheck lookUpColType lookUpOp) args of
                ([], typedArgs) ->
                    let argTypes = fmap sExprType typedArgs
                        opFns = lookUpOp op
                    in
                        case unifyApp opFns argTypes of
                            Nothing -> Left $
                                "no variant of " ++ (show op) ++ " accepts types :: (" ++ reprTypes ++ ")\n" ++
                                "expr: " ++ (show expr)
                                where
                                    reprTypes = intercalate ", " $ fmap show argTypes
                            Just (_, argTypes, retType, fn) ->
                                case partitionEithers $ fmap _addCastIfNeccessary $ zip argTypes typedArgs of
                                    ([], args) -> Right $ SqlExpr
                                                            { sExprType = retType
                                                            , sExprE = SEApp op (OpFun fn) args
                                                            }
                                    (errors, _) -> Left $ (intercalate "\n" errors)
                (errors, _) -> Left $ intercalate "\n" errors
    where
        _addCastIfNeccessary (t, arg) =
            case sExprType arg of
                argType | argType == t -> Right arg
                STUnknown ->
                    -- this is a special case, STUnknown means it's Lit Null
                    -- and we can give it any type
                    Right $ SqlExpr
                        { sExprType = t
                        , sExprE = sExprE arg
                        }
                argType ->
                    let castOp = Cast argType t in
                        case lookUpOp castOp of
                            [] -> Left $ "Internal error: implicit cast from " ++
                                (show argType) ++ " to " ++ (show t) ++ " not implemented "
                            [(_, _, _, fn)] -> Right $ SqlExpr
                                            { sExprType = t
                                            , sExprE = SEApp castOp (OpFun fn) [arg]
                                            }

-- central definition of implicit casts, priority from high to low
-- and since sometimes, multiple casts have the same priority, the casts
-- are organized as [[]]
implicitCastTypes :: SqlType -> [[SqlType]]
implicitCastTypes STUnknown = [[STString, STInt, STDouble, STBool]]
implicitCastTypes STInt = [[STDouble]]
implicitCastTypes _ = []

-- Algorithm:
-- 1. filter fns with match argLen
-- 2. expandCands argTypes
-- 3. score fns with cands
-- 4. pick the lowest score one
-- 5. and there shouldn't be another fn with the same score (ambiguious)

unifyApp :: [SqlFnTuple] -> [SqlType] -> Maybe SqlFnTuple
unifyApp opFns argTs = do
    case _tryDirectMatch opFns argTs of
        Just fn -> Just $ fn
        Nothing ->
            let argLen = length argTs
                opFnsArgTs = filter (\ts -> length ts == argLen) $ fmap _opFnArgTs opFns
                tCands = _expandCands argTs
                opFnsOptScores = fmap (_scoreFn tCands) opFnsArgTs
                opFnsScores = catMaybes $ fmap (\(opFn, optScore) -> fmap (\score -> (opFn, score)) optScore) $ zip opFns opFnsOptScores
            in
                case sortBy (\(_, s1) (_, s2) -> compare s1 s2) opFnsScores of
                    [] -> Nothing
                    (fn, s1):rest -> case rest of
                        [] -> Just fn
                        (_, s2):_ -> if s1 == s2 then -- ambiguious fns found, return nothing
                                Nothing
                            else
                                Just fn

_expandCands :: [SqlType] -> [[[SqlType]]]
_expandCands argTs = fmap _expandCands1 argTs
_expandCands1 t = [t]:(implicitCastTypes t)

_scoreFn :: [[[SqlType]]] -> [SqlType] -> Maybe Int
_scoreFn tCands fnArgTs = fmap sum $ sequence $ fmap _scoreT $ zip tCands fnArgTs
    where
        _scoreT (cands, argT) = _scoreT' 0 cands argT
        _scoreT' s [] argT = Nothing
        _scoreT' s (c:cands) argT = if argT `elem` c then Just s else _scoreT' (s+1) cands argT

_tryDirectMatch :: [SqlFnTuple] -> [SqlType] -> Maybe SqlFnTuple
_tryDirectMatch opFns argTypes =
    if STUnknown `elem` argTypes then
        Nothing
    else
        let fnAndArgTs = zip opFns $ fmap _opFnArgTs opFns in
            fmap (\(fn, _) -> fn) $ find (\(_, fnArgTs) -> argTypes == fnArgTs) fnAndArgTs

_opFnArgTs :: SqlFnTuple -> [SqlType]
_opFnArgTs (_, argTs, _, _) = argTs
