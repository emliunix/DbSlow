{-# LANGUAGE FlexibleContexts #-}
module Planner where

-- planner, MVP goals:
-- * convert SQL AST into Tree of Stages
-- * schema checking to bail out invalid SQL expressions
-- * (optional) neccessary conversions to support syntax-sugar like structure
-- ** sub-select

import Data.List (find, intercalate)
import Data.List.Index (ifind, indexed)
import Data.Maybe (catMaybes)
import Control.Monad ((>=>), when)
import Control.Monad.Except (Except, runExcept, throwError, liftEither)
import Control.Monad.State (State, evalState, get, modify)

import Expr.Def
import Def
import Expr.TypeInfer (typeCheck)

-- For Plan Tree

data PlanTableStore = PlanTableStore
    { planTables :: [(String, Schema)]
    }

data TempSchema = TempSchema
    { lookUpCol :: Maybe String -> String -> ExceptS (Int, SqlColumn)
    , toSchema :: Result Schema
    }

instance Show TempSchema where
    show s = "tempSchema"

class TempSchemaAble s where
    toTempSchema :: s -> TempSchema

instance TempSchemaAble Schema where
    toTempSchema s = TempSchema
        { lookUpCol = \optNs colName -> case optNs of
            Nothing -> case ifind (\_ col -> sColName col == colName) $ schCols s of
                Just icol -> return icol
                Nothing -> throwError $ "column not found: " ++ colName
            _ -> throwError "namespace not allowed"
        , toSchema = Right s
        }

instance TempSchemaAble PlanTableStore where
    toTempSchema s = TempSchema
        { lookUpCol = \optNs colName -> tblStoreLookUpCol s optNs colName
        , toSchema = Right $ Schema
                { schCols = newCols
                , schName = "tempSch"
                }
        }
        where
            newCols = concat $ fmap _applyNs $ planTables s
            _applyNs (ns, sch) =
                fmap (\col -> col { sColName = ns ++ "." ++ (sColName col) }) $ schCols sch

tblStoreLookUpCol :: PlanTableStore -> Maybe String -> String -> ExceptS (Int, SqlColumn)
tblStoreLookUpCol store optNs colName =
    case optNs of
        Just ns -> case ifind (\_ (ns', _) -> ns' == ns) $ tables of
            Just (i, nameSch) -> case findColInSch nameSch of
                Just (j, _, col) -> return (prefixLen i + j, col)
                    where
                Nothing -> throwError $ "column not found: " ++ (fmtColName optNs colName)
                
        Nothing -> case catMaybes $ fmap (\(i, e) -> fmap (\r -> (i, r)) $ findColInSch e) $ indexed tables of
            [] -> throwError $ "column not found" ++ (fmtColName optNs colName)
            [(i, (j, _, col))] -> return (prefixLen i + j, col)
            xs -> throwError $ "ambiguous columns found: " ++ (intercalate ", " $ fmap (\(_, (_, ns, _)) -> ns ++ "." ++ colName) xs)
    where
        tables = planTables store
        prefixLen i = sum $ fmap length $ take i tables
        findColInSch (name, sch) =
            fmap (\(i, col) -> (i, name, col)) $ ifind (\_ col -> sColName col == colName) $ schCols sch
        fmtColName optNs colName =
            case optNs of
                Just ns -> ns ++ "." ++ colName
                _ -> colName

data SqlPlan = SqlPlan
    { sPlanSchema :: TempSchema
    , sPlanP :: SqlPlan'
    } deriving (Show)

data SqlPlan'
    = SPProj          [SqlExpr] SqlPlan
    | SPFilter        SqlExpr SqlPlan
    | SPJoin          SqlExpr SqlPlan SqlPlan
    | SPSort          [(SqlColumn, SqlOrderBy)] SqlPlan
    | SPLimit         Integer SqlPlan
    | SPTable         String
    | SPSingletonExpr SqlExpr
    deriving (Show)

buildPlanTree :: [SqlClause] -> OpContext -> SqlTableRepo -> Result SqlPlan
buildPlanTree sel opCtx tblRepo =
    case extractSqlClauses sel of
        Left reason -> Left reason
        Right (sel, from, optWhere, optOrderBy, optLimit) ->
            buildPFrom opCtx from tblRepo >>= (
                foldl (>=>) return $ catMaybes
                    [ fmap (\clsWhere -> buildPWhere opCtx clsWhere) optWhere
                    , fmap (\clsOrderBy -> buildPOrderBy clsOrderBy) optOrderBy
                    , fmap (\clsLimit -> buildPLimit clsLimit) optLimit
                    , Just $ buildPSelect opCtx sel
                    ]
                )

extractSqlClauses :: [SqlClause] -> Result (SqlClause, SqlClause, Maybe SqlClause, Maybe SqlClause, Maybe SqlClause)
extractSqlClauses (sel:from:rest)
    | isSelect sel && isFrom from =
        let (optFilter, optOrderBy, optLimit) = processRest rest in
            Right (sel, from, optFilter, optOrderBy, optLimit)
    where
        processRest rest = case optGet rest [isFilter, isOrderBy, isLimit] of
            [optFilter, optOrderBy, optLimit] -> (optFilter, optOrderBy, optLimit)
        optGet [] [] = []
        optGet [] (t:ts) = Nothing : optGet [] ts
        optGet xs@(x:xs') (t:ts)
            | t x = Just x : optGet xs' ts
            | otherwise = Nothing : optGet xs ts
extractSqlClauses _ = Left "invalid sql"

-- buildTableQuery
-- buildSelectQuery

buildSelectSingleQuery :: [SqlClause] -> Maybe (Result SqlPlan)
buildSelectSingleQuery [SClsSelect exprs] = Just $ Left "not implemented"
buildSelectSingleQuery _ = Nothing

buildPFrom :: OpContext -> SqlClause -> SqlTableRepo -> Result SqlPlan
-- buildPFrom opCtx clsFrom tblRepo = Left "xx"
buildPFrom opCtx clsFrom tblRepo =
    runExcept $ do
        fromSpec <- _fromSpec clsFrom 
        case fromSpec of
            SFromTable tbl optBinding -> _fromTbl tbl optBinding
            SFromSubSelect _ _ -> throwError "from sub-select not supported yet"
            SFromJoin _ _ _ -> throwError "from join not supported yet"
    where
        _fromSpec cls = case cls of
            SClsFrom fromSpec -> return fromSpec
            _ -> throwError "Internal Error: not a from clause"
        _fromTbl :: String -> Maybe String -> ExceptS SqlPlan
        _fromTbl tbl optBinding = do
            case lookUpTable tblRepo tbl of
                Nothing -> throwError $ "Table not found: " ++ tbl
                Just tblDef -> return $ SqlPlan
                                        { sPlanSchema = toTempSchema $ tblSchema tblDef
                                        , sPlanP = SPTable tbl
                                        }

buildPWhere :: OpContext -> SqlClause -> SqlPlan -> Result SqlPlan
-- buildPWhere _ _ _ = Left "xx"
buildPWhere opCtx cls src =
    runExcept $ do
        expr <- _expr cls
        -- we can pre-check the existence of column refs in expr
        -- or we can do typeCheck directly and let it fail in the check
        -- exprCols <- return $ colsOfExpr expr
        -- colDefs <- mapM (\(optNs, col) -> liftEither $ schLookUpCol optNs col) exprCols
        exprTyped <- liftEither $ typeCheck schLookUpCol (lookUpOp opCtx) expr
        when (sExprType exprTyped /= STBool) $
            throwError $ "Where expression is not of type bool: " ++ (show expr)
        return $ SqlPlan
            { sPlanSchema = sPlanSchema src
            , sPlanP = SPFilter exprTyped src
            }
    where
        schLookUpCol optNs colName = fmap snd $ (lookUpCol $ sPlanSchema src) optNs colName
        _expr cls =
            case cls of
                SClsWhere expr -> return expr
                _ -> throwError "Internal Error: not a where clause"

buildPOrderBy :: SqlClause -> SqlPlan -> Result SqlPlan
buildPOrderBy _ _ = Left "xx"
-- buildPOrderBy _ _ = Left "xx"

buildPLimit :: SqlClause -> SqlPlan -> Result SqlPlan
buildPLimit cls src =
    runExcept $ do
        lim <- _lim cls
        return $ SqlPlan
            { sPlanSchema = sPlanSchema src
            , sPlanP = SPLimit lim src
            }
    where
        _lim (SClsLimit n) = return n
        _lim _ = throwError "Internal Error: not a limit clause"

buildPSelect :: OpContext -> SqlClause -> SqlPlan -> Result SqlPlan
-- buildPSelect _ _ _ = Left "xx"
buildPSelect opCtx cls src =
    runExcept $ do
        sels <- _sels cls
        selNames <- return $ evalState (_selNames sels) 0
        -- selTypes <- mapM (\(optNs, colN) -> liftEither $ schLookUpCol optNs colN) sels
        selExprsTyped <- mapM
            (\expr -> liftEither $ typeCheck schLookUpCol (lookUpOp opCtx) expr)
            $ fmap snd sels
        selTypes <- return $ fmap sExprType selExprsTyped
        return $ SqlPlan
            { sPlanSchema = toTempSchema $ mkSchema selNames selTypes
            , sPlanP = SPProj selExprsTyped src
            }
    where
        mkSchema names types = Schema
            { schCols = fmap mkCol $ zip names types
            , schName = "select"
            }
        mkCol (n, t) = SqlColumn
            { sColName = n
            , sColType = t
            }
        schLookUpCol optNs colName = fmap snd $ (lookUpCol $ sPlanSchema src) optNs colName
        _sels :: SqlClause -> ExceptS [(Maybe String, SqlSimpleExpr)]
        _sels cls = case cls of
            SClsSelect sels -> return $ sels
            _ -> throwError "Internal Error: not a select clause"
        -- Select Names:
        -- cases
        -- 1. renaming exists -> use the renaming
        -- 2. col ref -> the name of col
        -- 3. expr -> auxName eg. expr1
        _selNames :: [(Maybe String, SqlSimpleExpr)] -> State Int [String]
        _selNames sels = mapM _selName1 sels
        _selName1 (optName, expr) = do
            case optName of
                Just name -> return name
                _ -> case _tryDirectColRef expr of
                    Just name -> return name
                    _ -> do
                        i <- get
                        modify (+1)
                        return $ "expr_" ++ (show i)
            where
                _tryDirectColRef (SSimpleCol optNs colName) =
                    Just $ case optNs of
                        Just ns -> ns ++ "_" ++ colName
                        _ -> colName
                _tryDirectColRef _ = Nothing
