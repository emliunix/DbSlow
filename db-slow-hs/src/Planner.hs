module Planner where

-- planner, MVP goals:
-- * convert SQL AST into Tree of Stages
-- * schema checking to bail out invalid SQL expressions
-- * (optional) neccessary conversions to support syntax-sugar like structure
-- ** sub-select

import Expr.Def
import Def

-- Parser -> (SqlExpr, SqlQuery)

type Schema = String
type SqlSimpleExpr = String
type SqlCol = String
type Result t = Either String t

data SqlTable = SqlTable
    { sTblName :: String
    , sTblSchema :: Schema
    }

-- For Plan Tree

data SqlTableRepo = SqlTableRepo { lookUpTable :: String -> SqlTable }

data SqlPlan
    = SPProj Schema SqlExpr SqlPlan
    | SPFilter Schema SqlExpr SqlPlan
    | SPJoin Schema SqlExpr SqlPlan SqlPlan
    | SPSort Schema [(SqlCol, SqlOrderBy)] SqlPlan
    | SPLimit Schema Int SqlPlan
    | SPSingletonExpr Schema SqlExpr

buildPlanTree :: [SqlClause] -> Result SqlPlan
buildPlanTree _ = Left "not implemented"

extractSqlClauses :: [SqlClause] -> Result (SqlClause, SqlClause, Maybe SqlClause, Maybe SqlClause, Maybe SqlClause)
extractSqlClauses (sel:from:rest)
    | isSelect sel && isFrom from =
        let (optFilter, optOrderBy, optLimit) = processRest rest in Right (sel, from, optFilter, optOrderBy, optLimit)
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

-- checkQuerySchema :: SqlQuery