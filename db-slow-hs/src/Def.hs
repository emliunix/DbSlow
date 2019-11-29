module Def
    ( SqlSimpleExpr (..)
    , SqlStmt (..)
    , SqlClause (..)
    , isSelect
    , isFrom
    , isFilter
    , isOrderBy
    , isLimit
    , SqlFromSpec (..)
    , SqlOrderBy (..)
    , SqlJoinType (..)
    , module Expr.Def
    ) where

import Expr.Def
    ( Op (..)
    , SqlVal (..)
    )

-- For parser

data SqlSimpleExpr
    = SSimpleLit SqlVal
    | SSimpleCol (Maybe String) String
    | SSimpleApp Op [SqlSimpleExpr]
    deriving (Show)

data SqlStmt
    = SStmtAttach String (Maybe String) -- attach 'path/to/data.csv' [AS tblCsv]
    | SStmtTable String -- table someTbl
    | SStmtSingleSelect SqlClause -- single SClsSelect exprs statement
    | SStmtSelect [SqlClause] -- select
    deriving (Show)

data SqlClause
    = SClsSelect [(Maybe String, SqlSimpleExpr)]
    | SClsFrom SqlFromSpec
    | SClsWhere SqlSimpleExpr
    | SClsOrderBy [(String, SqlOrderBy)]
    | SClsLimit Integer
    deriving (Show)

isSelect (SClsSelect _) = True
isSelect _ = False
isFrom (SClsFrom _) = True
isFrom _ = False
isFilter (SClsWhere _) = True
isFilter _ = False
isOrderBy (SClsOrderBy _) = True
isOrderBy _ = False
isLimit (SClsLimit _) = True
isLimit _ = False

data SqlFromSpec
    = SFromTable String (Maybe String)
    | SFromSubSelect SqlStmt String -- must be SStmtSelect
    | SFromJoin SqlJoinType SqlFromSpec SqlFromSpec
    deriving (Show)

data SqlOrderBy
    = SOrderASC
    | SOrderDESC
    deriving (Eq, Show)

data SqlJoinType
    = SJInner
    | SJLeft
    | SJRight
    | SJOuter
    deriving (Eq, Show)
