{-# LANGUAGE ScopedTypeVariables #-}
module Def
    ( Result
    , ExceptS
    -- Base Defs
    , SqlColumn (..)
    , Schema (..)
    , SqlTable (..)
    , SqlTableRepo (..)
    , Row
    , FnLookUpCol
    -- SQL Surface AST
    , SqlSimpleExpr (..)
    , colsOfExpr
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
    -- Stage and Cursor
    , Stage (..)
    , Cursor
    , curNext
    , curClose
    , RawCursor (..)
    , processCursor
    , printCursor
    , cursorToList
    , module Expr.Def
    ) where

import Data.List (intersperse)
import Control.Monad.State.Lazy
import Control.Monad.Except (Except)

import Expr.Def
    ( Op (..)
    , SqlVal (..)
    , SqlType (..)
    )

type Result = Either String

type ExceptS = Except String

data SqlColumn = SqlColumn
    { sColName :: String
    , sColType :: SqlType
    } deriving (Show)

data Schema = Schema
    { schCols :: [SqlColumn]
    , schName :: String
    } deriving (Show)

data SqlTable = SqlTable
    { tblName :: String
    , tblSchema :: Schema
    , tblStage :: Stage
    }

data SqlTableRepo = SqlTableRepo { lookUpTable :: String -> Maybe SqlTable, addTable :: SqlTable -> SqlTableRepo }

type Row = [SqlVal]

type FnLookUpCol = Maybe String -> String -> ExceptS (Int, SqlColumn)

-- Stage Definitions

data Stage = Stage { stgNewCursor :: IO Cursor }

data Cursor = Cursor
    { curNext' :: StateT Cursor IO (Maybe Row)
    , curClose' :: StateT Cursor IO ()
    }

curNext :: StateT Cursor IO (Maybe Row)
curNext = do
    cn <- get
    (curNext' cn)

curClose :: StateT Cursor IO ()
curClose = do
    cn <- get
    (curClose' cn)

-- Cursor Utility

processCursor :: (Row -> IO ()) -> Cursor -> IO ()
processCursor act cur = evalStateT processCursor' cur
    where
        processCursor' = do
            r <- curNext
            case r of
                Just r -> do
                    lift $ act r
                    processCursor'
                Nothing -> return ()

printCursor :: Cursor -> IO ()
printCursor c = processCursor pr c
    where
        pr r = print $ foldl1 (++) $ intersperse ", " $ fmap show r

cursorToList :: Cursor -> IO [Row]
cursorToList cur = evalStateT (cursorToList' []) cur
    where
        cursorToList' out = do
            r <- curNext
            case r of
                Just r -> do
                    cursorToList' (r:out)
                Nothing -> return $ reverse out

-- Raw Cursor

class RawCursor c where
    rawCurNext :: StateT c IO (Maybe Row)
    rawCurClose :: StateT c IO ()
    toCursor :: c -> Cursor
    toCursor c = Cursor (_wrap rawCurNext) (_wrap rawCurClose)
        where
            _wrap :: StateT c IO t -> StateT Cursor IO t
            _wrap f = do
                (r, c') <- lift $ runStateT f c
                put $ toCursor c'
                return r

-- For parser

data SqlSimpleExpr
    = SSimpleLit SqlVal
    | SSimpleCol (Maybe String) String
    | SSimpleApp Op [SqlSimpleExpr]
    deriving (Show)

data SqlStmt
    = SStmtAttach String String -- attach tblCsv csv 'path/to/data.csv'
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

-- # Extract Column References From SimpleExpr #

colsOfExpr :: SqlSimpleExpr -> [(Maybe String, String)]
colsOfExpr expr =
    case expr of
        SSimpleLit _ -> []
        SSimpleCol optNs col -> [(optNs, col)]
        SSimpleApp _ exprs -> concat $ fmap colsOfExpr exprs
