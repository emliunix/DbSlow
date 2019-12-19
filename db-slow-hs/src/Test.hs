module Test where

import Expr.TypeInfer
import Expr.Def
import Def
import PrimOps
import Control.Monad.Except
import Planner

import Stage.TestDataStage (mkTestDataStage)
import Stage.BuildStage (buildStage)
import Parser (parseSelect, parseExpr, parseFactor, parseCol, mkOpParser, addLeftBinaryParser, parseAttach)
import Text.Trifecta
import Data.List (find)

-- type ExceptS = Except String

xxx :: ExceptS Int
xxx = do
    throwError "someError"
    return 3

-- >>> runExcept xxx
-- Left "someError"
--

primLookUpOp = lookUpOp primOpContext

testExpr :: SqlSimpleExpr
testExpr = SSimpleApp Plus [(SSimpleCol Nothing "dub"), (SSimpleLit $ SVInt 3)]

lookUpCol _ col = case col of
    "dub" -> Right $ SqlColumn { sColName = "dub", sColType = STDouble }
    "str" -> Right $ SqlColumn { sColName = "str", sColType = STString }
    "int" -> Right $ SqlColumn { sColName = "int", sColType = STInt }
    _ -> Left "column not found"

-- >>> typeCheck lookUpCol primLookUpOp testExpr
-- Right (SqlExpr {sExprType = STDouble, sExprE = SEApp Plus fun [SqlExpr {sExprType = STDouble, sExprE = SECol Nothing "dub"},SqlExpr {sExprType = STDouble, sExprE = SEApp (Cast STInt STDouble) fun [SqlExpr {sExprType = STInt, sExprE = SELit (SVInt 3)}]}]})
--

testTableSch = Schema
    { schName = "test"
    , schCols =
        [ SqlColumn { sColName = "a", sColType = STDouble }
        , SqlColumn { sColName = "b", sColType = STString }
        , SqlColumn { sColName = "c", sColType = STInt }
        ]
    }

testTable = SqlTable
    { tblName = "test"
    , tblSchema = testTableSch
    , tblStage = mkTestDataStage
        [ [SVDouble 1.1, SVString "strval1", SVInt 1]
        , [SVDouble 2.2, SVString "strval1", SVInt 2]
        , [SVDouble 3.3, SVString "strval1", SVInt 3]
        ]
    }

tblRepo = mkTblRepo [testTable]
mkTblRepo tbls = SqlTableRepo
    { lookUpTable = \tbl -> find (\t -> tblName t == tbl) tbls
    , addTable = \tbl -> mkTblRepo (tbl:tbls)
    }

-- buildPlanTree :: [SqlClause] -> OpContext -> SqlTableRepo -> Result SqlPlan

testSelClause =
    [ SClsSelect
        [ (Just "aa", SSimpleCol Nothing "a")
        , (Just "bb", SSimpleCol Nothing "b")
        , (Just "cc", SSimpleCol Nothing "c")
        ]
    , SClsFrom $ SFromTable "test" (Just "t1")
    ]

myTest :: String -> IO ()
myTest sql =
    case stage of
        Right stg -> do
            cur <- stgNewCursor stg
            printCursor cur
        Left msg -> putStrLn msg
    where
        stage = runExcept $ case parseString parseSelect mempty sql of
            Success (SStmtSelect clauses) -> do
                plan <- liftEither $ buildPlanTree clauses primOpContext tblRepo
                buildStage tblRepo plan

-- >>> myTest "select a, b, c from test"
-- "SVDouble 1.1, SVString \"strval1\", SVInt 1"
-- "SVDouble 2.2, SVString \"strval1\", SVInt 2"
-- "SVDouble 3.3, SVString \"strval1\", SVInt 3"
--

-- >>> myTest "select a, c from test where a > 1.1"
-- "SVDouble 2.2, SVInt 2"
-- "SVDouble 3.3, SVInt 3"
--

-- >>> parseString parseSelect mempty "select a, c from test where a > 1"
-- Success (SStmtSelect [SClsSelect [(Nothing,SSimpleCol Nothing "a"),(Nothing,SSimpleCol Nothing "c")],SClsFrom (SFromTable "test" Nothing),SClsWhere (SSimpleCol Nothing "a")])
--

-- >>> parseString parseSelect mempty "select a > 1"
-- Success (SStmtSelect [SClsSelect [(Nothing,SSimpleCol Nothing "a")]])
--

-- >>> parseString parseExpr mempty "a > 1"
-- Success (SSimpleCol Nothing "a")
--
-- >>> parseString parseSelect mempty "SelEct 1 + fun(1, 2, 3) as t"
-- >>> parseString parseSelect mempty "sELect 1 + 1"
-- >>> parseString parseSelect mempty "select t.a from b as t"
-- >>> parseString parseSelect mempty "select * from a where c > 0 and b < 1 order by d limit 100"
-- >>> parseString parseFrom mempty "from a as t"
-- >>> parseString parseFrom mempty "from (select t.a from b as t) AS t1"
-- >>> parseString parseOrderBy mempty "order by a.t asc, b.t desc"
-- >>> parseString parseExpr mempty "c > 0 and b < 1 order"
-- >>> parseString ((try $ keyword "or") <|> return "xx") mempty "order"

-- >>> parseString ((symbol ">=") <|> (symbol ">")) mempty ">"
-- Success ">"

-- >>> parseString parseExpr mempty "a > 1"
-- Success (SSimpleApp GreaterThan [SSimpleCol Nothing "a",SSimpleLit (SVInt 1)])
--
-- >>> parseString parseAttach mempty "attach tbl_1 csv '/path/to/data.csv'"
-- Success (SStmtAttach "tbl_1" "/path/to/data.csv")
--
