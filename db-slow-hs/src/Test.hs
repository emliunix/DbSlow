module Test where

import Expr.TypeInfer
import Expr.Def
import Def
import PrimOps
import Control.Monad.Except
import Planner

import Stage.TestDataStage (mkTestDataStage)
import Stage.BuildStage (buildStage)
import Parser (parseSelect)
import Text.Trifecta

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

tblRepo = SqlTableRepo
    { lookUpTable = \tbl -> case tbl of
        "test" -> Just testTable
        _ -> Nothing
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

-- >>> myTest "select a, c from test where a > 1"
-- Where expression is not of type bool: SSimpleCol Nothing "a"
--

-- >>> parseString parseSelect mempty "select a, c from test where a > 1"
-- Success (SStmtSelect [SClsSelect [(Nothing,SSimpleCol Nothing "a"),(Nothing,SSimpleCol Nothing "c")],SClsFrom (SFromTable "test" Nothing),SClsWhere (SSimpleCol Nothing "a")])
--
