module Test where

import Expr.TypeInfer
import Expr.Def
import Def
import PrimOps
import Control.Monad.Except
import Planner

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

testTable = Schema
    { schName = "test"
    , schCols =
        [ SqlColumn { sColName = "dub", sColType = STDouble }
        , SqlColumn { sColName = "str", sColType = STString }
        , SqlColumn { sColName = "int", sColType = STInt }
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
        [ (Just "aa", SSimpleCol Nothing "dub")
        , (Just "bb", SSimpleCol Nothing "str")
        , (Just "cc", SSimpleCol Nothing "int")
        ]
    , SClsFrom $ SFromTable "test" (Just "t1")
    ]

-- >>> buildPlanTree testSelClause primOpContext tblRepo
