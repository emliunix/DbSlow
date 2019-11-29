module Test where

import Expr.TypeInfer
import Expr.Def
import Def
import PrimOps

primLookUpOp = lookUpOp primOpContext

testExpr :: SqlSimpleExpr
testExpr = SSimpleApp Plus [(SSimpleCol Nothing "dub"), (SSimpleLit $ SVInt 3)]

colTypes _ col = case col of
    "dub" -> STDouble
    "str" -> STString
    "int" -> STInt

-- >>> typeCheck colTypes primLookUpOp testExpr
-- Right (SqlExpr {sExprType = STDouble, sExprE = SEApp Plus fun [SqlExpr {sExprType = STDouble, sExprE = SECol Nothing "dub"},SqlExpr {sExprType = STDouble, sExprE = SEApp (Cast STInt STDouble) fun [SqlExpr {sExprType = STInt, sExprE = SELit (SVInt 3)}]}]})
--
