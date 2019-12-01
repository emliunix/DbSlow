module Expr.Eval where

import Expr.Def
import Def

eval :: FnLookUpCol -> Row -> SqlExpr -> ExceptS SqlVal
eval lookUpCol row expr =
    eval' expr'
    where
        eType = sExprType expr
        expr' = sExprE expr
        eval' (SELit v) = return v
        eval' (SECol optNs colName) = do
            (idx, _) <- lookUpCol optNs colName
            return $ row !! idx
        eval' (SEApp _ (OpFun fn) exprs) = do
            vals <- mapM (eval lookUpCol row) exprs
            return $ fn vals
