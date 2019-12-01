{-# LANGUAGE FlexibleContexts #-}
module Stage.ProjStage
    ( mkProjStage
    ) where

import Control.Monad.Except (runExcept)
import Control.Monad.State.Lazy
import Def
    ( Stage (..)
    , Schema (..)
    , Row
    , Cursor
    , curNext
    , curClose
    , RawCursor (..)
    , FnLookUpCol
    )
import Expr.Def (SqlExpr)
import Expr.Eval (eval)

data ProjCursor = ProjCursor
    { projCurSrc :: Cursor
    , projCurFnLookUpCol :: FnLookUpCol
    , projCurExprs :: [SqlExpr]
    }

instance RawCursor ProjCursor where
    rawCurNext = do
        cur <- get
        processCur cur
        where
            processCur cur = do
                (r, srcCur') <- lift $ runStateT curNext srcCur
                put $ cur { projCurSrc = srcCur' }
                return $ fmap evalExprs r
                where
                    srcCur = projCurSrc cur
                    fnLookUpCol = projCurFnLookUpCol cur
                    exprs = projCurExprs cur
                    evalExprs row =
                        case runExcept $ mapM (evalExpr1 row) exprs of
                            Right vals -> vals
                    evalExpr1 row expr = eval fnLookUpCol row expr

    rawCurClose = return ()

mkProjStage :: FnLookUpCol -> [SqlExpr] -> Stage -> Stage
mkProjStage fnLookUpCol exprs srcStg = Stage
    { stgNewCursor = do
        srcCur <- stgNewCursor srcStg
        return $ toCursor ProjCursor 
            { projCurSrc = srcCur
            , projCurFnLookUpCol = fnLookUpCol
            , projCurExprs = exprs
            }
    }
