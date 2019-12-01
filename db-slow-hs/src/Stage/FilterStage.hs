{-# LANGUAGE FlexibleContexts #-}
module Stage.FilterStage
    ( mkFilterStage
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
import Expr.Def (SqlExpr, SqlVal (..))
import Expr.Eval (eval)

data FilterCursor = FilterCursor
    { filCurSrc :: Cursor
    , filCurFnLookUpCol :: FnLookUpCol
    , filCurTestExpr :: SqlExpr
    }

instance RawCursor FilterCursor where
    rawCurNext = do
        cur <- get
        processCur cur
        where
            processCur cur = do
                (r, srcCur') <- lift $ _nextMatch srcCur fnLookUpCol testExpr
                put $ cur { filCurSrc = srcCur' }
                return r
                where
                    srcCur = filCurSrc cur
                    fnLookUpCol = filCurFnLookUpCol cur
                    testExpr = filCurTestExpr cur
            _nextMatch cur fnLookUpCol testExpr = runStateT _nextMatch' cur
                where
                    _nextMatch' = do
                        row <- curNext
                        case row of
                            Just row | _isEvalToTrue row ->
                                return $ Just row
                            Just _ -> _nextMatch'
                            Nothing -> return Nothing
                    _isEvalToTrue row = 
                        let ret = runExcept $ eval fnLookUpCol row testExpr
                        in ret == (Right $ SVBool True)

    rawCurClose = return ()

mkFilterStage :: FnLookUpCol -> SqlExpr -> Stage -> Stage
mkFilterStage fnLookUpCol expr srcStg = Stage
    { stgNewCursor = do
        srcCur <- stgNewCursor srcStg
        return $ toCursor FilterCursor 
            { filCurSrc = srcCur
            , filCurFnLookUpCol = fnLookUpCol
            , filCurTestExpr = expr
            }
    }
