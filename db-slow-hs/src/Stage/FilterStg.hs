module Stage.FilterStg
    ( mkFilterStage
    ) where

import Control.Monad.State.Lazy
import Stage.Def
    ( Stage(..)
    , Schema(..)
    , Row
    , Cursor
    , curNext
    , curClose
    , RawCursor (..)
    )

type Expr = String

data FilterCursor = FilterCursor
    { filCurSrc :: Cursor
    , filCurSrcSchema :: Schema
    , filCurTestExpr :: Expr
    }

data SqlVal
    = SqlBool Bool
    | SqlInt Int
    | SqlString String

evalExpr :: Expr -> Schema -> Row -> SqlVal
evalExpr _ _ _ = SqlBool True

isTrue :: SqlVal -> Bool
isTrue (SqlBool v) = v
isTrue _ = False

instance RawCursor FilterCursor where
    rawCurNext = do
        cur <- get
        srcCur <- return $ filCurSrc cur
        srcSchema <- return $ filCurSrcSchema cur
        testExpr <- return $ filCurTestExpr cur
        (r, srcCur') <- lift $ _nextMatch srcCur srcSchema testExpr
        put $ cur { filCurSrc = srcCur' }
        return r
        where
            _nextMatch cur schema testExpr = runStateT _nextMatch' cur
                where
                    _nextMatch' = do
                        row <- curNext
                        case row of
                            Just row | isTrue $ evalExpr testExpr schema row -> return $ Just row
                            Just _ -> _nextMatch'
                            Nothing -> return Nothing

    rawCurClose = return ()

mkFilterStage :: Expr -> Stage -> Stage
mkFilterStage expr srcStg = Stage
                            { stgSchema = stgSchema srcStg
                            , stgNewCursor = do
                                srcSch <- return $ stgSchema srcStg
                                srcCur <- stgNewCursor srcStg
                                return $ toCursor FilterCursor 
                                                    { filCurSrc = srcCur
                                                    , filCurSrcSchema = srcSch
                                                    , filCurTestExpr = expr
                                                    }
                            }
