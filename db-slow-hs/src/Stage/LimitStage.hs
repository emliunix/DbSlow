module Stage.LimitStage
    (
    -- mkLimitStage
    ) where

-- import Stage.Def
--     ( Stage(..)
--     , Schema(..)
--     , Cursor
--     , curNext
--     , curClose
--     , RawCursor (..)
--     )
-- import Control.Monad.State.Lazy
    
-- data LimitCursor = LimitCursor { limCurRemain :: Int, limCurSrc :: Cursor }

-- instance RawCursor LimitCursor where
--     rawCurNext = do
--         x <- get
--         if limCurRemain x == 0 then do
--             return Nothing
--         else
--             tryTakeOne
--         where
--             tryTakeOne = do
--                 x <- get
--                 (r, curSrc') <- lift $ runStateT curNext (limCurSrc x)
--                 case r of
--                     Just _ -> do
--                         put $ x { limCurRemain = (limCurRemain x) - 1, limCurSrc = curSrc' }
--                         return r
--                     Nothing -> do
--                         put $ x { limCurRemain = 0, limCurSrc = curSrc' }
--                         return r
--     rawCurClose = do
--         x <- get
--         (_, curSrc') <- lift $ runStateT curClose (limCurSrc x)
--         put x { limCurSrc = curSrc' }


-- mkLimitStage :: Int -> Stage -> Stage
-- mkLimitStage lim srcStg = Stage
--         { stgSchema = stgSchema srcStg
--         , stgNewCursor = do
--             src <- stgNewCursor srcStg
--             return $ toCursor LimitCursor { limCurRemain = lim, limCurSrc = src }
--         }

-- >>> printCursor $ toCursor LimitCursor { limCurRemain = 3, limCurSrc = toCursor TestCursor { testIdx = 3 } }
-- "3"
-- "4"
-- "5"
--
-- 