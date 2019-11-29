{-# LANGUAGE ScopedTypeVariables #-}
module Stage.Def
    ( Schema (..)
    , Row
    , Stage (..)
    , Cursor
    , curNext
    , curClose
    , processCursor
    , printCursor
    , cursorToList
    , RawCursor (..)
    ) where

import Data.List
import Control.Monad.State.Lazy

data Schema = Schema
    { schCols :: [String]
    , schName :: String
    }

type Row = [String]

data Stage = Stage
    { stgSchema :: Schema
    , stgNewCursor :: IO Cursor
    }

-- Cursor

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
        pr r = print $ foldl1 (++) $ intersperse ", " r

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
