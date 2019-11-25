{-# LANGUAGE ExistentialQuantification
           , RankNTypes
           , MultiParamTypeClasses
           , OverloadedStrings
           , ScopedTypeVariables
#-}

module Stage2 where

import Data.List
import Data.Maybe
import qualified Data.Csv as Csv
import qualified System.IO as SIO
import qualified System.Directory as SDir
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
-- import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy

-- ## Stages
-- * CsvFile
-- * Filter
-- * Projection
-- * Sort
-- * MergeSorted

-- ## Definitions

data Schema = Schema
    { schCols :: [String]
    , schName :: String
    }

type Row = Vector ByteString

data Stage = Stage
    { stgSchema :: Schema
    , stgNewCursor :: Cursor
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
        pr r = print $ BS.concat $ intersperse ", " $ V.toList r

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

-- # Test Cursor #

data TestCursor = TestCursor { testIdx :: Int }

instance RawCursor TestCursor where
    rawCurNext = do
        x <- get
        put $ x { testIdx = 1 + (testIdx x) }
        return $ Just $ V.singleton (BS8.pack $ show $ testIdx x)
    rawCurClose = return ()

-- test program

testCur = runStateT testRun $ toCursor $ TestCursor 0
    where
        testRun = do
            a <- curNext
            b <- curNext
            c <- curNext
            lift $ print $ foldl1 (++) $ fmap show [a, b, c]

-- >>> testCur
-- "Just [\"0\"]Just [\"1\"]Just [\"2\"]"
--

-- # Simple File Cursor #

data SimpleFileCursor = SimpleFileCursor { fileIdx :: Int }

instance RawCursor SimpleFileCursor where
    rawCurNext = do
        -- update cursor
        x <- get
        put $ x { fileIdx = (fileIdx x) + 1 }
        -- read data
        let _pfix = fileIdx x 
            fpath = "data/part-" ++ (show _pfix)
            in lift $ do
                exists <- SDir.doesFileExist fpath
                if exists then
                    fmap (Just . V.singleton . BS8.pack) $ readFile fpath
                else
                    return Nothing

    -- Nothing to close
    rawCurClose = return ()

-- >>> printCursor $ toCursor SimpleFileCursor { fileIdx = 0 }
-- "xxxx"
-- "yyyyyy"
--

data LimitCursor = LimitCursor { limCurRemain :: Int, limCurSrc :: Cursor }

instance RawCursor LimitCursor where
    rawCurNext = do
        x <- get
        if limCurRemain x == 0 then do
            -- explicit close when finalized
            rawCurClose
            return Nothing
        else
            tryTakeOne
        where
            tryTakeOne = do
                x <- get
                (r, curSrc') <- lift $ runStateT curNext (limCurSrc x)
                case r of
                    Just _ -> do
                        put $ x { limCurRemain = (limCurRemain x) - 1, limCurSrc = curSrc' }
                        return r
                    Nothing -> do
                        put $ x { limCurRemain = 0, limCurSrc = curSrc' }
                        return r
    rawCurClose = do
        x <- get
        (_, curSrc') <- lift $ runStateT curClose (limCurSrc x)
        put x { limCurSrc = curSrc' }

-- >>> printCursor $ toCursor LimitCursor { limCurRemain = 3, limCurSrc = toCursor TestCursor { testIdx = 3 } }
-- "3"
-- "4"
-- "5"
--

-- # SimpleJoin
data SimpleJoinCursor = SimpleJoinCursor { simpJoinCurLeft :: Cursor, simpJoinCurRight :: Cursor, simpJoinClosed :: Bool }

instance RawCursor SimpleJoinCursor where
    rawCurNext = do
        x <- get
        let curLeft = simpJoinCurLeft x
            curRight = simpJoinCurRight x
            closed = simpJoinClosed x
            in if closed then
                    return Nothing
                else do
                    (vLeft, curLeft') <- lift $ runStateT curNext curLeft
                    (vRight, curRight') <- lift $ runStateT curNext curRight
                    case (vLeft, vRight) of
                        (Nothing, _) -> do
                            (_, curRight') <- lift $ runStateT curClose curRight'
                            _updateCursors x curLeft' curRight' True
                            return Nothing
                        (_, Nothing) -> do
                            (_, curLeft') <- lift $ runStateT curClose curLeft'
                            _updateCursors x curLeft' curRight' True
                            return Nothing
                        (Just v1, Just v2) -> do
                            _updateCursors x curLeft' curRight' False
                            return $ Just $ v1 V.++ v2
        where
            _updateCursors :: SimpleJoinCursor -> Cursor -> Cursor -> Bool -> StateT SimpleJoinCursor IO ()
            _updateCursors cur curLeft' curRight' closed =
                put cur
                    { simpJoinCurLeft = curLeft'
                    , simpJoinCurRight = curRight'
                    , simpJoinClosed = closed
                    }

    rawCurClose = do
        x <- get
        let curLeft = simpJoinCurLeft x
            curRight = simpJoinCurRight x
            in do
                (_, curLeft') <- lift $ runStateT curClose curLeft
                (_, curRight') <- lift $ runStateT curClose curRight
                put x
                    { simpJoinCurLeft = curLeft'
                    , simpJoinCurRight = curRight'
                    }

testJoinCur :: Cursor
testJoinCur = toCursor SimpleJoinCursor
    { simpJoinCurLeft = toCursor LimitCursor
                                    { limCurRemain = 7
                                    , limCurSrc = toCursor TestCursor { testIdx = 3 }
                                    }
    , simpJoinCurRight = toCursor SimpleJoinCursor
                                    { simpJoinCurLeft = toCursor TestCursor { testIdx = 5 }
                                    , simpJoinCurRight = toCursor TestCursor { testIdx = 7 }
                                    , simpJoinClosed = False
                                    }
    , simpJoinClosed = False
    }

-- >>> printCursor testJoinCur
-- "3, 5, 7"
-- "4, 6, 8"
-- "5, 7, 9"
-- "6, 8, 10"
-- "7, 9, 11"
-- "8, 10, 12"
-- "9, 11, 13"
--


-- # Simple CSV Stage #

-- data StageCsvFile = StageCsvFile
--     { csvPath :: String
--     , csvSchema :: Schema
--     }

-- data CursorCsvFile = CursorCsvFile
--     { csvCurData :: Vector (Vector ByteString)
--     , csvCurIdx :: Int
--     , csvCurClosed :: Bool
--     }

-- instance Cursor CursorCsvFile where
--     curNext = do
--         c <- get
--         if csvCurClosed c then 
--             return Nothing
--         else
--             let idx = csvCurIdx c
--             in do
--                 put $ c { csvCurIdx = 1 + idx }
--                 case (csvCurData c) V.!? idx of
--                     Just v -> return $ Just v
--                     Nothing -> do
--                         put $ c { csvCurClosed = True }
--                         return Nothing

--     curClose = return ()

-- createCsvCursor :: FilePath -> IO CursorCsvFile
-- createCsvCursor fpath = do
--     d <- BS.readFile fpath
--     case Csv.decode Csv.HasHeader d of
--         Right v -> return $ CursorCsvFile
--             { csvCurData = v
--             , csvCurIdx = 0
--             , csvCurClosed = False
--             }
--         Left reason -> fail reason

-- testCsv :: IO ()
-- testCsv = do
--     cur <- createCsvCursor "data/test.csv"
--     printCursor cur

-- >>> testCsv
-- "Processing Row"
-- "John27"
-- "Processing Row"
-- "Jane28"
-- "End Processing"
--

-- # Filter Stage #

-- data StageFilter = StageFilter
--     { sFilterSchame :: Schema
--     , sFilterSrc :: forall s. Stage s => s
--     , sFilterTestFn :: Row -> Bool
--     }

-- data CursorFilter = CursorFilter
--     { curFilterSrc :: forall c. Cursor c => c
--     , curFilterTestFn :: Row -> Bool
--     }

-- instance Cursor CursorFilter where
--     curNext = do
--         c <- get
--         tmp <- lift $ runStateT curNext (curFilterSrc c)
--         return Nothing
--         -- case tmp of
--         --     (r, (src' :: forall c. Cursor c => c)) -> do
--         --         put $ c { curFilterSrc = src' }
--         --         return Nothing
--     curClose = return ()

-- inc :: State Int ()
-- inc = do
--     modify ((+1) :: Int -> Int)

-- test :: State Int Int
-- test = do
--     inc
--     x <- get
--     return $ x + 3

-- for a tree of states (Tree of Cursors)
-- (State Cursor) is kind of Global

-- 1. Cursor itself is of tree structure
-- then how to simulate sub cursor?
-- run (IO, Cusor a)
--     run (IO Cursor sub-1)
--     run (IO Cursor sub-2)
-- end
-- need to test how to combine State Monad and IO Monad

-- 1.1. experiments results
-- to construct the tree strucutre with method 1:
-- with Moand m => StateT Cursor m,
-- (1) we now have states
-- (2) for sub-cursor, (runStateT op sub_cursor) makes it a IO,
-- and (lift $ ...) makes it part of the StateT. So requirements met.

test2 :: StateT Int IO ()
test2 = do
    modify ((+1) :: Int -> Int)
    x <- get
    lift $ print $ "x = " ++ (show x)

-- >>> runStateT test2 1
-- "x = 2"
-- ((),2)
--

-- >>> runState test 1
-- (5,2)
--
