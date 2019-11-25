{-# LANGUAGE ExistentialQuantification
           , MultiParamTypeClasses
           , OverloadedStrings
#-}

module Stage where

import Data.List
import Data.Maybe
import qualified Data.Csv as Csv
import qualified System.IO as SIO
import qualified System.Directory as SDir
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
-- import Control.Monad.ST.Lazy
import Control.Monad.State.Lazy

import TypedExpr
    ( TypeTag
    )

-- IO is a monad, which has kind * -> *
-- how do we keep state together with IO
-- maybe a Monad Transformer that combines IO and State

-- back to the stages, let's say, the most simple stage, tableScan
-- it should be split into 3 IO parts:
-- 1. init
-- 2. next
-- 3. close
-- and also, we need some inpspection functions to retrieve meta information, let's say
-- 1. schema :: t -> [(String, TypeTag)]

-- ## Stages
-- * CsvFile
-- * Filter
-- * Projection
-- * Sort
-- * MergeSorted

-- ## Definitions

data Schema = Schema { schema :: [String] }

type Row = Vector ByteString

class Stage t where
    getSchema :: t -> IO Schema
    getCursor :: forall c. Cursor c => t -> c

class Cursor c where
    curNext :: StateT c IO (Maybe Row)
    curClose :: StateT c IO ()

processCursor :: forall c. Cursor c => c -> (Row -> IO ()) -> IO ()
processCursor c process = do
    tmp <- runStateT curNext c
    case tmp of
        (Just d, c') -> do
            print "Processing Row"
            process d
            processCursor c' process
        (Nothing, _) -> do
            print "End Processing"
            runStateT curClose c
            return ()

printCursor :: forall c. Cursor c => c -> IO ()
printCursor c = do
    processCursor c $ \ row ->
        print $ V.foldl1 BS.append row

-- # TestFileCursor #

-- data TestFileCursor = TestFileCursor { postfix :: Int }

-- instance Cursor TestFileCursor where
--     curNext = do
--         -- update cursor
--         x <- get
--         put $ x { postfix = (postfix x) + 1 }
--         -- read data
--         lift $ let
--                 _pfix = postfix x 
--                 fpath = "data/part-" ++ (show _pfix)
--             in do
--                 exists <- SDir.doesFileExist fpath
--                 if exists then
--                     fmap Just $ readFile $ fpath
--                 else
--                     return Nothing

--     -- Nothing to close
--     curClose = return ()

-- >>> printCursor TestFileCursor { postfix = 0 }
-- "Processing Row"
-- "xxxx"
-- "Processing Row"
-- "yyyyyy"
-- "End Processing"
--

-- # Simple CSV Stage #

data StageCsvFile = StageCsvFile
    { csvPath :: String
    , csvSchema :: Schema
    }

data CursorCsvFile = CursorCsvFile
    { csvCurData :: Vector (Vector ByteString)
    , csvCurIdx :: Int
    , csvCurClosed :: Bool
    }

xx :: Either String (Vector (Vector ByteString))
xx = Csv.decode Csv.NoHeader d
    where
        d = "John,27\r\nJane,28\r\n" :: ByteString

instance Cursor CursorCsvFile where
    curNext = do
        c <- get
        if csvCurClosed c then 
            return Nothing
        else
            let idx = csvCurIdx c
            in do
                put $ c { csvCurIdx = 1 + idx }
                case (csvCurData c) V.!? idx of
                    Just v -> return $ Just v
                    Nothing -> do
                        put $ c { csvCurClosed = True }
                        return Nothing

    curClose = return ()

createCsvCursor :: FilePath -> IO CursorCsvFile
createCsvCursor fpath = do
    d <- BS.readFile fpath
    case Csv.decode Csv.HasHeader d of
        Right v -> return $ CursorCsvFile
            { csvCurData = v
            , csvCurIdx = 0
            , csvCurClosed = False
            }
        Left reason -> fail reason

testCsv :: IO ()
testCsv = do
    cur <- createCsvCursor "data/test.csv"
    printCursor cur

-- >>> testCsv
-- "Processing Row"
-- "John27"
-- "Processing Row"
-- "Jane28"
-- "End Processing"
--

inc :: State Int ()
inc = do
    modify ((+1) :: Int -> Int)

test :: State Int Int
test = do
    inc
    x <- get
    return $ x + 3

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
