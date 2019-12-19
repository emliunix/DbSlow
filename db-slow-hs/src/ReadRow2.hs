{-# LANGUAGE OverloadedStrings #-}
module ReadRow2 where

--

import System.IO (hIsEOF, hClose, openFile, IOMode (..), Handle)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (hGetSome)
import Data.Csv.Incremental (Parser (..), decode, HasHeader (..), decodeHeader, HeaderParser (..))
import Data.Vector (Vector, toList)
import Control.Monad.State.Lazy (StateT, lift, get, put, runStateT)
import Data.Either (rights)
import Data.List (intersperse)

import Def (Row, Schema, SqlType (..))

import SchemaInfer (inferRow)

--

type ParseFn = ByteString -> Parser (Vector ByteString)

data ReadCsv = ReadCsv
    { rcsvHd :: Handle
    , rcsvK :: ParseFn
    , rcsvRows :: [Vector ByteString]
    }

toRow :: Vector ByteString -> Schema -> Row
toRow _ _ = []

emptyParseFn :: ParseFn
emptyParseFn = case decode HasHeader of
    Many [] k -> k

readCsv :: FilePath -> IO ReadCsv
readCsv fp = do
    fHd <- openFile fp ReadMode
    return $ ReadCsv fHd emptyParseFn []

nextRow :: StateT ReadCsv IO (Maybe (Vector ByteString))
nextRow = do
    rc <- get
    (r, rc') <- lift $ nextRow' rc
    put rc'
    return r
    where
        nextRow' :: ReadCsv -> IO (Maybe (Vector ByteString), ReadCsv)
        nextRow' rc =
            case rows of
                r:rs -> return (Just r, rc { rcsvRows = rs })
                [] -> do
                    (rs, k) <- fetchMore [] k
                    case rs of
                        Just (r:rs) -> return $ (Just r, rc { rcsvK = k, rcsvRows = rs })
                        Nothing -> return (Nothing, rc { rcsvK = k, rcsvRows = []})
            where
                hd = rcsvHd rc
                k = rcsvK rc
                rows = rcsvRows rc
                fetchMore rs k =
                    case rs of
                        [] -> do
                            isEOF <- hIsEOF hd
                            if isEOF then
                                return (Nothing, k)
                            else do
                                bs <- hGetSome hd 4096
                                case k bs of
                                    Fail _ reason -> fail reason
                                    Many rs k -> fetchMore rs k
                                    Done rs -> fetchMore rs emptyParseFn
                        _ -> return $ (Just (rights rs), k)

closeReadCsv :: ReadCsv -> IO ()
closeReadCsv rc = hClose $ rcsvHd rc

prnCsv :: FilePath -> IO ()
prnCsv fp = do
    rc <- readCsv fp
    (_, rc) <- runStateT consumeRows rc
    closeReadCsv rc
    return ()
    where
        consumeRows = do
            r <- nextRow
            case r of
                Just r -> do
                    lift $ prnRow r
                    consumeRows
                Nothing -> return ()
        prnRow :: Vector ByteString -> IO ()
        prnRow row = putStrLn $ BS.unpack $ BS.concat $ intersperse "," $ toList row

inferType :: FilePath -> IO [SqlType]
inferType fp = do
    rc <- readCsv fp
    (ts, rc) <- runStateT consumeRows rc
    closeReadCsv rc
    return ts
    where
        initTypes = repeat STUnknown
        -- consumeRows :: StateT ReadCsv IO [SqlType]
        consumeRows = inferRowTypes 0 initTypes
        -- inferRowTypes :: Int -> [SqlType] -> StateT ReadCsv IO [SqlType]
        inferRowTypes i types = 
            if i == 100 then
                return types
            else do
                r <- nextRow
                case r of
                    Just r -> do
                        inferRowTypes (i+1) (inferRow types (toList r))
                    Nothing -> return types

readHeader :: FilePath -> IO (Vector ByteString)
readHeader fp = do
    hd <- openFile fp ReadMode
    case decodeHeader of
        PartialH k -> feedMore k hd
    where
        -- feedMore :: (ByteString -> HeaderParser ByteString) -> Handle -> IO (Vector ByteString)
        feedMore k hd = do
            bs <- hGetSome hd 4096
            case k bs of
                DoneH header _ -> do
                    hClose hd
                    return header
                PartialH k -> feedMore k hd

-- >>> inferType "/home/liu/play/Popular_Baby_Names.csv"
-- [STInt,STString,STString,STString,STDouble,STInt]
--
