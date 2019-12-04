{-# LANGUAGE OverloadedStrings #-}
module ReadRow where

import Data.Csv.Incremental (decode, HasHeader (..), Parser (..))
import Data.Vector (Vector, toList)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.List (intersperse)
import qualified Data.ByteString.Char8 as BS
import System.IO as SysIO
import Conduit (runConduit, runConduitRes, ResourceT, liftIO, stdoutC, (.|))
import qualified Data.Conduit.Combinators as C

type Row = Vector ByteString

type Accum = BS.ByteString -> Parser Row

test :: IO ()
-- test = withFile "xxxx.csv" ReadMode $ \h = 
--     do
--         hIsEOF h
--         if isEof then
--             return $ k xx
--         else
--             k 
-- test = putStrLn "hello"
test = runConduitRes px
    where
        px = C.sourceFile "data/test.csv"
            .| expandCsv
            .| C.map fmtRow
            .| stdoutC
        expandCsv = C.concatMapAccumM expandCsv1 restart
        expandCsv1 :: BS.ByteString -> Accum -> ResourceT IO (Accum, [[ByteString]])
        expandCsv1 bs k =
            case k bs of
                Fail _ reason -> do
                    liftIO $ putStrLn reason
                    return (restart, [])
                Many rs k -> return $ (k,  fmap toList $ rights rs)
                Done rs -> return $ (restart, fmap toList $ rights rs)
        restart = case decode NoHeader of
            Many _ k -> k
        fmtRow :: [ByteString] -> ByteString
        fmtRow xs = BS.append s "\n"
            where s = BS.concat $ intersperse "," xs

-- >>> :t BS.pack
-- BS.pack :: String -> ByteString
--
-- >>> :t BS.concat
-- BS.concat :: [ByteString] -> ByteString
--

-- >>> :t BS.concat . (intersperse (BS.pack ","))

-- {-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- import Control.Monad
-- import qualified Data.ByteString as B
-- import Data.Csv.Incremental
-- import System.Exit
-- import System.IO

-- main :: IO ()
-- main = withFile "salaries.csv" ReadMode $ \ csvFile -> do
--     let loop !_ (Fail _ errMsg) = putStrLn errMsg >> exitFailure
--         loop acc (Many rs k)    = loop (acc + sumSalaries rs) =<< feed k
--         loop acc (Done rs)      = putStrLn $ "Total salaries: " ++
--                                   show (sumSalaries rs + acc)

--         feed k = do
--             isEof <- hIsEOF csvFile
--             if isEof
--                 then return $ k B.empty
--                 else k `fmap` B.hGetSome csvFile 4096
--     loop 0 (decode NoHeader)
--   where
--     sumSalaries rs = sum [salary | Right (_ :: String, salary :: Int) <- rs]