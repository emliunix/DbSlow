module Stage.CsvFileStage where

--

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
    , SqlType (..)
    , SqlVal (..)
    )
import Expr.Def (SqlExpr)
import Expr.Eval (eval)
import Data.Vector (toList)

import ReadRow2 (ReadCsv, readCsv, nextRow, closeReadCsv)
import SchemaInfer (decodeRow)

mkCsvFileStage :: FilePath -> [SqlType] -> Stage
mkCsvFileStage fp types = Stage
    { stgNewCursor = return $ toCursor $ CsvFileCursor { csvFileCursorState = Initial fp types }
    }

data CsvFileCursor = CsvFileCursor
    { csvFileCursorState :: CsvFileCursorState
    }

data CsvFileCursorState
    = Initial FilePath [SqlType]
    | Consuming ReadCsv [SqlType]
    | Consumed

instance RawCursor CsvFileCursor where
    rawCurNext = do
        openIfInit
        cur <- get
        case csvFileCursorState cur of
            Consuming rCsv types -> do
                (row, rCsv) <- lift $ runStateT nextRow rCsv
                case row of
                    Just row -> do
                        put $ cur { csvFileCursorState = Consuming rCsv types }
                        return $ Just $ decodeRow types (toList row)
                    Nothing -> do
                        put $ cur { csvFileCursorState = Consumed }
                        return Nothing
        where
            openIfInit = do
                cur <- get
                case csvFileCursorState cur of
                    Initial fp sch -> do
                        rCsv <- lift $ readCsv fp
                        put $ cur { csvFileCursorState = Consuming rCsv sch }
                    _ -> return ()

    rawCurClose = return ()
