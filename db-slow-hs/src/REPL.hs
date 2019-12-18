{-# LANGUAGE ExistentialQuantification #-}

module REPL where

--

import Data.List (intercalate)
import Data.Functor.Identity (Identity (..))
import Control.Monad (forever, forM_)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, modify, get)
import Control.Monad.Except (runExceptT, ExceptT, throwError)
import qualified Control.Monad.Except as Exc
import Control.Monad.IO.Class (MonadIO (..))
import Text.Trifecta (parseString, Result (..))

import System.Console.Haskeline

import Def (Row, SqlStmt (..), SqlVal (..), SqlClause, SqlTableRepo, Stage (..), cursorToList)
import Parser (parseStmt)
import Planner (buildPlanTree)
import Stage.BuildStage (buildStage)
import PrimOps (primOpContext)

import qualified Test

data Context = Context
    { theValue :: Int
    , shouldExit :: Bool
    , tblRepo :: SqlTableRepo
    }
data EvalResult = EvalResult { rows :: [Row], msg :: String }
-- tblRepo :: TableRepository

initialContext = Context { theValue = 0, shouldExit = False, tblRepo = Test.tblRepo }

helpMsg :: String
helpMsg =
    "Supported Statements:\n" ++
    " * quit\n" ++
    " * more\n" ++
    " * attach tbl_name '/path/to/data.csv';\n" ++
    " * select ...;\n"

repl :: IO ()
repl = runInputT defaultSettings (evalStateT loop initialContext)
    where
        loop = do
            ipt <- lift $ getInputLine "> "
            case ipt of
                Just ipt ->
                    case ipt of
                        "quit" -> setExit
                        _ -> processInput ipt
                Nothing -> do
                    lift $ outputStr helpMsg
            checkExitAndLoop
        setExit :: forall m . Monad m => StateT Context m ()
        setExit = modify (\ctx -> ctx { shouldExit = True })
        checkExitAndLoop = do
            ctx <- get
            if shouldExit ctx then
                return ()
            else
                loop
        processInput ipt = do
            r <- runExceptT $ processInputExc ipt
            case r of
                Right () -> return ()
                Left err -> lift (outputStrLn "Error:" >> outputStrLn err)

-- read & eval & print
processInputExc :: forall m. MonadException m => String -> ExceptT String (StateT Context (InputT m)) ()
processInputExc ipt = do
    stmt <- pStmt ipt
    res <- evalStmt stmt
    case msg res of
        "" -> return ()
        msg -> Exc.lift $ lift $ outputStrLn msg
    case rows res of
        [] -> return ()
        rows -> forM_ rows (Exc.lift . lift . pRow)
    return ()
    where
        pRow row = outputStrLn $ formatRow row

pStmt :: forall m. Monad m => String -> ExceptT String m SqlStmt
pStmt s = case parseString parseStmt mempty s of
    Success stmt -> return stmt
    Failure reason -> throwError (show reason)

evalStmt :: forall m. MonadException m => SqlStmt -> ExceptT String (StateT Context (InputT m)) EvalResult
evalStmt stmt = case stmt of
    SStmtSelect clauses -> evalSelect clauses

evalSelect :: forall m. MonadException m => [SqlClause] -> ExceptT String (StateT Context (InputT m)) EvalResult
evalSelect clauses = do
    tblRepo <- Exc.lift getTblRepo
    plan <- Exc.liftEither $ buildPlanTree clauses primOpContext tblRepo
    stg <- Exc.mapExceptT (\x -> return $ runIdentity x) $ buildStage tblRepo plan
    cur <- Exc.lift $ liftIO $ stgNewCursor stg
    rows <- Exc.lift $ liftIO $ cursorToList cur
    return $ EvalResult rows (show $ SStmtSelect clauses)
    where
        getTblRepo = do
            ctx <- get
            return $ tblRepo ctx

formatRow :: Row -> String
formatRow row =
    intercalate " | " $ fmap formatCell row
    where
        formatCell (SVInt v) = show v
        formatCell (SVDouble v) = show v
        formatCell (SVBool v) = show v
        formatCell (SVString v) = "\"" ++ v ++ "\""
        formatCell SVNull = "NULL"
