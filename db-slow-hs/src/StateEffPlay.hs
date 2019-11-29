{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module StateEffPlay where

import Control.Eff
import Control.Eff.State.Lazy


-- a stage exposes schema and [Row] where Row is [Val]
-- and Row should satisfy schema

effPlusOne :: (Member (State Int) r, Lifted IO r) => Eff r Int
effPlusOne = do
    x :: Int <- get
    res <- pure $ x + 1
    lift $ putStrLn $ "result is " ++ (show res)
    return res

runEffPlusOne :: IO (Int, Int)
runEffPlusOne = runLift . runState 3 $ effPlusOne

-- >>> runEffPlusOne
-- <interactive>:255:2-14: error: Variable not in scope: runEffPlusOne
--
