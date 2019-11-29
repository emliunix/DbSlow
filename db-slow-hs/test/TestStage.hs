module TestStage where

import Test.Hspec
import Control.Monad.State.Lazy
import Stage.Def
import Stage.TestStg
import Stage.LimitStg

-- test program

testTestStage :: Spec
testTestStage = describe "TestStage" $ do
        it "should return 1 2 3" $ do
            testRun `shouldReturn` (Just ["1", "2", "3"])
    where
        testRun = do
            cur <- stgNewCursor $ mkTestStg 1
            evalStateT testRun' cur
        testRun' = do
            a <- curNext
            b <- curNext
            c <- curNext
            return $ fmap concat $ sequence [a, b, c]

testLimStage :: Spec
testLimStage = describe "LimitStage" $ do
    it "should return 1 2 3" $ do
        limTestRun `shouldReturn` ["1", "2", "3"]
    where
        limTestRun = do
            cur2 <- stgNewCursor $ mkLimitStage 3 $ mkTestStg 1
            v <- cursorToList cur2
            return $ concat v
