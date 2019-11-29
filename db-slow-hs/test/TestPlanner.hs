module TestStage where

import Test.Hspec
import Planner

testExtractSqlClauses :: Spec
testTestStage = describe "extractSqlClauses" $ do
        it "[] should be Left" $ do
            extractSqlClauses [] `shouldBe` Left "invalid sql"
        it "[sel, from] should work" $ do
            extractSqlClauses [SClsSelect ]

testLimStage :: Spec
testLimStage = describe "LimitStage" $ do
    it "should return 1 2 3" $ do
        limTestRun `shouldReturn` ["1", "2", "3"]
    where
        limTestRun = do
            cur2 <- stgNewCursor $ mkLimitStage 3 $ mkTestStg 1
            v <- cursorToList cur2
            return $ concat v
