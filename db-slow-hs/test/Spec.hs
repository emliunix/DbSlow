-- module Main where

import Test.Hspec
import TypedExpr
import TestStage

_prnRet3 :: IO Int
_prnRet3 = do
    print "3"
    return 3

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main = hspec $ do
    describe "Cursor test" $ do
        it "print and return 3" $ do
            _prnRet3 `shouldReturn` (3 :: Int)
    testTestStage
    testLimStage

-- main2 :: IO ()
-- main2 = hspec $ do
--     describe "Prelude.head" $ do
--         it "returns the first element of a list" $ do
--         head [23 ..] `shouldBe` (23 :: Int)
    
--         it "returns the first element of an *arbitrary* list" $
--         property $ \x xs -> head (x:xs) == (x :: Int)
    
--         it "throws an exception if used with an empty list" $ do
--         evaluate (head []) `shouldThrow` anyException
-- **** test expressions ****

-- testExpr :: Expr
-- testExpr = App Plus [Lit $ IntVal 3, Lit $ DoubleVal 3.1]

-- testCastExpr :: Expr
-- testCastExpr = App Plus [App (Cast IntType DoubleType) [Lit $ IntVal 3], Lit $ DoubleVal 3.1];

-- res :: String
-- res = case typeCheck (\_ -> UnknownType) testCastExpr of
--     Right v -> show v
--     Left e -> e

-- >>> show res