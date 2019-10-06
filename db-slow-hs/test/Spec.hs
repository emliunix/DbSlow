module Spec where

import TypedExpr

main :: IO ()
main = putStrLn "Test suite not yet implemented"

-- **** test expressions ****

testExpr :: Expr
testExpr = App Plus [Lit $ IntVal 3, Lit $ DoubleVal 3.1]

testCastExpr :: Expr
testCastExpr = App Plus [App (Cast IntType DoubleType) [Lit $ IntVal 3], Lit $ DoubleVal 3.1];

res :: String
res = case typeCheck (\_ -> UnknownType) testCastExpr of
    Right v -> show v
    Left e -> e

-- >>> show res