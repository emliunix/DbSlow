module Test where

import TypedExpr

-- **** test expressions ****

testExpr :: Expr
testExpr = App Plus [Lit $ IntVal 3, Lit $ DoubleVal 3.1]

testExpr2 :: Expr
testExpr2 = App Plus [Lit $ IntVal 3, Lit $ IntVal 4]

testCastExpr :: Expr
testCastExpr = App Plus [App (Cast IntType DoubleType) [Lit $ IntVal 3], Lit $ DoubleVal 3.1];

res :: String
res = case typeCheck (\_ -> UnknownType) testExpr of
    Right v -> show v
    Left e -> e

-- >>> putStrLn res
-- TypedExpr {typedExprTypeTag = DoubleType, typedExprE = TApp Plus [TypedExpr {typedExprTypeTag = DoubleType, typedExprE = TApp (Cast IntType DoubleType) [TypedExpr {typedExprTypeTag = IntType, typedExprE = TLit (IntVal 3)}] fun},TypedExpr {typedExprTypeTag = DoubleType, typedExprE = TLit (DoubleVal 3.1)}] fun}
--
