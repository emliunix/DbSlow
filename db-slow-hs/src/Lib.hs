{-# LANGUAGE KindSignatures, RankNTypes #-}
module Lib
    ( parseExpr
    , someFunc
    , testTracing
    ) where

import Data.List
import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Char as C
import Debug.Trace (trace)
        
someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- data Token
--     = Select
--     | From
--     | Where
--     | As
--     | In
--     | Left
--     | Right
--     | Full
--     | Inner
--     | Outer
--     | Join
--     | Is
--     | Not
--     | Null
--     | Eq
--     | Ne
--     | Gt
--     | GtE
--     | Lt
--     | LtE
--     | Ident String
--     deriving (Show)

data Op
    = Plus
    | Minus
    | Multiply
    | Divide
    deriving (Show)

data Expr
    = Lit Integer
    | Col [String]
    | Compu Op [Expr]
    | App String [Expr]
    deriving (Show)

opMeta :: [(Int, [(String, Op)])]
opMeta = [
         (1, [("+", Plus), ("-", Minus)])
        ,(2, [("*", Multiply), ("/", Divide)])
    ]

identifier :: Parser String
identifier = do {
        x <- underscore <|> C.letter;
        xs <- many (underscore <|> C.digit <|> C.letter);
        let s = x:xs in
        return s
    } <?> "identifier"
    where
        underscore = char '_'

colIdent :: Parser String
colIdent = identifier <|> string "*" <?> "column"

qualifiedColIdent :: Parser [String]
qualifiedColIdent = do {
    id0 <- colIdent;
    ids <- many $ (char '.') *> colIdent;
    return $ id0:ids
}

parseLit :: Parser Expr
parseLit = (\i -> Lit i) <$> integer

parseCol :: Parser Expr
parseCol = (\s -> Col s) <$> (token qualifiedColIdent)

parseFunCall :: Parser Expr
parseFunCall = try $ do {
        funName <- token identifier;
        args <- parens (commaSep parseExpr);
        return $ App funName args
    }

parseFactor :: Parser Expr
parseFactor = parseLit <|> parseFunCall <|> parseCol <|> parens parseExpr <?> "factor"

parseExpr :: Parser Expr
parseExpr =
    foldr addParser parseFactor opMeta
    where

        mkOpParser ops =
            foldl1 (<|>) $ map (\(opSym, op) -> symbol opSym *> pure op) ops
        addParser :: (Int, [(String, Op)]) -> Parser Expr -> Parser Expr
        addParser (_, ops) p = do {
            v <- p;
            vs <- many (
                let parseOp = mkOpParser ops in
                    do {
                        op <- parseOp;
                        v2 <- p;
                        return (op, v2)
                    }
            );
            -- constructs a binary tree representation for the expression
            -- 1 + 2 - 3 becomes
            --    (-)
            --    / \
            --  (+)  3
            --  / \
            -- 1   2
            -- use foldr when right associative
            -- haven't ever seen right associative in SQL, leave only left support
            return $ foldl (\e1 (op, e2) -> Compu op [e1, e2]) v vs
        }

evalOp :: Op -> [Integer] -> Integer
evalOp op vals =
    foldl1 opFun vals
    where
        opFun = case op of
            Plus -> (+)
            Minus -> (-)
            Multiply -> (*)
            Divide -> div

eval :: (String -> Integer) -> (String -> [Integer] -> Integer) -> Expr -> Integer
eval lookupCol lookupFun expr =
    case expr of
        Lit i -> i
        Col (x:xs) -> lookupCol x
        Compu op exprs ->
            let vals = fmap (eval lookupCol lookupFun) exprs in
                evalOp op vals
        App funName args ->
            let argVals = fmap (eval lookupCol lookupFun) args in
                lookupFun funName argVals

evalTracing :: Expr -> [String]
evalTracing expr =
    case expr of
        Lit i -> ["const " ++ (show i)]
        Col (x:xs) -> ["get " ++ x]
        Compu op exprs -> shimOps op $ fmap evalTracing exprs
        App funName args -> shimApp funName $ fmap evalTracing args
    where
        shimOpsTwo :: Op -> [String] -> [String] -> [String]
        shimOpsTwo op a b =
            a ++ b ++ [asmOp op]
        shimOps :: Op -> [[String]] -> [String]
        shimOps op vals = foldl1 (shimOpsTwo op) vals
        asmOp Plus = "add"
        asmOp Minus = "sub"
        asmOp Multiply = "mul"
        asmOp Divide = "div"
        shimApp :: String -> [[String]] -> [String]
        shimApp funName args =
            (foldl1 (++) args) ++ [("call " ++ funName)]

colDefs :: String -> Integer
colDefs "a" = 1
colDefs "b" = 2
colDefs "c" = 3
colDefs _ = -1

funDefs :: String -> [Integer] -> Integer
funDefs "plus" vals = evalOp Plus vals
funDefs _ _ = -1

test = let exprOpt = parseString parseExpr mempty "a + 2 * plus(3 , c) - 14 / (3 - 1)" in
    case exprOpt of
        Success e -> show $ eval colDefs funDefs e
        Failure err -> show err

-- >>> putStrLn test
-- 6
--

testTracing = let exprOpt = parseString parseExpr mempty "a + 2 * plus(3, c) - 14 / (3 - 1)" in
    case exprOpt of
        Success e -> intercalate "\n" $ evalTracing e
        Failure err -> show err

-- >>> putStrLn testTracing
-- get a
-- const 2
-- const 3
-- get c
-- call plus
-- mul
-- add
-- const 14
-- const 3
-- const 1
-- sub
-- div
-- sub
--
