module Parser where

--
import Data.List
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Control.Applicative
import Text.Trifecta
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Char as C
import Debug.Trace (trace)

import Def

-- # Expression Parser #

lowercase :: String -> String
lowercase s = [toLower c | c <- s]

keywords :: [String]
keywords = [
        "select" , "as" , "from" , "and" , "or" ,
        "not" , "is" , "like" , "null" , "order" ,
        "by" , "left" , "right" , "inner" , "outer" ,
        "join" , "limit" , "where"
    ]

data OpInfo
    = LeftBinary
    | RightBinary
    | RightUnary

stringNoCase s = try $ do
    stringNoCase' s
    return s
    where
        stringNoCase' [] = return ()
        stringNoCase' (c:s) = satisfy (\c2 -> toLower c2 == toLower c) *> stringNoCase' s

keyword :: String -> Parser String
keyword s = 
    let sLower = lowercase s in try $ do
        s' <- token $ many C.letter
        let sLower' = lowercase s' in
            if sLower == sLower' then
                return s
            else
                fail "keyword mismatch"

opDefs :: [(OpInfo, [(Parser String, Op)])]
opDefs =
    [ ( RightUnary
      , [ (symbol "-", Minus)
        ]
      )
    , ( RightBinary
      , [ (symbol "^", Exponential)
        ]
      )
    , ( LeftBinary
      , [ (symbol "*", Multiply)
        ]
      )
    , ( LeftBinary
      , [ (symbol "*", Multiply)
        , (symbol "/", Divide)
        , (symbol "%", Modulo)
        ]
      )
    , ( LeftBinary
      , [ (symbol "+", Plus)
        , (symbol "-", Minus)
        ]
      )
    , ( LeftBinary
        -- ther order is important, >= and > has the same prefix, pick the longest match first
      , [ (symbol ">=", GreaterThanEqual)
        , (symbol ">", GreaterThan)
        , (symbol "<=", LessThanEqual)
        , (symbol "<>", NotEqual)
        , (symbol "<", LessThan)
        , (symbol "=", Equal)
        ]
      )
    , ( LeftBinary
      , [ (keyword "is", Is)
        , (keyword "like", Like)
        ]
      )
    , ( RightUnary
      , [ (keyword "not", Not)
        ]
      )
    , ( LeftBinary
      , [ (keyword "and", And)
        ]
      )
    , ( LeftBinary
      , [ (keyword "or", Or)
        ]
      )
    ]

identifier :: Parser String
identifier =
    try $ do
        x <- underscore <|> C.letter
        xs <- many (underscore <|> C.digit <|> C.letter)
        s <- return $ x:xs
        if isKeyword s then
            fail $ s ++ " is keyword"
        else
            return s
    <?> "identifier"
    where
        underscore = char '_'
        isKeyword s = [toLower c | c <- s] `elem` keywords

selIdent :: Parser String
selIdent = identifier <|> string "*" <?> "column"

qualifiedColIdent :: Parser [String]
qualifiedColIdent = do
    ids <- many $ try $ identifier <* (char '.')
    idn <- selIdent
    return $ ids ++ [idn]

parseLit :: Parser SqlSimpleExpr
parseLit = (\i -> SSimpleLit $ SVInt i) <$> integer

parseCol :: Parser SqlSimpleExpr
parseCol =
    do
        ids <- token qualifiedColIdent
        case ids of
            [] -> fail "invalid column"
            [ident] -> return $ SSimpleCol Nothing ident
            [ns, ident] -> return $ SSimpleCol (Just ns) ident
            -- xs -> case reverse ids of
            --     ident:rns -> return $ SSimpleCol (Just (foldl1 (\a b -> a ++ "." ++ b) (reverse rns))) ident
            _ -> fail "invalid column"
    <?> "column"

parseFunCall :: Parser SqlSimpleExpr
parseFunCall = try $ do
    funName <- token identifier
    args <- parens (commaSep parseExpr)
    return $ SSimpleApp (Fun funName) args

parseFactor :: Parser SqlSimpleExpr
parseFactor = parseLit <|> parseFunCall <|> parseCol <|> parens parseExpr <?> "factor"

parseExpr :: Parser SqlSimpleExpr
parseExpr =
    foldl addParser parseFactor opDefs
    where
        addParser :: Parser SqlSimpleExpr -> (OpInfo, [(Parser String, Op)]) -> Parser SqlSimpleExpr
        addParser p (LeftBinary, ops) =  addLeftBinaryParser ops p
        addParser p (RightBinary, ops) =  addRightBinaryParser ops p
        addParser p (RightUnary, ops) =  addUnaryParser ops p

mkOpParser :: [(Parser String, Op)] -> Parser Op
mkOpParser ops = foldl1 (<|>) $ fmap _mkOp ops
    where
        _mkOp (opP, op) = opP *> pure op <?> (show op)

addLeftBinaryParser :: [(Parser String, Op)] -> Parser SqlSimpleExpr -> Parser SqlSimpleExpr
addLeftBinaryParser ops p = do
    -- 1 + 2 - 3 becomes
    --    (-)
    --    / \
    --  (+)  3
    --  / \
    -- 1   2
    v <- p
    vs <- let parseOp = mkOpParser ops in
            many $ try $ do
                op <- parseOp
                v2 <- p
                return (op, v2)
    return $ foldl _mergeLeft v vs
    where
        _mergeLeft e1 (op, e2) = SSimpleApp op [e1, e2]

addRightBinaryParser :: [(Parser String, Op)] -> Parser SqlSimpleExpr -> Parser SqlSimpleExpr
addRightBinaryParser ops p = do
    -- 1 + 2 - 3 becomes
    --    (+)
    --    / \
    --   1  (-)
    --      / \
    --     2   3
    vs <- let parseOp = mkOpParser ops in
            many $ try $ do
                v2 <- p
                op <- parseOp
                return (v2, op)
    v <- p
    return $ foldr _mergeRight v vs
    where
        _mergeRight (e1, op) e2 = SSimpleApp op [e1, e2]

addUnaryParser :: [(Parser String, Op)] -> Parser SqlSimpleExpr -> Parser SqlSimpleExpr
addUnaryParser ops p = do
    -- - - - 1 becomes
    -- - (- (- (1)))
    let opParser = mkOpParser ops in
        do
            unaryOps <- many opParser
            v <- p
            return $ foldr _applyUnary v unaryOps
    where
        _applyUnary op v = SSimpleApp op [v]

-- # Select Query Parser #

parseSelect :: Parser SqlStmt
parseSelect = try $ do
    keyword "select"
    selCols <- many _parseSelCol
    optFrom <- fmap Just parseFrom <|> return Nothing
    case optFrom of
        -- it's a singletonExprSelect: select 1 + 1
        -- no need to parse where/orderBy/limit
        Nothing -> return $ SStmtSelect [SClsSelect selCols]
        Just clsFrom -> do
            optWhere <- fmap Just parseWhere <|> return Nothing
            optOrderBy <- fmap Just parseOrderBy <|> return Nothing
            optLimit <- fmap Just parseLimit <|> return Nothing
            return $ SStmtSelect $ [SClsSelect selCols, clsFrom] ++ catMaybes [optWhere, optOrderBy, optLimit]
    where
        _parseSelCol = try $ do
            expr <- token parseExpr
            name <- (fmap Just _asBinding) <|> return Nothing
            return (name, expr)
        _asBinding = try $ do
            keyword "as"
            token identifier

-- >>> parseString parseSelect mempty "SelEct 1 + fun(1, 2, 3) as t"
-- Success (SStmtSelect [SClsSelect [(Just "t",SSimpleApp Plus [SSimpleLit (SVInt 1),SSimpleApp (Fun "fun") [SSimpleLit (SVInt 1),SSimpleLit (SVInt 2),SSimpleLit (SVInt 3)]])]])
--
-- >>> parseString parseSelect mempty "sELect 1 + 1"
-- Success (SStmtSelect [SClsSelect [(Nothing,SSimpleApp Plus [SSimpleLit (SVInt 1),SSimpleLit (SVInt 1)])]])
--

parseFrom :: Parser SqlClause
parseFrom = try $ do
    keyword "from"
    f1 <- _subFrom
    -- fs <- many _join
    return $ SClsFrom f1
    where
        _asBinding = try $ do
            -- optional as
            (try $ keyword "as") <|> return ""
            token identifier
        _optAsBinding = fmap Just _asBinding <|> return Nothing
        _tbl = try $ do
            tbl <- token identifier
            bind <- _optAsBinding
            return $ SFromTable tbl bind
        _subSelect = try $ do
            selStmt <- parens $ parseSelect
            bind <- _asBinding
            return $ SFromSubSelect selStmt bind
        _subFrom = _tbl <|> _subSelect
        -- _join = 

-- >>> parseString parseFrom mempty "from a as t"
-- Success (SClsFrom (SFromTable "a" (Just "t")))
--

-- >>> parseString parseSelect mempty "select t.a from b as t"
-- Success (SStmtSelect [SClsSelect [(Nothing,SSimpleCol "t.a"),(Nothing,SSimpleCol "from"),(Just "t",SSimpleCol "b")]])
--

-- >>> parseString parseFrom mempty "from (select t.a from b as t) AS t1"
-- Success (SClsFrom (SFromSubSelect (SStmtSelect [SClsSelect [(Nothing,SSimpleCol "t.a"),(Nothing,SSimpleCol "from"),(Just "t",SSimpleCol "b")]]) "t1"))
--

parseWhere :: Parser SqlClause
parseWhere = try $ do
    keyword "where"
    expr <- token parseExpr
    return $ SClsWhere expr

parseOrderBy :: Parser SqlClause
parseOrderBy = try $ do
    keyword "order"
    keyword "by"
    orderBySpecs <- commaSep $ parseOrderBySpec
    return $ SClsOrderBy orderBySpecs

parseOrderBySpec :: Parser (String, SqlOrderBy)
parseOrderBySpec = try $ do
    col <- (foldl1 (\a b -> a ++ "." ++ b)) <$> token qualifiedColIdent
    ord <-
        (try $ keyword "asc" *> return SOrderASC) <|> 
        (try $ keyword "desc" *> return SOrderDESC) <|>
        return SOrderASC
    return (col, ord)

-- >>> parseString parseOrderBy mempty "order by a.t asc, b.t desc"
-- Success (SClsOrderBy [("a.t",SOrderASC),("b.t",SOrderDESC)])
--

parseLimit :: Parser SqlClause
parseLimit = try $ do
    keyword "limit"
    n <- integer
    return $ SClsLimit n

-- >>> parseString parseSelect mempty "select * from a where c > 0 and b < 1 order by d limit 100"
-- Success (SStmtSelect [SClsSelect [(Nothing,SSimpleCol Nothing "*")],SClsFrom (SFromTable "a" Nothing),SClsWhere (SSimpleApp And [SSimpleApp GreaterThan [SSimpleCol Nothing "c",SSimpleLit (SVInt 0)],SSimpleApp LessThan [SSimpleCol Nothing "b",SSimpleLit (SVInt 1)]]),SClsOrderBy [("d",SOrderASC)],SClsLimit 100])
--

-- >>> parseString parseExpr mempty "c > 0 and b < 1 order"
-- Success (SSimpleApp And [SSimpleApp GreaterThan [SSimpleCol "c",SSimpleLit (SVInt 0)],SSimpleApp LessThan [SSimpleCol "b",SSimpleLit (SVInt 1)]])
--

-- >>> parseString ((try $ keyword "or") <|> return "xx") mempty "order"
-- Success "xx"
--
