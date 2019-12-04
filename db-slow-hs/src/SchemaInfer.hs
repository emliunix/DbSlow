
module SchemaInfer where

--

import Expr.Def (SqlType (..))
import Text.Parser.Token (whiteSpace, integer, double)
import Text.Parser.Char (letter)
import Text.Parser.Combinators (many, eof, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Text.Trifecta (Result (..), Parser, parseByteString)
import Data.Char (toLower)
import Control.Applicative ((<|>))

--

-- Unknown -> [Int, Double, Bool]
-- Int -> [Double, String]
-- Double -> [String]
-- Bool -> [String]

type IsTypeFn = ByteString -> Bool

isUnknown :: IsTypeFn
isUnknown _ = False

isInt :: IsTypeFn
isInt s = case parseByteString (whiteSpace >> (integer >> whiteSpace >> eof) <|> eof) mempty s of
    Success _ -> True
    Failure _ -> False

isDouble :: IsTypeFn
isDouble s = case parseByteString (whiteSpace >> (tryDouble <|> tryInteger <|> eof)) mempty s of
    Success _ -> True
    Failure _ -> False
    where
        tryInteger = try (integer >> whiteSpace >> eof)
        tryDouble = try (double >> whiteSpace >> eof)

isBool :: IsTypeFn
isBool s = case parseByteString (whiteSpace >>( pBool >> whiteSpace >> eof) <|> eof) mempty s of
    Success _ -> True
    Failure _ -> False
    where
        pBool = do
            s <- many letter
            s <- return $ [toLower c | c <- s]
            if s `elem` ["true", "t", "yes"] then
                return True
            else if s `elem` ["false", "f", "no"] then
                return False
            else
                fail "not a bool"

isString :: IsTypeFn
isString _ = True

casts :: SqlType -> [SqlType]
casts STUnknown = [STInt, STDouble, STBool, STString]
casts STInt = [STDouble, STString]
casts STDouble = [STString]
casts STBool = [STString]

getIsTypeFn :: SqlType -> IsTypeFn
getIsTypeFn STUnknown = isUnknown
getIsTypeFn STInt = isInt
getIsTypeFn STDouble = isDouble
getIsTypeFn STBool = isBool
getIsTypeFn STString = isString

infer1 :: SqlType -> ByteString -> SqlType
infer1 t s =
    if getIsTypeFn t s then
        t
    else
        findType (casts t) s
    where
        findType (t:ts) s =
            if getIsTypeFn t s then
                t
            else
                findType ts s

inferRow :: [SqlType] -> [ByteString] -> [SqlType]
inferRow types vals =
    fmap (\(t, v) -> infer1 t v) $ zip types vals

infer :: [[ByteString]] -> [SqlType]
infer rows@(r:_) =
    foldl inferRow initialTypes rows
    where
        initialTypes = take (length r) $ repeat STUnknown
