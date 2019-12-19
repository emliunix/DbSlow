{-# LANGUAGE ExistentialQuantification
           , OverloadedStrings
           #-}
module SchemaInfer where

--

import Def (Schema)
import Expr.Def (SqlType (..), SqlVal (..))
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

-- >> infixl 1
-- <|> infxil 3
-- <$> infixl 4
-- <* infixl 4

pEof :: forall a. Parser (Maybe a)
pEof = eof *> (return Nothing)

decodeInt :: ByteString -> Maybe SqlVal
decodeInt s =
    case parseByteString (whiteSpace >> pInt <|> pEof) mempty s of
        Success (Just v) -> Just $ SVInt v
        Success Nothing -> Just SVNull
        Failure _ -> Nothing
    where
        pInt = Just <$> integer <* whiteSpace <* eof

decodeDouble :: ByteString -> Maybe SqlVal
decodeDouble s =
    case parseByteString (whiteSpace >> pDoubleOrInt <|> pEof) mempty s of
        Success (Just v) -> Just $ SVDouble v
        Success Nothing -> Just SVNull
        Failure _ -> Nothing
    where
        pDoubleOrInt = Just <$> ((try double) <|> (fromInteger <$> try integer)) <* whiteSpace <* eof

decodeBool :: ByteString -> Maybe SqlVal
decodeBool s =
    case parseByteString (whiteSpace >>( Just <$> pBool <* whiteSpace <* eof) <|> pEof) mempty s of
        Success (Just v) -> Just $ SVBool v
        Success Nothing -> Just SVNull
        Failure _ -> Nothing
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

decodeString :: ByteString -> Maybe SqlVal
decodeString s = Just . SVString $ B.unpack s

mkIsTypeFn :: (ByteString -> Maybe SqlVal) -> IsTypeFn
mkIsTypeFn f s = case f s of
    Just _ -> True
    Nothing -> False

isInt :: IsTypeFn
isInt = mkIsTypeFn decodeInt

isDouble :: IsTypeFn
isDouble = mkIsTypeFn decodeDouble

isBool :: IsTypeFn
isBool = mkIsTypeFn decodeBool

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

decodeRow :: [SqlType] -> [ByteString] -> [SqlVal]
decodeRow sch bs = map (\(t, v) -> decodeCell t v) $ zip sch bs

decodeCell :: SqlType -> ByteString -> SqlVal
decodeCell t v =
    case (t, v) of
        (STString, "") -> SVString ""
        (_, "") -> SVNull
        (STInt, v) -> unwrap $ decodeInt v
        (STDouble, v) -> unwrap $ decodeDouble v
        (STBool, v) -> unwrap $ decodeBool v
        (STString, v) -> SVString $ B.unpack v
    where
        unwrap v = case v of
            Just v -> v
