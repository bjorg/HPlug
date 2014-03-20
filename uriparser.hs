{-# LANGUAGE OverloadedStrings #-}

--import Uri

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char (chr, digitToInt, isAlpha, isAlphaNum, isHexDigit)
--import Data.Attoparsec.Combinator
import Data.Text
--import Data.Text.Read as T

type Scheme = Text
type Username = Text
type Password = Text
type Hostname = Text
type Port = Int

parseUri :: Parser (Scheme, Maybe Username, Maybe Password, Hostname, Maybe Port)
parseUri = do
    scheme <- parseScheme
    (user, pwd, hostname, port) <- parseAuthority
    return (scheme, user, pwd, hostname, port)

parseScheme :: Parser Text
parseScheme = do
    schemeFirst <- A.satisfy isAlpha
    schemeRest <- A.takeWhile isAlphaNum
    _ <- string "://"
    return $ schemeFirst `cons` schemeRest

parseAuthority :: Parser (Maybe Username, Maybe Password, Hostname, Maybe Port)
parseAuthority = do
        (username, pwd) <- parseUserInfo
        hostname <- parseHostname
        port <- parsePort
        return (username, pwd, hostname, port)

parseUserInfo :: Parser (Maybe Username, Maybe Password)
parseUserInfo = do
        username <- A.takeWhile isUserInfoChar
        _ <- char ':'
        password <- A.takeWhile isUserInfoChar
        _ <- char '@'
        return (Just username, Just password)
    <|> do
        username <- A.takeWhile isUserInfoChar
        _ <- char '@'
        return (Just username, Nothing)
    <|>
        return (Nothing, Nothing)

parseHostname :: Parser Text
parseHostname = do
    hostname <- A.takeWhile isHostnameChar
    return hostname

parsePort :: Parser (Maybe Port)
parsePort = do
        _ <- char ':'
        port <- decimal
        return $ Just port
    <|> do
        return Nothing

parseEncodedChar :: (Char -> Bool) -> Parser Char
parseEncodedChar isValidChar = do
        _ <- char '%'
        h <- A.satisfy isHexDigit
        l <- A.satisfy isHexDigit
        return $ chr $ (digitToInt h) * 16 + (digitToInt l)
    <|> do
        c <- A.satisfy isValidChar
        return c

isUserInfoChar :: Char -> Bool
isUserInfoChar c =
    ((c >= 'a') && (c <= 'z')) ||
    ((c >= 'A') && (c <= 'Z')) ||
    ((c >= '0') && (c <= '9')) ||
    ((c >= '$') && (c <= '.')) ||   -- one of: $%&'()*+,-.
    (c == '!') || (c == ';') || (c == '=') || (c == '_') || (c == '~') || 
    (isAlphaNum c)    

isHostnameChar :: Char -> Bool
isHostnameChar c = 
    ((c >= 'a') && (c <= 'z')) ||
    ((c >= 'A') && (c <= 'Z')) ||
    ((c >= '0') && (c <= '9')) ||
    ((c >= '$') && (c <= '.')) ||   -- one of: $%&'()*+,-.
    (c == '!') || (c == ';') || (c == '=') || (c == '_') || (c == '~') || 
    (isAlphaNum c)

isPathChar :: Char -> Bool
isPathChar c =
    ((c >= 'a') && (c <= '~')) ||   -- one of: abcdefghijklmnopqrstuvwxyz{|}~
    ((c >= '@') && (c <= '_')) ||   -- one of: @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
    ((c >= '$') && (c <= ';')) ||   -- one of: $%&'()*+,-./0123456789:;
    (c == '=') || (c == '!') || 
    (isAlphaNum c)

isQueryChar :: Char -> Bool
isQueryChar c =
    ((c >= 'a') && (c <= '~')) ||   -- one of: abcdefghijklmnopqrstuvwxyz{|}~
    ((c >= '?') && (c <= '_')) ||   -- one of: ?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
    ((c >= '\'') && (c <= ';')) ||  -- one of: '()*+,-./0123456789:;
    (c == '$') || (c == '%') || (c == '!') || 
    (isAlphaNum c)

isFragmentChar :: Char -> Bool
isFragmentChar c = 
    ((c >= 'a') && (c <= '~')) ||   -- one of: abcdefghijklmnopqrstuvwxyz{|}~
    ((c >= '?') && (c <= '_')) ||   -- one of: ?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_
    ((c >= '#') && (c <= ';')) ||   -- one of: #$%&'()*+,-./0123456789:;
    (c == '=') || (c == '!') || 
    (isAlphaNum c)
