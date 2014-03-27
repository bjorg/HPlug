{-# LANGUAGE OverloadedStrings #-}

--import Uri

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Bits ((.|.), (.&.), shiftL)
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

-- adapated from http://hackage.haskell.org/package/network
-- by Graham Klyne, BSD-style
decode :: [Char] -> [Char]
decode [] = ""
decode s@(c : cs) = case decodeChar s of
    Just (byte, rest) -> decodeUtf8 byte rest
    Nothing -> c : decode cs

decodeChar :: [Char] -> Maybe (Int, [Char])
decodeChar ('%' : h : l : s) | isHexDigit h && isHexDigit l = 
    Just (digitToInt h * 16 + digitToInt l, s)
decodeChar _ = Nothing

-- Adapted from http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
decodeUtf8 :: Int -> [Char] -> [Char]
decodeUtf8 c rest
    | c < 0x80 = chr c : decode rest
    | c < 0xc0 = replacement_character : decode rest
    | c < 0xe0 = multi1
    | c < 0xf0 = multi_byte 2 0xf 0x800
    | c < 0xf8 = multi_byte 3 0x7 0x10000
    | c < 0xfc = multi_byte 4 0x3 0x200000
    | c < 0xfe = multi_byte 5 0x1 0x4000000
    | otherwise    = replacement_character : decode rest
    where
        replacement_character = '\xfffd'
        multi1 = case decodeChar rest of
          Just (c1, ds) | c1 .&. 0xc0 == 0x80 ->
            let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
            in if d >= 0x000080 then toEnum d : decode ds
                                else replacement_character : decode ds
          _ -> replacement_character : decode rest

        multi_byte :: Int -> Int -> Int -> [Char]
        multi_byte i mask overlong =
          aux i rest (decodeChar rest) (c .&. mask)
          where
            aux 0 rs _ acc
              | overlong <= acc && acc <= 0x10ffff &&
                (acc < 0xd800 || 0xdfff < acc)     &&
                (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
              | otherwise = replacement_character : decode rs

            aux n _ (Just (r, rs)) acc
              | r .&. 0xc0 == 0x80 = aux (n-1) rs (decodeChar rs)
                                   $! shiftL acc 6 .|. (r .&. 0x3f)

            aux _ rs _ _ = replacement_character : decode rs
