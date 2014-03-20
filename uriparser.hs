{-# LANGUAGE OverloadedStrings #-}

--import Uri

import Control.Applicative
import Data.Attoparsec.Text as A
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
    (user, pwd, host, port) <- parseAuthority
    return (scheme, user, pwd, host, port)

parseScheme :: Parser Text
parseScheme = do
    s1 <- A.satisfy isAlphaChar
    sr <- A.takeWhile isSchemeChar
    _ <- string "://"
    return $ s1 `cons` sr

parseAuthority :: Parser (Maybe Username, Maybe Password, Hostname, Maybe Port)
parseAuthority = do
        (user, pwd) <- parseUserInfo
        host <- parseHostname
        port <- parsePort
        return (user, pwd, host, port)

parseUserInfo :: Parser (Maybe Username, Maybe Password)
parseUserInfo = do
        u <- A.takeWhile isUserInfoChar
        _ <- char ':'
        p <- A.takeWhile isUserInfoChar
        _ <- char '@'
        return (Just u, Just p)
    <|> do
        u <- A.takeWhile isUserInfoChar
        _ <- char '@'
        return (Just u, Nothing)
    <|>
        return (Nothing, Nothing)

parseHostname :: Parser Text
parseHostname = do
    h <- A.takeWhile isHostnameChar
    return h

parsePort :: Parser (Maybe Port)
parsePort = do
        _ <- char ':'
        p <- decimal
        return $ Just p
    <|> do
        return Nothing








{-}

host :: URIParser String
host = ipLiteral <|> try ipv4address <|> regName

ipLiteral :: URIParser String
ipLiteral =
    do  { char '['
        ; ua <- ( ipv6address <|> ipvFuture )
        ; char ']'
        ; return $ "[" ++ ua ++ "]"
        }
    <?> "IP address literal"

ipv6address :: URIParser String
ipv6address =
        try ( do
                { a2 <- count 6 h4c
                ; a3 <- ls32
                ; return $ concat a2 ++ a3
                } )
    <|> try ( do
                { string "::"
                ; a2 <- count 5 h4c
                ; a3 <- ls32
                ; return $ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 0
                ; string "::"
                ; a2 <- count 4 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 1
                ; string "::"
                ; a2 <- count 3 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 2
                ; string "::"
                ; a2 <- count 2 h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ concat a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 3
                ; string "::"
                ; a2 <- h4c
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ a2 ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 4
                ; string "::"
                ; a3 <- ls32
                ; return $ a1 ++ "::" ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 5
                ; string "::"
                ; a3 <- h4
                ; return $ a1 ++ "::" ++ a3
                } )
    <|> try ( do
                { a1 <- opt_n_h4c_h4 6
                ; string "::"
                ; return $ a1 ++ "::"
                } )
    <?> "IPv6 address"


opt_n_h4c_h4 :: Int -> URIParser String
opt_n_h4c_h4 n = option "" $
    do  { a1 <- countMinMax 0 n h4c
        ; a2 <- h4
        ; return $ concat a1 ++ a2
        }

ls32 :: URIParser String
ls32 =  try ( do
                { a1 <- h4c
                ; a2 <- h4
                ; return (a1++a2)
                } )
    <|> ipv4address

h4c :: URIParser String
h4c = try $
    do  { a1 <- h4
        ; char ':'
        ; notFollowedBy (char ':')
        ; return $ a1 ++ ":"
        }

h4 :: URIParser String
h4 = countMinMax 1 4 hexDigitChar

ipv4address :: URIParser String
ipv4address =
    do  { a1 <- decOctet ; char '.'
        ; a2 <- decOctet ; char '.'
        ; a3 <- decOctet ; char '.'
        ; a4 <- decOctet
        ; notFollowedBy regName
        ; return $ a1++"."++a2++"."++a3++"."++a4
        }
    <?> "IPv4 Address"

decOctet :: URIParser String
decOctet =
    do  { a1 <- countMinMax 1 3 digitChar
        ; if (read a1 :: Integer) > 255 then
            fail "Decimal octet value too large"
          else
            return a1
        }
-}

isAlphaChar :: Char -> Bool
isAlphaChar c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

isDigitChar :: Char -> Bool
isDigitChar c    = (c >= '0' && c <= '9')

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isSchemeChar :: Char -> Bool
isSchemeChar c = (isAlphaNumChar c) || (c `elem` "+-.")

isUserInfoChar :: Char -> Bool
isUserInfoChar = isAlphaNumChar -- undefined

isHostnameChar :: Char -> Bool
isHostnameChar = isAlphaNumChar -- undefined


oneThenMany :: (Char -> Bool) -> (Char -> Bool) -> Parser Text
oneThenMany p1 pr = do
    a1 <- A.satisfy p1
    ar <- A.takeWhile pr
    return $ a1 `cons` ar
