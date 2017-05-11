module Web.SimpleServer.HTTPRequest (
      HTTPRequest(..)
    , Method(..)
    , getHttpRequest
) where

import Control.Applicative
import Control.Monad                 (liftM4)
import Data.Map.Strict               hiding (map)
import Numeric                       (readHex)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import System.IO                     (Handle)

data Method = Get | Post | Delete
          deriving (Eq, Ord, Read, Show)

data HTTPRequest = HTTPRequest {
      reqMethod :: Method
    , reqURL :: String
    , reqHeaders :: Map String String
    , reqBody :: Maybe String
    } deriving (Eq, Read, Show)


-- from the Haskell O'Reilly book. Huh!
parseRequest :: CharParser () HTTPRequest
parseRequest = pRequest' "GET" Get (pure Nothing)
           <|> pRequest' "DELETE" Delete (pure Nothing)
           <|> pRequest' "POST" Post (Just <$> many anyChar)

pRequest' :: String -> Method -> CharParser () (Maybe String) -> CharParser () HTTPRequest
pRequest' name ctor = liftM4 HTTPRequest parseMethod parseUrl parseHeaders
  where 
    parseMethod = ctor <$ string name <* char ' '
    parseUrl    = optional (char '/') *>
                  manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
                  <* crlf
                                      

parseHeaders :: CharParser st (Map String String)
parseHeaders = fromList <$> (header `manyTill` crlf)
  where header       = liftA2 (,) fieldName (char ':' *> spaces *> contents)
        contents     = liftA2 (++) (many1 notEOL <* crlf)
                                   (continuation <|> pure [])
        continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents
        fieldName    = (:) <$> letter <*> many fieldChar
        fieldChar    = letter <|> digit <|> oneOf "-_"

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"

getHttpRequest :: String -> Either ParseError HTTPRequest
getHttpRequest = parse parseRequest ""
