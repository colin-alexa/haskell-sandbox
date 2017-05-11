import Control.Exception.Base (bracket)

import Data.List (intercalate)
import Data.Map.Strict hiding (map)
import Data.Maybe (catMaybes)

import Network
import Network.Socket (close)

import System.IO

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

data HTTPResponse = HTTPResponse
    { code :: Int
    , headers :: Map String String
    , message :: String
    }

autoHeaders :: [(String, HTTPResponse -> String)]
autoHeaders = [ ("Content-Length", contentLength)
              ]

contentLength :: HTTPResponse -> String
contentLength = show . length . message

http_OK, http_BADREQUEST, http_NOTFOUND, http_SERVERERROR :: Int
http_OK = 200
http_REDIRECT = 308
http_BADREQUEST = 400
http_NOTFOUND = 404
http_SERVERERROR = 500

okMessage :: String -> HTTPResponse
okMessage = HTTPResponse http_OK (fromList [])

setAutoHeaders :: HTTPResponse -> HTTPResponse
setAutoHeaders hr@(HTTPResponse c headers m) = HTTPResponse c headers' m
    where
        autoHeaderMap = fromList [(k, f hr) | (k, f) <- autoHeaders]
        headers'      = union autoHeaderMap headers

setHeader :: HTTPResponse -> String -> String -> HTTPResponse
setHeader (HTTPResponse c headers m) = \h val -> HTTPResponse c (insert h val headers) m

instance Show HTTPResponse where
    show hr = firstLine ++
              crlf ++
              headerLines ++
              crlf ++
              crlf ++
              message hr
        where
            crlf     = "\r\n"
            firstLine   = "HTTP/1.0 " ++ (show . code) hr
            headers'    = map ((intercalate ": ") . pairToList) $ toList (headers hr)
            headerLines = intercalate crlf headers'

-- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
-- | like `try { } finally { }`, bracket wraps an IO action with a cleanup callback

withSocket :: PortID -> (Socket -> IO a) -> IO a
withSocket pn = bracket (listenOn pn) close

route :: Handle -> IO ()
route conn = do
    hSetBuffering conn LineBuffering
    recvd <- hGetLine conn
    hPutStrLn conn (show . okMessage $ "Hello, World!\nYou sent:\n\n" ++ recvd)
    hClose conn

serveOn :: Socket -> IO ()
serveOn sock = do
    (connectionHandle, hostname, port) <- accept sock
    route connectionHandle
    serveOn sock

main = withSocket (PortNumber 5000) serveOn
