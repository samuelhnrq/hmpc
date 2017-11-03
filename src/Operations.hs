module Operations(
    next,
    prev,
    buffSize
) where

import Network.Simple.TCP
import Data.Maybe
import Data.ByteString.Char8 (pack, unpack)

type MPDConn = Socket

buffSize :: Int
buffSize = 1024

ok :: String
ok = "OK"

exec :: String -> MPDConn -> IO Bool
exec comm x = do
    send x (pack (comm ++ "\n")) 
    res <- recv x buffSize
    return $ (init . unpack . fromJust) res == ok

next :: MPDConn -> IO Bool
next = exec "next"

prev :: MPDConn -> IO Bool
prev = exec "prev"
