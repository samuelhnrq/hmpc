module Hmpc.Networking (MPDConn, execRaw, exec) where

import Network.Simple.TCP
import Data.Maybe
import Data.ByteString.Char8 (pack, unpack)

type MPDConn = Socket

buffSize :: Int
buffSize = 1024

ok :: String
ok = "OK"

execRaw :: String -> MPDConn -> IO String
execRaw comm x = do
    send x (pack (comm ++ "\n")) 
    res <- recv x buffSize
    return $ (init . unpack . fromJust) res

exec ::String -> MPDConn -> IO Bool
exec cmd conn = do 
    res <- execRaw cmd conn
    return $ res == ok
