module Hmpc.Operations where

import Network.Simple.TCP
import Data.Maybe
import Data.Char (toUpper)
import Data.ByteString.Char8 (pack, unpack)
import Data.Either (rights)
import Data.Ini
import qualified Data.Map.Strict as M
import qualified Data.Text as T

type MPDConn = Socket
type Status = M.Map String String
data State = Play | Stop | Pause deriving (Eq, Show, Enum, Read)

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

-- | Skips the current playing song in given MPD connection
-- returns whether the MPD server responded with an OK
next :: MPDConn -> IO Bool
next = exec "next"

-- | Rewind to the previous song in given MPD connection
-- returns whether the MPD server responded with an OK
prev :: MPDConn -> IO Bool
prev = exec "previous"

play :: MPDConn -> IO Bool
play = exec "pause 0"

pause :: MPDConn -> IO Bool
pause = exec "pause 1"

getState :: Status -> State
getState x = read state :: State
    where (frst:rest) = (x M.! "state"); state = (toUpper frst) : rest

status :: MPDConn -> IO Status
status conn = do
    txt <- execRaw "status" conn
    let iniTxt = "[STATUS]\n" ++ txt
        (Right ini) = parseIni $ T.pack iniTxt
        section = T.pack "STATUS"
        (Right statusKeys) = keys section ini
        vals = rights [lookupValue section x ini | x <- statusKeys]
        final = zipWith (\x y -> (T.unpack x, T.unpack y)) statusKeys vals
    return $ M.fromList final
