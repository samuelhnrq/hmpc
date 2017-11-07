module Hmpc.Operations where

import Hmpc.Networking

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
