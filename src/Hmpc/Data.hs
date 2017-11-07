module Hmpc.Data (
    Song,
    State(..),
    Status(..),
    getStatus
)where

import Hmpc.Networking
import qualified Data.Map.Strict as M hiding ((!))
import Data.Char (toUpper)

type Song = M.Map String String
data State = Play | Stop | Pause deriving (Eq, Show, Enum, Read)
data Status = Status { volume :: Int
                     , repeatSong :: Bool
                     , random :: Bool
                     , single :: Bool
                     , consume :: Bool
                     , playlist :: Int
                     , playlistLength :: Int
                     , mixRampDB :: Double
                     , state :: State
                     , song :: Int
                     , songID :: Int
                     , time :: String
                     , elapsed :: Double
                     , bitRate :: Int
                     , dutation :: Double
                     , audio :: String
                     , nextSong :: Int
                     , nextSongID :: Int } deriving (Show)

instance Read Status where
    readsPrec _ input =
        let vals = [let (_, val) = span (/= ' ') j in tail val | j <- lines input]
        in
            [(Status { volume = read (head vals)::Int
                     , repeatSong = isOne (vals !! 1)
                     , random = isOne (vals !! 2)
                     , single = isOne (vals !! 3)
                     , consume = isOne (vals !! 4)
                     , playlist = read (vals !! 5)::Int
                     , playlistLength = read (vals !! 6)::Int
                     , mixRampDB = read (vals !! 7)::Double
                     , state = read (capitalize (vals !! 8))::State
                     , song = read (vals !! 9)::Int
                     , songID = read (vals !! 10)::Int
                     , time = vals !! 11
                     , elapsed = read (vals !! 12)::Double
                     , bitRate = read (vals !! 13)::Int
                     , dutation = read (vals !! 14)::Double
                     , audio = vals !! 15
                     , nextSong = read (vals !! 16)::Int
                     , nextSongID = read (vals !! 17)::Int}, 
            unlines (drop 18 (lines input)))]
        where isOne = (== "1")
              capitalize (x:rest) = toUpper x : rest

getStatus :: MPDConn -> IO Status
getStatus conn = do
    txt <- execRaw "status" conn
    return (read txt::Status)
