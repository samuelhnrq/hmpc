module Main where
import Network.Simple.TCP
import Data.List (isSuffixOf)
import Data.Maybe (isNothing)
import Control.Monad (when)
import System.Exit (die)
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import Hmpc.Operations

main :: IO ()
main = do
  -- TODO: Configurable host and port
  catch (connect "localhost" "6600" (\(mpdConn, _) -> do
    mpdVer <- recv mpdConn buffSize
    when (isNothing mpdVer) $ die "Server isn't responding."
    args <- getArgs
    when (null args) $ die "Type the desired command"
    currStatus <- status mpdConn
    _ <- handleCmd currStatus (head args) (tail args) mpdConn
    return ()  )) failedConnection

failedConnection :: SomeException -> IO ()
failedConnection err = do 
  putStrLn "Error while trying to connect to mpd server"
  if "(Connection refused)" `isSuffixOf` (show err) then 
    putStrLn "Connection was refused, is the MPD server running?"
  else 
    print err

-- | Each different implementation of handleCmd is a possible command line argument
handleCmd :: Status -> String -> [String] -> MPDConn -> IO Bool
handleCmd _ "next" _ = next

handleCmd _ "prev" _ = prev
handleCmd a "previous" _ = handleCmd a "prev" []

handleCmd _ "play" _ = play

handleCmd _ "pause" _ = pause

handleCmd sts "toggle" _ = if (getState sts) == Play then pause else play

handleCmd _ _ _ = \_ -> return False
