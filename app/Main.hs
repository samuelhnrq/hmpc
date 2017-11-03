module Main where
import Network.Simple.TCP
import Data.Maybe (fromJust, isNothing)
import Data.ByteString.Char8 (unpack)
import Control.Monad (when)
import System.Exit (die)
import System.Environment (getArgs)
import Operations

main :: IO ()
main = do
    -- TODO: Configurable host and port
    connect "localhost" "6600" $ \(connSocket, _) -> do
      mpdVer <- recv connSocket buffSize
      when (isNothing mpdVer) $ die "Couldn't connect to server"
      putStrLn $ "Connected successfully the server responded with " ++ (init . unpack . fromJust) mpdVer
      args <- getArgs
      case head args of "next" -> next connSocket
                        "prev" -> prev connSocket
                        _ -> do putStrLn "Command not found!"; return False
      return ()
