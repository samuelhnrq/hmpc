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
    connect "localhost" "6600" $ \(mpdConn, _) -> do
      mpdVer <- recv mpdConn buffSize
      when (isNothing mpdVer) $ die "Couldn't connect to server"
      args <- getArgs
      when (null args) $ die "Type the desired command"
      handleCmd (head args) (tail args) mpdConn
      return ()

handleCmd :: String -> [String] -> MPDConn -> IO Bool
handleCmd "next" _ = next
handleCmd "prev" _ = prev
handleCmd "previous" _ = handleCmd "prev" []
handleCmd _ _ = \_ -> return False
