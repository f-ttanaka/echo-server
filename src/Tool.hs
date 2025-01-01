module Tool where

import Client
import Common
import Control.Exception.Safe
import EchoServer
import Network.Socket (PortNumber)

defaultRole :: String
defaultRole = "server"

defaultPortNumber :: PortNumber
defaultPortNumber = 8080

readIO :: (Read a) => String -> IO a
readIO s = case readMaybe s of
  Just x -> return x
  _ -> throwString "invalid argument."

runTool :: IO ()
runTool = do
  args <- getArgs
  case args of
    [] -> runServer defaultPortNumber
    ["server"] -> runServer defaultPortNumber
    ["client"] -> runClient defaultPortNumber
    "server" : pn : _ -> runServer =<< readIO pn
    "client" : pn : _ -> runClient =<< readIO pn
    _ -> throwString "invalid argument."
