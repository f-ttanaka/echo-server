module Tool where

import Common
import NW.Client
import NW.EchoServer
import NW.UDPServer
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
    [] -> runTCPServer defaultPortNumber
    ["server"] -> runTCPServer defaultPortNumber
    ["client"] -> runTCPClient defaultPortNumber
    "server" : pn : "tcp" : _ -> runTCPServer =<< readIO pn
    "client" : pn : "tcp" : _ -> runTCPClient =<< readIO pn
    "server" : pn : "udp" : _ -> runUDPServer =<< readIO pn
    "client" : pn : "udp" : _ -> runUDPClient =<< readIO pn
    "server" : pn : _ -> runTCPServer =<< readIO pn
    "client" : pn : _ -> runTCPClient =<< readIO pn
    _ -> throwString "invalid argument."
