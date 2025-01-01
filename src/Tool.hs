module Tool where

import Client
import Common
import Control.Exception.Safe
import EchoServer

runTool :: IO ()
runTool = do
  args <- getArgs
  case args of
    [] -> runServer
    "server" : _ -> runServer
    "client" : _ -> runClient
    _ -> throwString "invalid argument."
