module EchoServer (runServer) where

import Common
import Control.Concurrent
import Control.Exception.Safe
import Network.Socket
import System.IO (hClose, hGetLine, hPutStr)

runServer :: PortNumber -> IO ()
runServer pn = do
  addr <- getAddrInfo1 pn
  socketComm addr processClient

-- socket communication
socketComm :: AddrInfo -> (Handle -> IO ()) -> IO ()
socketComm addr action = bracket (openSocket addr) close $ \soc -> do
  setSocketOption soc ReuseAddr 1 -- allow reuse socket address
  bind soc (addrAddress addr)
  listen soc 5
  forever $ do
    conn <- fst <$> accept soc
    forkIO $ bracket (socketToHandle conn ReadWriteMode) hClose action

-- process client connection
-- receive client socket handle
processClient :: Handle -> IO ()
processClient clh = do
  putStrLn "Client connected."
  catch (forever echo) $ \(SomeException _) -> putStrLn "\nConnection closed."
  where
    echo :: IO ()
    echo = do
      msg <- hGetLine clh
      putStrLn ("Send: " ++ msg)
      -- client cannot recognize end of inputs if there is no newline symbol.
      hPutStr clh (msg ++ "\n")
