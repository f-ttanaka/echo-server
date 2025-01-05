module NW.Client (runTCPClient, runUDPClient) where

import Common
import qualified Data.Text as T
import NW.Internal
import Network.Socket
import System.IO (hClose, hGetLine, hPutStr)

runTCPClient, runUDPClient :: PortNumber -> IO ()
runTCPClient pn = runClient pn TCP
runUDPClient pn = runClient pn UDP

runClient :: PortNumber -> CommType -> IO ()
runClient pn ct = do
  hSetBuffering stdout NoBuffering
  addr <- getAddrInfo1 pn ct
  bracket (openSocket addr) close $ \soc -> do
    connect soc (addrAddress addr)
    putStrLn "Connected to the echo server."
    -- communication loop
    bracket (socketToHandle soc ReadWriteMode) hClose communicate

communicate :: Handle -> IO ()
communicate hdl =
  catch (forever communicate1) $ \(SomeException _) -> putStrLn "\nConnection closed."
  where
    communicate1 :: IO ()
    communicate1 = do
      putStr "input> "
      msg <- getLine
      -- server cannot recognize end of inputs if there is no newline symbol.
      hPutStr hdl (T.unpack msg ++ "\n")
      -- receive response
      response <- hGetLine hdl
      putStrLn $ "Received: " ++ response
