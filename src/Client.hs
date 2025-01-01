module Client where

import Common
import Control.Exception.Safe
import qualified Data.Text as T
import Network.Socket
import System.IO (hClose, hGetLine, hPutStr)

runClient :: PortNumber -> IO ()
runClient pn = do
  hSetBuffering stdout NoBuffering
  addr <- getAddrInfo1 pn
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
