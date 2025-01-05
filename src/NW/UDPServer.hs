module NW.UDPServer (runUDPServer) where

import Common
import NW.Internal
import Network.Socket
import Network.Socket.ByteString

runUDPServer :: PortNumber -> IO ()
runUDPServer pn = do
  addr <- getAddrInfo1 pn UDP
  socketComm addr

-- socket communication
socketComm :: AddrInfo -> IO ()
socketComm addr = bracket (openSocket addr) close $ \soc -> do
  setSocketOption soc ReuseAddr 1 -- allow reuse socket address
  bind soc (addrAddress addr)
  forever $ do
    (msg, clientAddr) <- recvFrom soc 1024
    putStrLn $ "Send: " ++ show msg
    sendTo soc msg clientAddr
