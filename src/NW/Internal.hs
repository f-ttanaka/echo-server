module NW.Internal where

import Common
import Network.Socket

data CommType = TCP | UDP

getAddrInfo1 :: PortNumber -> CommType -> IO AddrInfo
getAddrInfo1 pn ct = do
  -- IPv4, TCP
  let socType = case ct of
        TCP -> Stream
        UDP -> Datagram
      aiHints =
        defaultHints
          { addrFlags = [AI_PASSIVE],
            addrSocketType = socType,
            addrFamily = AF_INET
          }
  addrInfos <- getAddrInfo (Just aiHints) Nothing (Just $ show pn)
  case viaNonEmpty head addrInfos of
    Nothing -> throwString "No address info"
    Just addr -> return addr
