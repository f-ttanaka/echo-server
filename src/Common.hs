module Common
  ( module Relude,
    getAddrInfo1,
  )
where

import Control.Exception.Safe
import Network.Socket
import Relude

getAddrInfo1 :: PortNumber -> IO AddrInfo
getAddrInfo1 pn = do
  -- IPv4, TCP
  let aiHints =
        defaultHints
          { addrFlags = [AI_PASSIVE],
            addrSocketType = Stream,
            addrFamily = AF_INET
          }
  addrInfos <- getAddrInfo (Just aiHints) Nothing (Just $ show pn)
  case viaNonEmpty head addrInfos of
    Nothing -> throwString "No address info"
    Just addr -> return addr
