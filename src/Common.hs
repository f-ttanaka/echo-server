module Common
  ( module Relude,
    getAddrInfo1,
  )
where

import Control.Exception.Safe
import Network.Socket
import Relude

getAddrInfo1 :: IO AddrInfo
getAddrInfo1 = do
  -- IPv4, TCP
  let aiHints =
        defaultHints
          { addrFlags = [AI_PASSIVE],
            addrSocketType = Stream,
            addrFamily = AF_INET
          }
  addrInfos <- getAddrInfo (Just aiHints) Nothing (Just "8080")
  case viaNonEmpty head addrInfos of
    Nothing -> throwString "No address info"
    Just addr -> return addr
