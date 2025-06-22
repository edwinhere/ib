module IB.Network.Connection
  ( connectTo
  , appSource
  , appSink
  ) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket (HostName, PortNumber, Socket)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NBS

-- | Establishes a TCP connection to the specified host and port.
connectTo :: HostName -> PortNumber -> IO Socket
connectTo host port = do
    addr <- head <$> N.getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock <- N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
    N.connect sock (N.addrAddress addr)
    return sock
  where
    hints = N.defaultHints { N.addrSocketType = N.Stream }

-- | Creates a 'Conduit' source that reads from a 'Socket'.
appSource :: Socket -> ConduitT () ByteString IO ()
appSource sock = loop
  where
    loop = do
      bs <- liftIO $ NBS.recv sock 4096
      if BS.null bs
        then return ()
        else do
          yield bs
          loop

-- | Creates a 'Conduit' sink that writes to a 'Socket'.
appSink :: Socket -> ConduitT ByteString Void IO ()
appSink sock = awaitForever (liftIO . NBS.sendAll sock)
