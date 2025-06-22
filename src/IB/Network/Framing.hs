{-# LANGUAGE OverloadedStrings #-}

module IB.Network.Framing
  ( frame
  , unframe
  ) where

import Conduit
import Data.Binary.Get (getWord32be, runGet)
import Data.Binary.Put (putWord32be, runPut)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | A conduit that prepends each incoming 'ByteString' chunk with its
--   length, formatted as a 4-byte big-endian integer.
frame :: Monad m => ConduitT ByteString ByteString m ()
frame = awaitForever $ \payload -> do
  let len = fromIntegral (BS.length payload)
      lenBytes = LBS.toStrict $ runPut (putWord32be len)
  yield lenBytes
  yield payload

-- | A conduit that parses a stream of bytes, extracting length-prefixed
--   payloads. It handles cases where message boundaries do not align with
--   input chunk boundaries.
unframe :: Monad m => ConduitT ByteString ByteString m ()
unframe = loop BS.empty
  where
    loop buffer = do
      -- Check if we have enough data to read the length prefix.
      if BS.length buffer < 4
        then do
          -- Not enough data for the length, so we need more input.
          mbs <- await
          case mbs of
            Nothing -> return () -- End of stream.
            Just bs -> loop (buffer `BS.append` bs)
        else do
          -- We have enough data for the length prefix.
          let (lenBytes, rest) = BS.splitAt 4 buffer
              payloadLen = fromIntegral . runGet getWord32be $ LBS.fromStrict lenBytes

          -- Check if we have the complete payload.
          if BS.length rest < payloadLen
            then do
              -- Not enough data for the payload, need more input.
              mbs <- await
              case mbs of
                Nothing -> return () -- End of stream, potentially with incomplete message.
                Just bs -> loop (buffer `BS.append` bs)
            else do
              -- We have a complete message.
              let (payload, remaining) = BS.splitAt payloadLen rest
              yield payload
              loop remaining
