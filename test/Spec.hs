{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec (main, spec) where

import           Test.Hspec
import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word32)
import IB.Codec.Decoder (decodeMessages)
import IB.Codec.Encoder (encodeMessages)
import IB.Network.Framing (unframe)
import IB.Protocol.Types (ClientMessage (..), ServerMessage (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Codec and Framing" $ do
    it "correctly unframes and decodes a single, complete server message" $ do
      -- 1. The message we expect to receive from the server.
      let expectedMessage = CurrentTime 1609459200
          serverPayload = LBS.toStrict $ B.toLazyByteString $
                            B.string8 "49\0" <> -- Message ID for CurrentTime
                            B.string8 "1\0" <>  -- Version
                            B.string8 "1609459200\0" -- The timestamp

      -- 2. The raw bytes on the wire (length-prefixed payload).
      let framedPayload = frameIt serverPayload

      -- 3. Run the framed bytes through our decoding pipeline.
      decodedMessages <- runConduit $
        yield framedPayload
          .| unframe
          .| decodeMessages
          .| sinkList

      -- 4. Assert that the result is what we expect.
      decodedMessages `shouldBe` [expectedMessage]

    it "correctly unframes and decodes a NextValidId message" $ do
      let expectedMessage = NextValidId 123
          serverPayload = LBS.toStrict $ B.toLazyByteString $
                            B.string8 "9\0" <> -- Message ID for NextValidId
                            B.string8 "1\0" <>  -- Version
                            B.string8 "123\0" -- The order ID

      let framedPayload = frameIt serverPayload

      decodedMessages <- runConduit $
        yield framedPayload
          .| unframe
          .| decodeMessages
          .| sinkList

      decodedMessages `shouldBe` [expectedMessage]

  describe "Encoder" $ do
    it "correctly encodes a ReqIds message" $ do
      let clientMessage = ReqIds
          expectedPayload = LBS.toStrict $ B.toLazyByteString $
                              B.string8 "8\0" <>
                              B.string8 "1\0" <>
                              B.string8 "1\0"

      [encoded] <- runConduit $
        yield clientMessage
          .| encodeMessages
          .| sinkList

      encoded `shouldBe` expectedPayload

  describe "Integration Tests" $ do
    it "should have integration tests available" $ do
      -- This is a placeholder for integration tests
      -- The actual integration tests are in IntegrationTest.hs
      -- and can be run separately or included in the build
      True `shouldBe` True

-- Helper to manually frame a payload with a 4-byte big-endian length.
frameIt :: ByteString -> ByteString
frameIt payload =
  let len = fromIntegral (LBS.length (LBS.fromStrict payload)) :: Word32
      lenBytes = LBS.toStrict $ B.toLazyByteString (B.word32BE len)
  in lenBytes <> payload
