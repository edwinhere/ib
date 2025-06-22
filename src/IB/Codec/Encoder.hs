{-# LANGUAGE OverloadedStrings #-}

module IB.Codec.Encoder
  ( encodeMessages
  , putField
  , putFieldT
  , putFieldS
  ) where

import Conduit
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import IB.Protocol.Constants
import IB.Protocol.Types
import Data.Maybe (fromMaybe)

-- | A conduit that encodes 'ClientMessage's into 'ByteString' payloads.
encodeMessages :: Monad m => ConduitT ClientMessage ByteString m ()
encodeMessages = awaitForever $ \msg ->
  yield $ LBS.toStrict $ B.toLazyByteString (messageBuilder msg)

-- | Constructs a 'Builder' for a given 'ClientMessage'.
messageBuilder :: ClientMessage -> B.Builder
messageBuilder ReqCurrentTime =
  putField (messageIdToBuilder reqCurrentTime) <>
  putField (B.intDec 1) -- Version
messageBuilder ReqIds =
  putField (messageIdToBuilder reqIds) <>
  putField (B.intDec 1) <> -- Version
  putField (B.intDec 1)    -- numIds (deprecated, but required)
messageBuilder (ReqContractDetails reqId c) =
  putField (messageIdToBuilder reqContractData) <>
  putFieldS (8 :: Int) <> -- Version
  putFieldS reqId <>
  putFieldS (fromMaybe 0 (conId c)) <>
  putFieldT (symbol c) <>
  putField (B.string7 $ show (secType c)) <>
  putFieldT (lastTradeDateOrContractMonth c) <>
  putFieldS (strike c) <>
  putField (maybe mempty (B.string7 . show) (right c)) <>
  putFieldT (multiplier c) <>
  putFieldT (exchange c) <>
  putFieldT (primaryExchange c) <>
  putFieldT (currency c) <>
  putFieldT (localSymbol c) <>
  putFieldT (tradingClass c) <>
  putFieldS (if includeExpired c then 1 :: Int else 0) <>
  putFieldT (secIdType c) <>
  putFieldT (secId c) <>
  putFieldT (issuerId c)
messageBuilder ReqManagedAccts =
  putField (messageIdToBuilder reqManagedAccts) <>
  putField (B.intDec 1) -- Version
messageBuilder (ReqHistoricalData req) =
  let c = histContract req
  in
  putField (messageIdToBuilder reqHistoricalData) <>
  -- Version 6 is omitted for modern servers
  putFieldS (histRequestId req) <>
  -- Start of contract fields (13 fields, like reqHeadTimestamp)
  putFieldS (fromMaybe 0 (conId c)) <>
  putFieldT (symbol c) <>
  putField (B.string7 $ show (secType c)) <>
  putFieldT (lastTradeDateOrContractMonth c) <>
  putFieldS (strike c) <>
  putField (maybe mempty (B.string7 . show) (right c)) <>
  putFieldT (multiplier c) <>
  putFieldT (exchange c) <>
  putFieldT (primaryExchange c) <>
  putFieldT (currency c) <>
  putFieldT (localSymbol c) <>
  putFieldT (tradingClass c) <>
  putFieldS (if includeExpired c then 1 :: Int else 0) <>
  -- End of contract fields
  putFieldT (histEndDate req) <>
  putFieldT (histBarSize req) <>
  putFieldT (histDuration req) <>
  putFieldS (if histUseRth req then 1 :: Int else 0) <>
  putFieldT (histWhatToShow req) <>
  putFieldS (histFormatDate req) <>
  putFieldS (if histKeepUpToDate req then 1 :: Int else 0) <>
  putField "" -- chartOptions (empty for now)
messageBuilder (ReqMktData req) =
  let c = mktDataContract req
  in
  putField (messageIdToBuilder reqMktData) <>
  putFieldS (11 :: Int) <> -- Version
  putFieldS (mktDataRequestId req) <>
  -- Start of contract
  putFieldS (fromMaybe 0 (conId c)) <>
  putFieldT (symbol c) <>
  putFieldS (secType c) <>
  putFieldT (lastTradeDateOrContractMonth c) <>
  putFieldS (strike c) <>
  putField (maybe mempty (B.string7 . show) (right c)) <>
  putFieldT (multiplier c) <>
  putFieldT (exchange c) <>
  putFieldT (primaryExchange c) <>
  putFieldT (currency c) <>
  putFieldT (localSymbol c) <>
  putFieldT (tradingClass c) <>
  putFieldS (if includeExpired c then 1 :: Int else 0) <>
  putFieldT (secIdType c) <>
  putFieldT (secId c) <>
  putFieldT (issuerId c) <>
  -- End of contract
  putFieldT (mktDataGenericTickList req) <>
  putFieldS (if mktDataSnapshot req then 1 :: Int else 0) <>
  putFieldS (if mktDataRegulatorySnapshot req then 1 :: Int else 0) <>
  putField "" -- mktDataOptions
messageBuilder (StartApi req) =
  putField (messageIdToBuilder startApi) <>
  putField (B.intDec 2) <> -- Version
  putField (B.intDec (clientId req)) <>
  putField (maybe mempty (B.byteString . T.encodeUtf8) (optionalCapabilities req))

-- | A helper function to create a null-terminated 'Builder' from another 'Builder'.
putField :: B.Builder -> B.Builder
putField b = b <> B.word8 0

-- Overload putField for Text
putFieldT :: Text -> B.Builder
putFieldT = putField . B.byteString . T.encodeUtf8

-- Overload putField for Show instances
putFieldS :: Show a => a -> B.Builder
putFieldS = putField . B.string7 . show

-- | Converts a 'MessageId' to a 'Builder'.
messageIdToBuilder :: MessageId -> B.Builder
messageIdToBuilder (MessageId mid) = B.word16Dec mid
