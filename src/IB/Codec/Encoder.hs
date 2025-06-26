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

-- Market Depth
messageBuilder (ReqMktDepth req) =
  let c = mktDepthContract req
  in
  putField (messageIdToBuilder reqMktDepth) <>
  putFieldS (5 :: Int) <> -- Version
  putFieldS (mktDepthRequestId req) <>
  -- Contract fields (same as reqMktData)
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
  putFieldS (mktDepthNumRows req) <>
  putFieldS (if mktDepthIsSmartDepth req then 1 :: Int else 0) <>
  putField "" -- mktDepthOptions (empty for now)

messageBuilder (CancelMktDepth reqId isSmartDepth) =
  putField (messageIdToBuilder cancelMktDepth) <>
  putFieldS (1 :: Int) <> -- Version
  putFieldS reqId <>
  putFieldS (if isSmartDepth then 1 :: Int else 0)

-- Real-time Bars
messageBuilder (ReqRealTimeBars req) =
  let c = realTimeBarsContract req
  in
  putField (messageIdToBuilder reqRealTimeBars) <>
  putFieldS (3 :: Int) <> -- Version
  putFieldS (realTimeBarsRequestId req) <>
  -- Contract fields
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
  putFieldS (realTimeBarsBarSize req) <>
  putFieldT (realTimeBarsWhatToShow req) <>
  putFieldS (if realTimeBarsUseRth req then 1 :: Int else 0) <>
  putField "" -- realTimeBarsOptions (empty for now)

messageBuilder (CancelRealTimeBars reqId) =
  putField (messageIdToBuilder cancelRealTimeBars) <>
  putFieldS (1 :: Int) <> -- Version
  putFieldS reqId

-- Tick-by-Tick Data
messageBuilder (ReqTickByTickData req) =
  let c = tickByTickContract req
      tickTypeStr = case tickByTickType req of
        TickLast -> "Last"
        TickAllLast -> "AllLast"
        TickBidAsk -> "BidAsk"
        TickMidPoint -> "MidPoint"
  in
  putField (messageIdToBuilder reqTickByTickData) <>
  putFieldS (tickByTickRequestId req) <>
  -- Contract fields
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
  putFieldT tickTypeStr <>
  putFieldS (tickByTickNumberOfTicks req) <>
  putFieldS (if tickByTickIgnoreSize req then 1 :: Int else 0)

messageBuilder (CancelTickByTickData reqId) =
  putField (messageIdToBuilder cancelTickByTickData) <>
  putFieldS reqId

-- Option Calculations
messageBuilder (ReqCalcImpliedVolatility req) =
  let c = optionCalcContract req
  in
  putField (messageIdToBuilder reqCalcImpliedVolat) <>
  putFieldS (3 :: Int) <> -- Version
  putFieldS (optionCalcRequestId req) <>
  -- Contract fields
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
  putFieldS (optionCalcOptionPrice req) <>
  putFieldS (optionCalcUnderlyingPrice req) <>
  putField "" -- implVolOptions (empty for now)

messageBuilder (ReqCalcOptionPrice req) =
  let c = optionCalcContract req
  in
  putField (messageIdToBuilder reqCalcOptionPrice) <>
  putFieldS (4 :: Int) <> -- Version
  putFieldS (optionCalcRequestId req) <>
  -- Contract fields
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
  putFieldS (optionCalcVolatility req) <>
  putFieldS (optionCalcUnderlyingPrice req) <>
  putField "" <> -- customerAccount (empty for now)
  putFieldS (0 :: Int) <> -- professionalCustomer (false)
  putField "" -- optPrcOptions (empty for now)

messageBuilder (CancelCalculateImpliedVolatility reqId) =
  putField (messageIdToBuilder cancelCalcImpliedVolat) <>
  putFieldS (1 :: Int) <> -- Version
  putFieldS reqId

messageBuilder (CancelCalculateOptionPrice reqId) =
  putField (messageIdToBuilder cancelCalcOptionPrice) <>
  putFieldS (1 :: Int) <> -- Version
  putFieldS reqId

-- Account and Position Requests
messageBuilder (ReqAccountSummary req) =
  putField (messageIdToBuilder reqAccountSummary) <>
  putFieldS (1 :: Int) <> -- Version
  putFieldS (accountSummaryRequestId req) <>
  putFieldT (accountSummaryGroup req) <>
  putFieldT (accountSummaryTags req)

messageBuilder (CancelAccountSummary reqId) =
  putField (messageIdToBuilder cancelAccountSummary) <>
  putFieldS (1 :: Int) <> -- Version
  putFieldS reqId

messageBuilder (ReqPositions _) =
  putField (messageIdToBuilder reqPositions) <>
  putFieldS (1 :: Int) -- Version

messageBuilder CancelPositions =
  putField (messageIdToBuilder cancelPositions) <>
  putFieldS (1 :: Int) -- Version

messageBuilder (ReqPnL req) =
  putField (messageIdToBuilder reqPnl) <>
  putFieldS (pnlRequestId req) <>
  putFieldT (pnlAccount req) <>
  putFieldT (pnlModelCode req)

messageBuilder (CancelPnL reqId) =
  putField (messageIdToBuilder cancelPnl) <>
  putFieldS reqId

-- Place Order - Updated to match official Python API exactly
messageBuilder (PlaceOrder req) =
  buildPlaceOrderMessage 187 req  -- Assume modern server version 187 for now

-- | Build PlaceOrder message with proper server version checking
buildPlaceOrderMessage :: ServerVersion -> PlaceOrderRequest -> B.Builder
buildPlaceOrderMessage serverVer req =
  let c = placeOrderContract req
      o = placeOrderOrder req
      -- Determine version field based on server version
      version = if serverVer < minServerVerNotHeld then 27 else 45
  in
  putField (messageIdToBuilder placeOrder) <>
  -- Version field (conditionally included)
  (if serverVer < minServerVerOrderContainer
   then putFieldS version
   else mempty) <>
  putFieldS (placeOrderId req) <>
  -- Contract fields with conditional inclusion
  (if serverVer >= minServerVerPlaceOrderConId
   then putFieldS (fromMaybe 0 (conId c))
   else mempty) <>
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
  (if serverVer >= minServerVerTradingClass
   then putFieldT (tradingClass c)
   else mempty) <>
  (if serverVer >= minServerVerSecIdType
   then putFieldT (secIdType c) <> putFieldT (secId c)
   else mempty) <>
  -- Main order fields
  putFieldS (orderAction o) <>
  (if serverVer >= minServerVerFractionalPositions
   then putFieldS (orderTotalQuantity o)  -- Send as string for fractional shares
   else putFieldS (round (orderTotalQuantity o) :: Int)) <> -- Send as int for older servers
  putFieldS (orderType o) <>
  putField (maybe "" (B.string7 . show) (orderLmtPrice o)) <> -- Empty string for MAX_VALUE
  putField (maybe "" (B.string7 . show) (orderAuxPrice o)) <> -- Empty string for MAX_VALUE
  -- Extended order fields
  putFieldS (orderTif o) <>
  putFieldT (orderOcaGroup o) <>
  putFieldT (orderAccount o) <>
  putFieldT (orderOpenClose o) <>
  putFieldS (orderOrigin o) <>
  putFieldT (orderRef o) <>
  putFieldS (if orderTransmit o then 1 :: Int else 0) <>
  putFieldS (fromMaybe 0 (orderParentId o)) <>
  putFieldS (if orderBlockOrder o then 1 :: Int else 0) <>
  putFieldS (if orderSweepToFill o then 1 :: Int else 0) <>
  putFieldS (fromMaybe 0 (orderDisplaySize o)) <>
  putFieldS (orderTriggerMethod o) <>
  putFieldS (if orderOutsideRth o then 1 :: Int else 0) <>
  putFieldS (if orderHidden o then 1 :: Int else 0) <>
  -- Combo legs - empty for non-BAG orders
  putField "" <> -- sharesAllocation (deprecated, always empty)
  putField "" <> -- discretionaryAmt (MAX_VALUE -> empty)
  putFieldT (orderGoodAfterTime o) <>
  putFieldT (orderGoodTillDate o) <>
  putFieldT (orderFaGroup o) <>
  putFieldT (orderFaMethod o) <>
  putFieldT (orderFaPercentage o) <>
  putFieldT (orderModelCode o) <>
  putFieldS (orderShortSaleSlot o) <>
  putFieldT (orderDesignatedLocation o) <>
  putFieldS (orderExemptCode o) <>
  putFieldS (orderOcaType o) <>
  putFieldT (orderRule80A o) <>
  putFieldT (orderSettlingFirm o) <>
  putFieldS (if orderAllOrNone o then 1 :: Int else 0) <>
  putField (maybe "" (B.string7 . show) (orderMinQty o)) <> -- Empty string for MAX_VALUE
  putField (maybe "" (B.string7 . show) (orderPercentOffset o)) <> -- Empty string for MAX_VALUE
  putFieldS (0 :: Int) <> -- auctionStrategy (default 0)
  putField "" <> -- startingPrice (MAX_VALUE -> empty)
  putField "" <> -- stockRefPrice (MAX_VALUE -> empty)
  putField "" <> -- delta (MAX_VALUE -> empty)
  putField "" <> -- stockRangeLower (MAX_VALUE -> empty)
  putField "" <> -- stockRangeUpper (MAX_VALUE -> empty)
  putFieldS (if orderOverridePercentageConstraints o then 1 :: Int else 0) <>
  -- Volatility order fields (all MAX_VALUE/defaults)
  putField "" <> -- volatility (MAX_VALUE -> empty)
  putField "" <> -- volatilityType (MAX_VALUE -> empty)
  putField "" <> -- deltaNeutralOrderType (empty)
  putField "" <> -- deltaNeutralAuxPrice (MAX_VALUE -> empty)
  putFieldS (0 :: Int) <> -- continuousUpdate (default 0)
  putField "" <> -- referencePriceType (MAX_VALUE -> empty)
  -- Scale order fields
  putField (maybe "" (B.string7 . show) (orderTrailStopPrice o)) <>
  putField (maybe "" (B.string7 . show) (orderTrailingPercent o)) <>
  putField "" <> -- scaleInitLevelSize (MAX_VALUE -> empty)
  putField "" <> -- scaleSubsLevelSize (MAX_VALUE -> empty)
  putField "" <> -- scalePriceIncrement (MAX_VALUE -> empty)
  -- Advanced fields with conditional inclusion
  (if serverVer >= minServerVerHedgeOrders
   then putField "" <> putField "" -- hedgeType, hedgeParam (empty)
   else mempty) <>
  putFieldS (0 :: Int) <> -- optOutSmartRouting (false)
  putFieldT (orderClearingAccount o) <>
  putFieldT (orderClearingIntent o) <>
  (if serverVer >= minServerVerNotHeld
   then putFieldS (if orderNotHeld o then 1 :: Int else 0)
   else mempty) <>
  (if serverVer >= minServerVerAlgoOrders
   then putField "" <> putField "" -- algoStrategy, algoId (empty)
   else mempty) <>
  putFieldS (if orderWhatIf o then 1 :: Int else 0) <>
  (if serverVer >= minServerVerOrderSolicited
   then putFieldS (if orderSolicited o then 1 :: Int else 0)
   else mempty) <>
  putFieldS (if orderRandomizeSize o then 1 :: Int else 0) <>
  putFieldS (if orderRandomizePrice o then 1 :: Int else 0)

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
