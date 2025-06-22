{-# LANGUAGE OverloadedStrings #-}

module IB.Codec.Decoder
  ( decodeMessages
  , getField
  ) where

import Conduit
import Control.Monad (void, replicateM)
import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import IB.Protocol.Constants
import IB.Protocol.Types
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-- | A conduit that parses unframed 'ByteString' payloads into 'ServerMessage's.
decodeMessages :: MonadIO m => ConduitT ByteString ServerMessage m ()
decodeMessages = awaitForever $ \payload -> do
  -- We allow for trailing nulls, which the server sometimes sends on info messages.
  let robustParser = messageParser <* A.many' (A.satisfy (== '\0')) <* endOfInput
  case parseOnly robustParser payload of
    Left err -> do
      liftIO $ putStrLn $ "Dropping unparseable message. Error: " ++ err ++ ". Payload: " ++ show payload
    Right msg -> yield msg

-- | Top-level parser that dispatches to the correct message parser based on the message ID.
messageParser :: Parser ServerMessage
messageParser = do
  messageIdInt <- getFieldAsSafeRead 0
  let msgId = MessageId messageIdInt
  if msgId == historicalData
    then parseHistoricalData
    else parseById msgId

parseById :: MessageId -> Parser ServerMessage
parseById msgId
  | msgId == tickPrice = parseTickPrice
  | msgId == tickSize = parseTickSize
  | msgId == errMsg = parseError
  | msgId == contractData = parseContractData
  | msgId == contractDataEnd = parseContractDataEnd
  | msgId == managedAccts = parseManagedAccounts
  | msgId == nextValidId = parseNextValidId
  | msgId == currentTime = parseCurrentTime
  | msgId == historicalData = parseHistoricalData
  | msgId == symbolSamples = parseSymbolSamples
  | msgId == tickEFP = parseTickEFP
  | msgId == marketDataType = parseMarketDataType
  | msgId == tickByTick = parseTickByTick
  | otherwise = fail $ "Unknown message ID: " ++ show msgId

-- | Parses a 'CurrentTime' message.
-- Format: [MessageID, Version, Time]
-- We only care about the final field.
parseCurrentTime :: Parser ServerMessage
parseCurrentTime = do
  _version <- getField
  unixTime <- getFieldAsSafeRead 0
  return $ CurrentTime unixTime

-- | Parses a 'TickPriceData' message.
-- Format: [MessageID, Version, RequestID, TickType, Price, Size, Attribs]
parseTickPrice :: Parser ServerMessage
parseTickPrice = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  field <- getFieldAsSafeRead (toEnum 0)
  price <- getFieldAsSafeRead 0.0
  size <- getFieldAsSafeRead 0
  attribs <- getFieldAsSafeRead 0
  let tick = TickPriceData reqId field price size attribs
  return $ TickPrice tick

-- | Parses a 'TickSizeData' message.
-- Format: [MessageID, Version, RequestID, TickType, Size]
parseTickSize :: Parser ServerMessage
parseTickSize = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  field <- getFieldAsSafeRead (toEnum 0)
  size <- getFieldAsSafeRead 0
  let tick = TickSizeData reqId field size
  return $ TickSize tick

-- | Parses a 'NextValidId' message.
-- Format: [MessageID, Version, OrderID]
parseNextValidId :: Parser ServerMessage
parseNextValidId = do
  _version <- getField
  orderId <- getFieldAsSafeRead 0
  return $ NextValidId orderId

-- | Parses a 'ManagedAccounts' message.
-- Format: [MessageID, Version, AccountsList]
parseManagedAccounts :: Parser ServerMessage
parseManagedAccounts = do
  _version <- getField
  accountsList <- T.splitOn "," . T.decodeUtf8 <$> getField
  return $ ManagedAccounts accountsList

-- | Parses a 'HistoricalData' message (a series of bars).
-- Format: [MessageID, Version?, ReqID, StartDate, EndDate, BarCount, [BarData...]]
-- Note: Version field is only present for server versions < 124 (MIN_SERVER_VER_SYNT_REALTIME_BARS)
parseHistoricalData :: Parser ServerMessage
parseHistoricalData = do
  -- For modern servers (>= 124), there's no version field
  -- For older servers, we'd need to read the version field here
  reqId <- getFieldAsSafeRead 0
  _startDate <- getField
  _endDate <- getField
  numBars <- getFieldAsSafeRead 0
  bars <- replicateM numBars parseBar
  return $ HistoricalDataResponse reqId bars

parseBar :: Parser Bar
parseBar = do
  date <- T.decodeUtf8 <$> getField
  open <- getFieldAsSafeRead 0
  high <- getFieldAsSafeRead 0
  low <- getFieldAsSafeRead 0
  close <- getFieldAsSafeRead 0
  volume <- getFieldAsSafeRead 0
  wap <- getFieldAsSafeRead 0
  -- For modern servers (>= 124), there's no hasGaps field
  -- For older servers, we'd need to read hasGaps here
  barCount'' <- getFieldAsSafeRead (-1)
  return $ Bar date open high low close volume wap barCount''

-- | Parses a 'ContractData' message.
-- Format is complex and version-dependent. This is a simplified v8 parser.
parseContractData :: Parser ServerMessage
parseContractData = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  
  -- Contract fields
  conId' <- getFieldAsSafeRead Nothing
  symbol' <- T.decodeUtf8 <$> getField
  secType' <- getFieldAsSafeRead (toEnum 0)
  lastTradeDateOrContractMonth' <- T.decodeUtf8 <$> getField
  strike' <- getFieldAsSafeRead 0
  right' <- getFieldAsSafeRead Nothing
  _multiplier <- getField -- Not in our Contract record, but in the message
  exchange' <- T.decodeUtf8 <$> getField
  currency' <- T.decodeUtf8 <$> getField
  localSymbol' <- T.decodeUtf8 <$> getField
  marketName' <- T.decodeUtf8 <$> getField
  tradingClass' <- T.decodeUtf8 <$> getField
  
  -- ContractDetails fields
  minTick' <- getFieldAsSafeRead 0
  _mdSizeMultiplier <- getField
  orderTypes' <- T.decodeUtf8 <$> getField
  validExchanges' <- T.decodeUtf8 <$> getField
  priceMagnifier' <- getFieldAsSafeRead 0
  
  let contract = Contract
        { conId = conId'
        , symbol = symbol'
        , secType = secType'
        , lastTradeDateOrContractMonth = lastTradeDateOrContractMonth'
        , strike = strike'
        , right = right'
        , multiplier = "" -- Not in this response
        , exchange = exchange'
        , primaryExchange = "" -- Not in this response
        , currency = currency'
        , localSymbol = localSymbol'
        , tradingClass = tradingClass'
        , includeExpired = False -- Not in this response
        , secIdType = "" -- Not in this response
        , secId = "" -- Not in this response
        , issuerId = "" -- Not in this response
        }
      details = ContractDetails
        { cdContract = contract
        , marketName = marketName'
        , minTick = minTick'
        , orderTypes = orderTypes'
        , validExchanges = validExchanges'
        , priceMagnifier = priceMagnifier'
        }
  -- Consume any remaining fields until the end of the input
  void $ many' getField
  return $ ContractData reqId details

-- | Parses a 'ContractDataEnd' message.
parseContractDataEnd :: Parser ServerMessage
parseContractDataEnd = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  return $ ContractDataEnd reqId

-- | Parses an 'Error' message.
-- Format: [MessageID, Version, RequestID, ErrorCode, ErrorMessage]
parseError :: Parser ServerMessage
parseError = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  errCode <- getFieldAsSafeRead 0
  msg <- T.decodeUtf8 <$> getField
  return $ Error $ ErrorInfo reqId errCode msg

-- | Parses a 'TickEFP' message.
-- Format: [MessageID, Version, RequestID, TickType, BasisPoints, FormattedBasisPoints, ImpliedFuture, HoldDays, FutureLastTradeDate, DividendImpact, DividendsToLastTradeDate]
parseTickEFP :: Parser ServerMessage
parseTickEFP = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  _tickType <- getField
  _basisPoints <- getField
  _formattedBasisPoints <- getField
  _impliedFuture <- getField
  _holdDays <- getField
  _futureLastTradeDate <- getField
  _dividendImpact <- getField
  _dividendsToLastTradeDate <- getField
  return $ TickEFP reqId -- For now, we don't need the other fields

-- | Parses a 'MarketDataType' message.
-- Format: [MessageID, Version, RequestID, MarketDataType]
parseMarketDataType :: Parser ServerMessage
parseMarketDataType = do
  _version <- getField
  reqId <- getFieldAsSafeRead 0
  dataType <- getFieldAsSafeRead 0
  return $ MarketDataType reqId dataType

-- | Parses a 'SymbolSamples' message.
-- Format: [MessageID, ReqID, [ContractDescriptions...]]
parseSymbolSamples :: Parser ServerMessage
parseSymbolSamples = do
  reqId <- getFieldAsSafeRead 0
  -- For now, we'll just consume the rest of the message without parsing it.
  -- This is a complex, nested structure we don't need yet.
  void $ many' getField
  return $ SymbolSamples reqId

-- | Parses a 'TickByTick' message.
-- This is a placeholder and simply consumes the message without parsing.
parseTickByTick :: Parser ServerMessage
parseTickByTick = do
  void $ many' getField
  return TickByTick

-- | A helper parser to consume a single null-terminated field from the input.
getField :: Parser ByteString
getField = A.takeTill (== '\0') <* satisfy (== '\0')

-- | Helper to get a field and apply a pure reading function.
-- getFieldAs :: (String -> a) -> Parser a
-- getFieldAs f = f . C8.unpack <$> getField

-- | Helper to get a field and safely apply a reading function, with a default value.
getFieldAsSafeRead :: Read a => a -> Parser a
getFieldAsSafeRead def = fromMaybe def . readMaybe . C8.unpack <$> getField
