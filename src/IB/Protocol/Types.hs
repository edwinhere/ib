{-# LANGUAGE OverloadedStrings #-}

module IB.Protocol.Types
  ( -- * Core Types
    Contract(..)
  , SecurityType(..)
  , OptionRight(..)
  , ContractDetails(..)
    -- * Message Types
  , MessageId(..)
  , ClientMessage(..)
  , ServerMessage(..)
  , StartApiRequest(..)
  , ErrorInfo(..)
    -- * Helper Types
  , RequestId
  , ServerVersion
  , ConnectionTime
  , UnixTime
  , HistoricalDataRequest(..)
  , Bar(..)
  , MarketDataRequest(..)
  , TickPriceData(..)
  , TickSizeData(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word16)

-- * Helper Type Synonyms

type RequestId = Int
type ServerVersion = Int
type ConnectionTime = ByteString
type UnixTime = Int

-- * Core Data Structures

-- | Represents the type of a security.
data SecurityType
  = STK
  | OPT
  | FUT
  | CASH
  | BOND
  | NEWS
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Represents the right of an option contract.
data OptionRight
  = Call
  | Put
  deriving (Show, Eq, Read, Enum, Bounded)

-- | A detailed description of a financial instrument.
data Contract = Contract
  { conId :: Maybe Int
  , symbol :: Text
  , secType :: SecurityType
  , lastTradeDateOrContractMonth :: Text
  , strike :: Double
  , right :: Maybe OptionRight
  , multiplier :: Text
  , exchange :: Text
  , primaryExchange :: Text
  , currency :: Text
  , localSymbol :: Text
  , tradingClass :: Text
  , includeExpired :: Bool
  , secIdType :: Text
  , secId :: Text
  , issuerId :: Text
  } deriving (Show, Eq)

-- | A fully detailed description of a financial instrument from the server.
data ContractDetails = ContractDetails
  { cdContract :: Contract
  , marketName :: Text
  , minTick :: Double
  , orderTypes :: Text
  , validExchanges :: Text
  , priceMagnifier :: Int
  -- Add more fields from the protocol as needed
  } deriving (Show, Eq)

-- * Message Types

-- | A type-safe representation of a message ID.
newtype MessageId = MessageId Word16
  deriving (Show, Eq, Ord)

-- | Represents a message sent from the client to the server.
data ClientMessage
  = StartApi StartApiRequest
  | ReqCurrentTime
  | ReqIds
  | ReqContractDetails RequestId Contract
  | ReqManagedAccts
  | ReqHistoricalData HistoricalDataRequest
  | ReqMktData MarketDataRequest
  -- Add other client messages here
  deriving (Show, Eq)

-- | Represents a message received from the server.
data ServerMessage
  = CurrentTime UnixTime
  | NextValidId Int
  | ManagedAccounts [Text]
  | ContractData RequestId ContractDetails
  | ContractDataEnd RequestId
  | Error ErrorInfo
  | HistoricalData RequestId Bar
  | HistoricalDataResponse RequestId [Bar]
  | TickPrice TickPriceData
  | TickSize TickSizeData
  | TickEFP RequestId
  | SymbolSamples RequestId
  | MarketDataType RequestId Int
  | TickByTick
  -- Add other server messages here
  deriving (Show, Eq)

-- | Fields required for the initial START_API message.
data StartApiRequest = StartApiRequest
  { apiVersion :: Int
  , clientId :: Int
  , optionalCapabilities :: Maybe Text
  } deriving (Show, Eq)

-- | Information about an error or a status update from the server.
data ErrorInfo = ErrorInfo
  { errorRequestId :: RequestId
  , errorCode :: Int
  , errorMessage :: Text
  } deriving (Show, Eq)

-- | Fields for a historical data request.
data HistoricalDataRequest = HistoricalDataRequest
  { histRequestId :: RequestId
  , histContract :: Contract
  , histEndDate :: Text -- yyyymmdd HH:mm:ss ttt
  , histBarSize :: Text
  , histDuration :: Text
  , histUseRth :: Bool
  , histWhatToShow :: Text
  , histFormatDate :: Int -- 1 for string, 2 for unix timestamp
  , histKeepUpToDate :: Bool
  } deriving (Show, Eq)

-- | Fields for a market data request.
data MarketDataRequest = MarketDataRequest
  { mktDataRequestId :: RequestId
  , mktDataContract :: Contract
  , mktDataGenericTickList :: Text
  , mktDataSnapshot :: Bool
  , mktDataRegulatorySnapshot :: Bool
  } deriving (Show, Eq)

-- | Represents a single historical data bar.
data Bar = Bar
  { barDate :: Text -- Can contain "finished-..." to indicate completion
  , barOpen :: Double
  , barHigh :: Double
  , barLow :: Double
  , barClose :: Double
  , barVolume :: Int
  , barWap :: Double
  , barCount :: Int
  } deriving (Show, Eq)

-- | Represents a tick price update.
data TickPriceData = TickPriceData
  { tickPriceRequestId :: RequestId
  , tickPriceField :: Int -- e.g., 1=bid, 2=ask, 4=last
  , tickPricePrice :: Double
  , tickPriceSize :: Int
  , tickPriceAttribs :: Int -- bitmask
  } deriving (Show, Eq)

-- | Represents a tick size update.
data TickSizeData = TickSizeData
  { tickSizeRequestId :: RequestId
  , tickSizeField :: Int -- e.g., 0=bid size, 3=ask size, 5=last size
  , tickSizeSize :: Int
  } deriving (Show, Eq)
