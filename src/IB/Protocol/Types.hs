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
    -- * New Market Depth Types
  , MarketDepthRequest(..)
  , MarketDepthData(..)
  , MarketDepthOperation(..)
  , MarketDepthSide(..)
    -- * New Real-time Bars Types
  , RealTimeBarsRequest(..)
  , RealTimeBar(..)
    -- * New Tick-by-Tick Types
  , TickByTickRequest(..)
  , TickByTickData(..)
  , TickByTickType(..)
    -- * New Option Calculation Types
  , OptionCalculationRequest(..)
  , OptionCalculationType(..)
  , OptionCalculationData(..)
    -- * New Account and Position Types
  , AccountSummaryRequest(..)
  , AccountSummaryData(..)
  , PositionRequest(..)
  , PositionData(..)
  , PnLRequest(..)
  , PnLData(..)
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

-- * Market Depth Types

-- | Market depth operation types
data MarketDepthOperation
  = Insert
  | Update
  | Delete
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Market depth side
data MarketDepthSide
  = Ask
  | Bid
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Request for market depth (order book) data
data MarketDepthRequest = MarketDepthRequest
  { mktDepthRequestId :: RequestId
  , mktDepthContract :: Contract
  , mktDepthNumRows :: Int
  , mktDepthIsSmartDepth :: Bool
  } deriving (Show, Eq)

-- | Market depth data update
data MarketDepthData = MarketDepthData
  { mktDepthDataRequestId :: RequestId
  , mktDepthDataPosition :: Int
  , mktDepthDataOperation :: MarketDepthOperation
  , mktDepthDataSide :: MarketDepthSide
  , mktDepthDataPrice :: Double
  , mktDepthDataSize :: Int
  } deriving (Show, Eq)

-- * Real-time Bars Types

-- | Request for real-time bars
data RealTimeBarsRequest = RealTimeBarsRequest
  { realTimeBarsRequestId :: RequestId
  , realTimeBarsContract :: Contract
  , realTimeBarsBarSize :: Int -- Currently only 5 is supported
  , realTimeBarsWhatToShow :: Text -- "TRADES", "BID", "ASK", "MIDPOINT"
  , realTimeBarsUseRth :: Bool
  } deriving (Show, Eq)

-- | Real-time bar data
data RealTimeBar = RealTimeBar
  { realTimeBarRequestId :: RequestId
  , realTimeBarTime :: UnixTime
  , realTimeBarOpen :: Double
  , realTimeBarHigh :: Double
  , realTimeBarLow :: Double
  , realTimeBarClose :: Double
  , realTimeBarVolume :: Int
  , realTimeBarWap :: Double
  , realTimeBarCount :: Int
  } deriving (Show, Eq)

-- * Tick-by-Tick Types

-- | Tick-by-tick data types
data TickByTickType
  = TickLast
  | TickAllLast
  | TickBidAsk
  | TickMidPoint
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Request for tick-by-tick data
data TickByTickRequest = TickByTickRequest
  { tickByTickRequestId :: RequestId
  , tickByTickContract :: Contract
  , tickByTickType :: TickByTickType
  , tickByTickNumberOfTicks :: Int -- 0 for unlimited
  , tickByTickIgnoreSize :: Bool
  } deriving (Show, Eq)

-- | Tick-by-tick data
data TickByTickData = TickByTickData
  { tickByTickDataRequestId :: RequestId
  , tickByTickDataType :: TickByTickType
  , tickByTickDataTime :: UnixTime
  , tickByTickDataPrice :: Maybe Double
  , tickByTickDataSize :: Maybe Int
  , tickByTickDataBidPrice :: Maybe Double
  , tickByTickDataAskPrice :: Maybe Double
  , tickByTickDataBidSize :: Maybe Int
  , tickByTickDataAskSize :: Maybe Int
  , tickByTickDataMidPoint :: Maybe Double
  , tickByTickDataExchange :: Maybe Text
  , tickByTickDataSpecialConditions :: Maybe Text
  } deriving (Show, Eq)

-- * Option Calculation Types

-- | Type of option calculation
data OptionCalculationType
  = CalcImpliedVolatility
  | CalcOptionPrice
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Request for option calculations
data OptionCalculationRequest = OptionCalculationRequest
  { optionCalcRequestId :: RequestId
  , optionCalcType :: OptionCalculationType
  , optionCalcContract :: Contract
  , optionCalcOptionPrice :: Double -- For implied vol calculation
  , optionCalcUnderlyingPrice :: Double
  , optionCalcVolatility :: Double -- For option price calculation
  } deriving (Show, Eq)

-- | Option calculation result
data OptionCalculationData = OptionCalculationData
  { optionCalcDataRequestId :: RequestId
  , optionCalcDataImpliedVolatility :: Double
  , optionCalcDataDelta :: Double
  , optionCalcDataOptionPrice :: Double
  , optionCalcDataPvDividend :: Double
  , optionCalcDataGamma :: Double
  , optionCalcDataVega :: Double
  , optionCalcDataTheta :: Double
  , optionCalcDataUnderlyingPrice :: Double
  } deriving (Show, Eq)

-- * Account and Position Types

-- | Request for account summary
data AccountSummaryRequest = AccountSummaryRequest
  { accountSummaryRequestId :: RequestId
  , accountSummaryGroup :: Text -- "All" for all accounts
  , accountSummaryTags :: Text -- Comma-separated list of tags
  } deriving (Show, Eq)

-- | Account summary data
data AccountSummaryData = AccountSummaryData
  { accountSummaryDataRequestId :: RequestId
  , accountSummaryDataAccount :: Text
  , accountSummaryDataTag :: Text
  , accountSummaryDataValue :: Text
  , accountSummaryDataCurrency :: Text
  } deriving (Show, Eq)

-- | Request for positions
data PositionRequest = PositionRequest
  deriving (Show, Eq)

-- | Position data
data PositionData = PositionData
  { positionAccount :: Text
  , positionContract :: Contract
  , positionPosition :: Double
  , positionAverageCost :: Double
  } deriving (Show, Eq)

-- | Request for PnL
data PnLRequest = PnLRequest
  { pnlRequestId :: RequestId
  , pnlAccount :: Text
  , pnlModelCode :: Text
  } deriving (Show, Eq)

-- | PnL data
data PnLData = PnLData
  { pnlDataRequestId :: RequestId
  , pnlDataDailyPnL :: Double
  , pnlDataUnrealizedPnL :: Double
  , pnlDataRealizedPnL :: Double
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
  | ReqMktDepth MarketDepthRequest
  | CancelMktDepth RequestId Bool -- RequestId, isSmartDepth
  | ReqRealTimeBars RealTimeBarsRequest
  | CancelRealTimeBars RequestId
  | ReqTickByTickData TickByTickRequest
  | CancelTickByTickData RequestId
  | ReqCalcImpliedVolatility OptionCalculationRequest
  | ReqCalcOptionPrice OptionCalculationRequest
  | CancelCalculateImpliedVolatility RequestId
  | CancelCalculateOptionPrice RequestId
  | ReqAccountSummary AccountSummaryRequest
  | CancelAccountSummary RequestId
  | ReqPositions PositionRequest
  | CancelPositions
  | ReqPnL PnLRequest
  | CancelPnL RequestId
  -- Add other client messages here
  deriving (Show, Eq)

-- | Represents a message received from the server.
data ServerMessage
  = CurrentTime UnixTime
  | ServerTime Text -- Server time message sent during connection setup
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
  | TickByTick TickByTickData
  | MarketDepth MarketDepthData
  | RealTimeBars RealTimeBar
  | OptionCalculation OptionCalculationData
  | AccountSummary AccountSummaryData
  | AccountSummaryEnd RequestId
  | Position PositionData
  | PositionEnd
  | PnL PnLData
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
