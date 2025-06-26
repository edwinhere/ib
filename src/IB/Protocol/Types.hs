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
    -- * Order Types
  , Order(..)
  , OrderAction(..)
  , OrderType(..)
  , TimeInForce(..)
  , PlaceOrderRequest(..)
  , defaultOrder
    -- * Server Version Constants
  , minServerVerNotHeld
  , minServerVerOrderContainer
  , minServerVerPlaceOrderConId
  , minServerVerTradingClass
  , minServerVerSecIdType
  , minServerVerFractionalPositions
  , minServerVerScaleOrders
  , minServerVerHedgeOrders
  , minServerVerAlgoOrders
  , minServerVerOrderSolicited
  , minServerVerModelsSupport
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word16)

-- * Helper Type Synonyms

type RequestId = Int
type ServerVersion = Int
type ConnectionTime = ByteString
type UnixTime = Int

-- * Server Version Constants (from official TWS API)

-- | Server version constants for conditional field inclusion
minServerVerNotHeld :: ServerVersion
minServerVerNotHeld = 44

minServerVerOrderContainer :: ServerVersion  
minServerVerOrderContainer = 145

minServerVerPlaceOrderConId :: ServerVersion
minServerVerPlaceOrderConId = 46

minServerVerTradingClass :: ServerVersion
minServerVerTradingClass = 68

minServerVerSecIdType :: ServerVersion
minServerVerSecIdType = 45

minServerVerFractionalPositions :: ServerVersion
minServerVerFractionalPositions = 160

minServerVerScaleOrders :: ServerVersion
minServerVerScaleOrders = 20

minServerVerHedgeOrders :: ServerVersion
minServerVerHedgeOrders = 53

minServerVerAlgoOrders :: ServerVersion
minServerVerAlgoOrders = 54

minServerVerOrderSolicited :: ServerVersion
minServerVerOrderSolicited = 104

minServerVerModelsSupport :: ServerVersion
minServerVerModelsSupport = 121

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

-- * Order Types

-- | Order action (BUY or SELL)
data OrderAction
  = BUY
  | SELL
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Order type
data OrderType
  = MKT    -- Market order
  | LMT    -- Limit order
  | STP    -- Stop order
  | STP_LMT -- Stop-limit order
  | REL    -- Relative order
  | TRAIL  -- Trailing stop order
  | BOX_TOP -- Box top order
  | FIX_PEGGED -- Fixed pegged order
  | LIT    -- Lit order
  | LMT_PLUS_MKT -- Limit plus market order
  | LOC    -- Limit on close order
  | MIT    -- Market if touched order
  | MKT_PRT -- Market with protection order
  | MOC    -- Market on close order
  | MTL    -- Market to limit order
  | PASSV_REL -- Passive relative order
  | PEG_BENCH -- Pegged to benchmark order
  | PEG_MID -- Pegged to midpoint order
  | PEG_MKT -- Pegged to market order
  | PEG_PRIM -- Pegged to primary order
  | PEG_STK -- Pegged to stock order  
  | REL_PLUS_LMT -- Relative plus limit order
  | REL_PLUS_MKT -- Relative plus market order
  | SNAP_MID -- Snap midpoint order
  | SNAP_MKT -- Snap market order
  | SNAP_PRIM -- Snap primary order
  | STP_PRT -- Stop with protection order
  | TRAIL_LIMIT -- Trailing stop limit order
  | TRAIL_LIT -- Trailing stop lit order
  | TRAIL_LMT_PLUS_MKT -- Trailing stop limit plus market order
  | TRAIL_MIT -- Trailing stop market if touched order
  | TRAIL_REL_PLUS_MKT -- Trailing stop relative plus market order
  | VOL -- Volatility order
  | VWAP -- Volume weighted average price order
  deriving (Show, Eq, Read)

-- | Time in force
data TimeInForce
  = DAY  -- Day order
  | GTC  -- Good till canceled
  | IOC  -- Immediate or cancel
  | GTD  -- Good till date
  | OPG  -- At the opening
  | FOK  -- Fill or kill
  | DTC  -- Day till canceled
  deriving (Show, Eq, Read, Enum, Bounded)

-- | Order data structure
data Order = Order
  { orderAction :: OrderAction
  , orderTotalQuantity :: Double -- Changed to Double for fractional shares
  , orderType :: OrderType
  , orderLmtPrice :: Maybe Double -- Nothing means Double.MAX_VALUE
  , orderAuxPrice :: Maybe Double -- Nothing means Double.MAX_VALUE
  , orderTif :: TimeInForce
  , orderOcaGroup :: Text
  , orderAccount :: Text  
  , orderOpenClose :: Text
  , orderOrigin :: Int -- 0=Customer, 1=Firm
  , orderRef :: Text
  , orderTransmit :: Bool
  , orderParentId :: Maybe Int
  , orderBlockOrder :: Bool
  , orderSweepToFill :: Bool
  , orderDisplaySize :: Maybe Int
  , orderTriggerMethod :: Int
  , orderOutsideRth :: Bool
  , orderHidden :: Bool
  , orderGoodAfterTime :: Text
  , orderGoodTillDate :: Text
  , orderOverridePercentageConstraints :: Bool
  , orderRule80A :: Text
  , orderAllOrNone :: Bool
  , orderMinQty :: Maybe Int
  , orderPercentOffset :: Maybe Double
  , orderTrailStopPrice :: Maybe Double
  , orderTrailingPercent :: Maybe Double
  , orderFaGroup :: Text
  , orderFaProfile :: Text
  , orderFaMethod :: Text
  , orderFaPercentage :: Text
  , orderModelCode :: Text
  , orderShortSaleSlot :: Int
  , orderDesignatedLocation :: Text
  , orderExemptCode :: Int
  , orderOcaType :: Int
  , orderSettlingFirm :: Text
  , orderClearingAccount :: Text
  , orderClearingIntent :: Text
  , orderNotHeld :: Bool
  , orderWhatIf :: Bool
  , orderSolicited :: Bool
  , orderRandomizeSize :: Bool
  , orderRandomizePrice :: Bool
  } deriving (Show, Eq)

-- | Default order with sensible defaults
defaultOrder :: OrderAction -> Double -> OrderType -> Order
defaultOrder action quantity oType = Order
  { orderAction = action
  , orderTotalQuantity = quantity
  , orderType = oType
  , orderLmtPrice = Nothing
  , orderAuxPrice = Nothing
  , orderTif = DAY
  , orderOcaGroup = ""
  , orderAccount = ""
  , orderOpenClose = ""
  , orderOrigin = 0
  , orderRef = ""
  , orderTransmit = True
  , orderParentId = Nothing
  , orderBlockOrder = False
  , orderSweepToFill = False
  , orderDisplaySize = Nothing
  , orderTriggerMethod = 0
  , orderOutsideRth = False
  , orderHidden = False
  , orderGoodAfterTime = ""
  , orderGoodTillDate = ""
  , orderOverridePercentageConstraints = False
  , orderRule80A = ""
  , orderAllOrNone = False
  , orderMinQty = Nothing
  , orderPercentOffset = Nothing
  , orderTrailStopPrice = Nothing
  , orderTrailingPercent = Nothing
  , orderFaGroup = ""
  , orderFaProfile = ""
  , orderFaMethod = ""
  , orderFaPercentage = ""
  , orderModelCode = ""
  , orderShortSaleSlot = 0
  , orderDesignatedLocation = ""
  , orderExemptCode = -1
  , orderOcaType = 0
  , orderSettlingFirm = ""
  , orderClearingAccount = ""
  , orderClearingIntent = ""
  , orderNotHeld = False
  , orderWhatIf = False
  , orderSolicited = False
  , orderRandomizeSize = False
  , orderRandomizePrice = False
  }

-- | Request to place an order
data PlaceOrderRequest = PlaceOrderRequest
  { placeOrderId :: Int
  , placeOrderContract :: Contract
  , placeOrderOrder :: Order
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
  | PlaceOrder PlaceOrderRequest
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
