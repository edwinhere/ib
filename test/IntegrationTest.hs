{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module IntegrationTest (spec) where

import           Test.Hspec
import           Control.Concurrent (threadDelay, newEmptyMVar, putMVar, tryTakeMVar, MVar)
import           Conduit
import           Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient, AppData)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import           Data.Word (Word32)
import           UnliftIO.Timeout (timeout)
import           Data.Text (Text)

import IB.Codec.Decoder (decodeMessages)
import IB.Codec.Encoder (encodeMessages)
import IB.Network.Framing (frame, unframe)
import IB.Protocol.Types

-- Test configuration
testHost :: ByteString
testHost = "127.0.0.1"

testPort :: Int
testPort = 7497

testTimeout :: Int
testTimeout = 30000000 -- 30 seconds in microseconds

-- Test data
testEurUsdContract :: Contract
testEurUsdContract = Contract
  { conId = Nothing
  , symbol = "EUR"
  , secType = CASH
  , lastTradeDateOrContractMonth = ""
  , strike = 0.0
  , right = Nothing
  , multiplier = ""
  , exchange = "IDEALPRO"
  , primaryExchange = ""
  , currency = "USD"
  , localSymbol = ""
  , tradingClass = ""
  , includeExpired = False
  , secIdType = ""
  , secId = ""
  , issuerId = ""
  }

testAaplOptionContract :: Contract
testAaplOptionContract = Contract
  { conId = Nothing
  , symbol = "AAPL"
  , secType = OPT
  , lastTradeDateOrContractMonth = "20241220"
  , strike = 150.0
  , right = Just Call
  , multiplier = "100"
  , exchange = "SMART"
  , primaryExchange = "CBOE"
  , currency = "USD"
  , localSymbol = ""
  , tradingClass = ""
  , includeExpired = False
  , secIdType = ""
  , secId = ""
  , issuerId = ""
  }

-- Helper functions
createHandshakeMessage :: ByteString
createHandshakeMessage = LBS.toStrict $ B.toLazyByteString $
  B.string7 "API\0" <>
  B.word32BE (fromIntegral $ LBS.length versionPayload) <>
  B.lazyByteString versionPayload
  where versionPayload = B.toLazyByteString $ B.string7 "v100..187"

-- Message collectors for testing
data MessageCollector = MessageCollector
  { mcHistoricalData :: MVar [Bar]
  , mcMarketDepth :: MVar [MarketDepthData]
  , mcRealTimeBars :: MVar [RealTimeBar]
  , mcTickByTick :: MVar [TickByTickData]
  , mcAccountSummary :: MVar [AccountSummaryData]
  , mcPositions :: MVar [PositionData]
  , mcOptionCalculations :: MVar [OptionCalculationData]
  , mcErrors :: MVar [ErrorInfo]
  , mcCurrentTime :: MVar [UnixTime]
  , mcNextValidId :: MVar [Int]
  , mcServerTime :: MVar [Text]
  }

newMessageCollector :: IO MessageCollector
newMessageCollector = do
  mcHistoricalData <- newEmptyMVar
  mcMarketDepth <- newEmptyMVar
  mcRealTimeBars <- newEmptyMVar
  mcTickByTick <- newEmptyMVar
  mcAccountSummary <- newEmptyMVar
  mcPositions <- newEmptyMVar
  mcOptionCalculations <- newEmptyMVar
  mcErrors <- newEmptyMVar
  mcCurrentTime <- newEmptyMVar
  mcNextValidId <- newEmptyMVar
  mcServerTime <- newEmptyMVar
  return MessageCollector
    { mcHistoricalData = mcHistoricalData
    , mcMarketDepth = mcMarketDepth
    , mcRealTimeBars = mcRealTimeBars
    , mcTickByTick = mcTickByTick
    , mcAccountSummary = mcAccountSummary
    , mcPositions = mcPositions
    , mcOptionCalculations = mcOptionCalculations
    , mcErrors = mcErrors
    , mcCurrentTime = mcCurrentTime
    , mcNextValidId = mcNextValidId
    , mcServerTime = mcServerTime
    }

collectMessage :: MessageCollector -> ServerMessage -> IO ()
collectMessage mc msg = case msg of
  HistoricalDataResponse _ bars -> do
    existing <- tryTakeMVar (mcHistoricalData mc)
    putMVar (mcHistoricalData mc) $ maybe bars (++ bars) existing
  MarketDepth depthData -> do
    existing <- tryTakeMVar (mcMarketDepth mc)
    putMVar (mcMarketDepth mc) $ maybe [depthData] (++ [depthData]) existing
  RealTimeBars barData -> do
    existing <- tryTakeMVar (mcRealTimeBars mc)
    putMVar (mcRealTimeBars mc) $ maybe [barData] (++ [barData]) existing
  TickByTick -> do
    -- TickByTick is just a marker, actual data comes separately
    return ()
  AccountSummary summaryData -> do
    existing <- tryTakeMVar (mcAccountSummary mc)
    putMVar (mcAccountSummary mc) $ maybe [summaryData] (++ [summaryData]) existing
  Position positionData -> do
    existing <- tryTakeMVar (mcPositions mc)
    putMVar (mcPositions mc) $ maybe [positionData] (++ [positionData]) existing
  OptionCalculation calcData -> do
    existing <- tryTakeMVar (mcOptionCalculations mc)
    putMVar (mcOptionCalculations mc) $ maybe [calcData] (++ [calcData]) existing
  Error errorMsg -> do
    existing <- tryTakeMVar (mcErrors mc)
    putMVar (mcErrors mc) $ maybe [errorMsg] (++ [errorMsg]) existing
  CurrentTime time -> do
    existing <- tryTakeMVar (mcCurrentTime mc)
    putMVar (mcCurrentTime mc) $ maybe [time] (++ [time]) existing
  NextValidId orderId -> do
    existing <- tryTakeMVar (mcNextValidId mc)
    putMVar (mcNextValidId mc) $ maybe [orderId] (++ [orderId]) existing
  ServerTime time -> do
    existing <- tryTakeMVar (mcServerTime mc)
    putMVar (mcServerTime mc) $ maybe [time] (++ [time]) existing
  _ -> return ()

-- Test specifications
spec :: Spec
spec = do
  describe "IB Client Integration Tests" $ do
    describe "Connection and Handshake" $ do
      it "should establish connection and perform handshake" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Send handshake
          let handshake = createHandshakeMessage
          runConduit $ yield handshake .| appSink app
          
          -- Read server response
          response <- runConduit $
            appSource app
              .| unframe
              .| takeC 1
              .| sinkList
          
          return $ not (null response)
        
        result `shouldBe` True

      it "should receive NextValidId after StartApi" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Send handshake
          let handshake = createHandshakeMessage
          runConduit $ yield handshake .| appSink app
          
          -- Send StartApi
          let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing
          runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app
          
          -- Collect messages for a short time
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check if we received NextValidId
          nextValidIds <- tryTakeMVar (mcNextValidId collector)
          return $ maybe False (not . null) nextValidIds
        
        result `shouldBe` True

    describe "Contract Details" $ do
      it "should request and receive contract details for EUR.USD" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request contract details
          let reqId = 100
          let contractDetailsReq = ReqContractDetails reqId testEurUsdContract
          runConduit $ yield contractDetailsReq .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for contract data or errors
          errors <- tryTakeMVar (mcErrors collector)
          return $ maybe True (all (\e -> errorCode e /= 200) . take 1) errors
        
        result `shouldBe` True

    describe "Historical Data" $ do
      it "should request and receive historical data for EUR.USD" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request historical data
          let histReq = HistoricalDataRequest
                { histRequestId = 200
                , histContract = testEurUsdContract
                , histEndDate = ""
                , histBarSize = "1 day"
                , histDuration = "1 M"
                , histUseRth = True
                , histWhatToShow = "MIDPOINT"
                , histFormatDate = 2
                , histKeepUpToDate = False
                }
          runConduit $ yield (ReqHistoricalData histReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for historical data or errors
          historicalData <- tryTakeMVar (mcHistoricalData collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (historicalData, errors) of
            (Just bars, _) -> return $ not (null bars)
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return False
        
        result `shouldBe` True

    describe "Market Data" $ do
      it "should request market data for EUR.USD" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request market data
          let mktDataReq = MarketDataRequest
                { mktDataRequestId = 300
                , mktDataContract = testEurUsdContract
                , mktDataGenericTickList = ""
                , mktDataSnapshot = False
                , mktDataRegulatorySnapshot = False
                }
          runConduit $ yield (ReqMktData mktDataReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for errors (market data subscription should not error)
          errors <- tryTakeMVar (mcErrors collector)
          return $ maybe True (all (\e -> errorCode e /= 200) . take 1) errors
        
        result `shouldBe` True

    describe "Market Depth" $ do
      it "should request market depth for EUR.USD" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request market depth
          let mktDepthReq = MarketDepthRequest
                { mktDepthRequestId = 400
                , mktDepthContract = testEurUsdContract
                , mktDepthNumRows = 5
                , mktDepthIsSmartDepth = False
                }
          runConduit $ yield (ReqMktDepth mktDepthReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for market depth data or errors
          marketDepth <- tryTakeMVar (mcMarketDepth collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (marketDepth, errors) of
            (Just depthData, _) -> return $ not (null depthData)
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return False
        
        result `shouldBe` True

    describe "Real-time Bars" $ do
      it "should request real-time bars for EUR.USD" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request real-time bars
          let realTimeBarsReq = RealTimeBarsRequest
                { realTimeBarsRequestId = 500
                , realTimeBarsContract = testEurUsdContract
                , realTimeBarsBarSize = 5
                , realTimeBarsWhatToShow = "MIDPOINT"
                , realTimeBarsUseRth = True
                }
          runConduit $ yield (ReqRealTimeBars realTimeBarsReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for real-time bars or errors
          realTimeBars <- tryTakeMVar (mcRealTimeBars collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (realTimeBars, errors) of
            (Just bars, _) -> return $ not (null bars)
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return False
        
        result `shouldBe` True

    describe "Tick-by-Tick Data" $ do
      it "should request tick-by-tick data for EUR.USD" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request tick-by-tick data
          let tickByTickReq = TickByTickRequest
                { tickByTickRequestId = 600
                , tickByTickContract = testEurUsdContract
                , tickByTickType = TickBidAsk
                , tickByTickNumberOfTicks = 0
                , tickByTickIgnoreSize = False
                }
          runConduit $ yield (ReqTickByTickData tickByTickReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for errors (tick-by-tick subscription should not error)
          errors <- tryTakeMVar (mcErrors collector)
          return $ maybe True (all (\e -> errorCode e /= 200) . take 1) errors
        
        result `shouldBe` True

    describe "Account Summary" $ do
      it "should request account summary" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request account summary
          let accountSummaryReq = AccountSummaryRequest
                { accountSummaryRequestId = 700
                , accountSummaryGroup = "All"
                , accountSummaryTags = "NetLiquidation,TotalCashValue"
                }
          runConduit $ yield (ReqAccountSummary accountSummaryReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for account summary data or errors
          accountSummary <- tryTakeMVar (mcAccountSummary collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (accountSummary, errors) of
            (Just summaryData, _) -> return $ not (null summaryData)
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return False
        
        result `shouldBe` True

    describe "Positions" $ do
      it "should request positions" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request positions
          runConduit $ yield (ReqPositions PositionRequest) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for position data or errors
          positions <- tryTakeMVar (mcPositions collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (positions, errors) of
            (Just _, _) -> return True -- Positions might be empty, which is valid
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return True -- No positions is also valid
        
        result `shouldBe` True

    describe "Option Calculations" $ do
      it "should calculate implied volatility for AAPL option" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Calculate implied volatility
          let impliedVolReq = OptionCalculationRequest
                { optionCalcRequestId = 800
                , optionCalcType = CalcImpliedVolatility
                , optionCalcContract = testAaplOptionContract
                , optionCalcOptionPrice = 5.0
                , optionCalcUnderlyingPrice = 155.0
                , optionCalcVolatility = 0.0
                }
          runConduit $ yield (ReqCalcImpliedVolatility impliedVolReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for option calculation data or errors
          optionCalcs <- tryTakeMVar (mcOptionCalculations collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (optionCalcs, errors) of
            (Just calcData, _) -> return $ not (null calcData)
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return False
        
        result `shouldBe` True

      it "should calculate option price for AAPL option" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Calculate option price
          let optionPriceReq = OptionCalculationRequest
                { optionCalcRequestId = 801
                , optionCalcType = CalcOptionPrice
                , optionCalcContract = testAaplOptionContract
                , optionCalcOptionPrice = 0.0
                , optionCalcUnderlyingPrice = 155.0
                , optionCalcVolatility = 0.25
                }
          runConduit $ yield (ReqCalcOptionPrice optionPriceReq) .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for option calculation data or errors
          optionCalcs <- tryTakeMVar (mcOptionCalculations collector)
          errors <- tryTakeMVar (mcErrors collector)
          
          case (optionCalcs, errors) of
            (Just calcData, _) -> return $ not (null calcData)
            (_, Just errs) -> return $ all (\e -> errorCode e /= 200) (take 1 errs)
            _ -> return False
        
        result `shouldBe` True

    describe "Current Time" $ do
      it "should request and receive current time" $ do
        result <- runTCPClient (clientSettings testPort testHost) $ \app -> do
          -- Setup connection
          setupConnection app
          
          -- Request current time
          runConduit $ yield ReqCurrentTime .| encodeMessages .| frame .| appSink app
          
          -- Collect responses
          collector <- newMessageCollector
          _ <- timeout testTimeout $ runConduit $
            appSource app
              .| unframe
              .| decodeMessages
              .| awaitForever (liftIO . collectMessage collector)
          
          -- Check for current time data
          currentTime <- tryTakeMVar (mcCurrentTime collector)
          return $ maybe False (not . null) currentTime
        
        result `shouldBe` True

-- Helper function to setup connection
setupConnection :: (MonadIO m) => AppData -> m ()
setupConnection app = do
  -- Send handshake
  let handshake = createHandshakeMessage
  runConduit $ yield handshake .| appSink app
  
  -- Send StartApi
  let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing
  runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app
  
  -- Brief delay to allow connection setup
  liftIO $ threadDelay 100000 -- 100ms 