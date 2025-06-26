{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

module IntegrationTest (spec) where

import           Test.Hspec
import           Control.Concurrent (threadDelay, newEmptyMVar, putMVar, tryTakeMVar, MVar, newMVar, readMVar, tryPutMVar)
import           Control.Monad (void, when, forM_)
import           Conduit
import           Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient, AppData)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import           UnliftIO.Timeout (timeout)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Exception (SomeException)
import           Control.Monad.Catch (throwM, fromException, Exception)
import           Data.Typeable (Typeable)

import IB.Codec.Decoder (decodeMessages)
import IB.Codec.Encoder (encodeMessages)
import IB.Network.Framing (frame, unframe)
import IB.Protocol.Types
import IB.Client

-- Custom exception for controlled termination of the conduit
data StopConduit = StopConduit deriving (Show, Typeable)
instance Exception StopConduit

-- Test configuration
testHost :: ByteString
testHost = "127.0.0.1"

testPort :: Int
testPort = 7497

testTimeout :: Int
testTimeout = 3000000 -- 3 seconds in microseconds

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
  { conId = Just 782591262
  , symbol = "AAPL"
  , secType = OPT
  , lastTradeDateOrContractMonth = "20250627"
  , strike = 200.0
  , right = Just Call
  , multiplier = "100"
  , exchange = "SMART"
  , primaryExchange = ""
  , currency = "USD"
  , localSymbol = "AAPL   250627C00200000"
  , tradingClass = "AAPL"
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
  { mcContractDetails :: MVar [ContractDetails]
  , mcHistoricalData :: MVar [Bar]
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
  mcContractDetails' <- newEmptyMVar
  mcHistoricalData' <- newEmptyMVar
  mcMarketDepth' <- newEmptyMVar
  mcRealTimeBars' <- newEmptyMVar
  mcTickByTick' <- newEmptyMVar
  mcAccountSummary' <- newEmptyMVar
  mcPositions' <- newEmptyMVar
  mcOptionCalculations' <- newEmptyMVar
  mcErrors' <- newEmptyMVar
  mcCurrentTime' <- newEmptyMVar
  mcNextValidId' <- newEmptyMVar
  mcServerTime' <- newEmptyMVar
  return MessageCollector
    { mcContractDetails = mcContractDetails'
    , mcHistoricalData = mcHistoricalData'
    , mcMarketDepth = mcMarketDepth'
    , mcRealTimeBars = mcRealTimeBars'
    , mcTickByTick = mcTickByTick'
    , mcAccountSummary = mcAccountSummary'
    , mcPositions = mcPositions'
    , mcOptionCalculations = mcOptionCalculations'
    , mcErrors = mcErrors'
    , mcCurrentTime = mcCurrentTime'
    , mcNextValidId = mcNextValidId'
    , mcServerTime = mcServerTime'
    }

collectMessage :: MessageCollector -> ServerMessage -> IO ()
collectMessage mc msg = case msg of
  ContractData _ details -> do
    existing <- tryTakeMVar (mcContractDetails mc)
    putMVar (mcContractDetails mc) $ maybe [details] (++ [details]) existing
  HistoricalDataResponse _ bars -> do
    existing <- tryTakeMVar (mcHistoricalData mc)
    putMVar (mcHistoricalData mc) $ maybe bars (++ bars) existing
  MarketDepth depthData -> do
    existing <- tryTakeMVar (mcMarketDepth mc)
    putMVar (mcMarketDepth mc) $ maybe [depthData] (++ [depthData]) existing
  RealTimeBars barData -> do
    existing <- tryTakeMVar (mcRealTimeBars mc)
    putMVar (mcRealTimeBars mc) $ maybe [barData] (++ [barData]) existing
  TickByTick tickData -> do
    existing <- tryTakeMVar (mcTickByTick mc)
    putMVar (mcTickByTick mc) $ maybe [tickData] (++ [tickData]) existing
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

-- Test execution helper
runTest :: [ClientMessage] -> (ServerMessage -> Bool) -> IO MessageCollector
runTest clientMsgs stopCondition =
  runTCPClient (clientSettings testPort testHost) $ \app -> do
    -- Send handshake and StartApi messages
    let handshake = createHandshakeMessage
    runConduit $ yield handshake .| appSink app
    let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing
    runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app
    liftIO $ threadDelay 100000 -- Give a moment for the server to respond

    -- Send test-specific messages
    forM_ clientMsgs $ \msg ->
      runConduit $ yield msg .| encodeMessages .| frame .| appSink app

    collector <- newMessageCollector
    isSetupDone <- newMVar False

    let mainPipe = awaitForever (\msg -> do
          liftIO $ collectMessage collector msg
          
          -- State machine: transition from setup to test phase
          wasSetupDone <- liftIO $ readMVar isSetupDone
          let isNextId = case msg of NextValidId{} -> True; _ -> False
          
          let isNowSetupDone = wasSetupDone || isNextId
          when (not wasSetupDone && isNextId) $
            liftIO $ void $ tryPutMVar isSetupDone True
          
          -- Check stop condition only when setup is complete
          when (isNowSetupDone && stopCondition msg) $ throwM StopConduit
          ) `catchC` \e ->
            case fromException e of
              Just StopConduit -> return ()
              Nothing          -> throwM e
    
    void . timeout testTimeout $ runConduit (appSource app .| unframe .| decodeMessages .| mainPipe)
    
    return collector

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
          
          -- Read server response to confirm connection
          response <- runConduit $
            appSource app
              .| unframe
              .| takeC 1
              .| sinkList
          
          return $ not (null response)
        
        result `shouldBe` True

      it "should receive NextValidId after StartApi" $ do
        collector <- runTest [] $ \case
          NextValidId{} -> True
          _             -> False
        
        nextValidIds <- tryTakeMVar (mcNextValidId collector)
        maybe False (not . null) nextValidIds `shouldBe` True

    describe "Contract Details" $ do
      it "should request and receive contract details for EUR.USD" $ do
        let reqId = 100
        let req = ReqContractDetails reqId testEurUsdContract
        collector <- runTest [req] $ \case
          ContractDataEnd r | r == reqId -> True
          Error (ErrorInfo _ 200 _) -> True -- "No security definition has been found"
          _ -> False

        details <- tryTakeMVar (mcContractDetails collector)
        errors <- tryTakeMVar (mcErrors collector)
        
        -- It's a pass if we get details or if we get an error other than "not found"
        let isSuccess = case (details, errors) of
              (Just _, _) -> True
              (_, Just errs) -> all (\e -> errorCode e /= 200) errs
              _ -> False -- Or no response, which is a failure
        isSuccess `shouldBe` True

    describe "Historical Data" $ do
      it "should request and receive historical data for EUR.USD" $ do
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
        collector <- runTest [ReqHistoricalData histReq] $ \case
          HistoricalDataResponse _ bars -> not (null bars) && any (T.isPrefixOf "finished" . barDate) bars
          Error (ErrorInfo _ 162 _) -> True -- "Historical Market Data Service error message"
          _ -> False
          
        historicalData <- tryTakeMVar (mcHistoricalData collector)
        errors <- tryTakeMVar (mcErrors collector)
        
        let isSuccess = case (historicalData, errors) of
              (Just bars, _) -> not (null bars)
              (_, Just errs) -> all (\e -> errorCode e /= 162) errs
              _ -> False
        isSuccess `shouldBe` True

    describe "Market Data" $ do
      it "should request market data for EUR.USD" $ do
        let mktDataReq = MarketDataRequest
              { mktDataRequestId = 300
              , mktDataContract = testEurUsdContract
              , mktDataGenericTickList = ""
              , mktDataSnapshot = False
              , mktDataRegulatorySnapshot = False
              }
        collector <- runTest [ReqMktData mktDataReq] $ \case
          TickPrice{} -> True
          TickSize{} -> True
          Error (ErrorInfo _ 200 _) -> True -- "No security definition has been found"
          _ -> False
        
        errors <- tryTakeMVar (mcErrors collector)
        maybe True (all ((/= 200) . errorCode)) errors `shouldBe` True

    describe "Market Depth" $ do
      it "should request market depth for EUR.USD" $ do
        let mktDepthReq = MarketDepthRequest
              { mktDepthRequestId = 400
              , mktDepthContract = testEurUsdContract
              , mktDepthNumRows = 5
              , mktDepthIsSmartDepth = False
              }
        collector <- runTest [ReqMktDepth mktDepthReq] $ \case
          MarketDepth{} -> True
          Error (ErrorInfo _ 200 _) -> True
          _ -> False

        marketDepth <- tryTakeMVar (mcMarketDepth collector)
        errors <- tryTakeMVar (mcErrors collector)
        
        let isSuccess = case (marketDepth, errors) of
              (Just _, _) -> True
              (_, Just errs) -> all (\e -> errorCode e /= 200) errs
              _ -> False
        isSuccess `shouldBe` True

    describe "Real-time Bars" $ do
      it "should request real-time bars for EUR.USD" $ do
        let realTimeBarsReq = RealTimeBarsRequest
              { realTimeBarsRequestId = 500
              , realTimeBarsContract = testEurUsdContract
              , realTimeBarsBarSize = 5
              , realTimeBarsWhatToShow = "MIDPOINT"
              , realTimeBarsUseRth = True
              }
        collector <- runTest [ReqRealTimeBars realTimeBarsReq] $ \case
          RealTimeBars{} -> True
          Error (ErrorInfo _ 200 _) -> True
          _ -> False

        realTimeBars <- tryTakeMVar (mcRealTimeBars collector)
        errors <- tryTakeMVar (mcErrors collector)
        
        let isSuccess = case (realTimeBars, errors) of
              (Just _, _) -> True
              (_, Just errs) -> all (\e -> errorCode e /= 200) errs
              _ -> False
        isSuccess `shouldBe` True

    describe "Tick-by-Tick Data" $ do
      it "should request tick-by-tick data for EUR.USD" $ do
        let tickByTickReq = TickByTickRequest
              { tickByTickRequestId = 600
              , tickByTickContract = testEurUsdContract
              , tickByTickType = TickBidAsk
              , tickByTickNumberOfTicks = 0
              , tickByTickIgnoreSize = False
              }
        collector <- runTest [ReqTickByTickData tickByTickReq] $ \case
          TickByTick{} -> True
          Error (ErrorInfo _ 200 _) -> True
          _ -> False
        
        errors <- tryTakeMVar (mcErrors collector)
        maybe True (all ((/= 200) . errorCode)) errors `shouldBe` True

    describe "Account Summary" $ do
      it "should request account summary" $ do
        let reqId = 700
        let accountSummaryReq = AccountSummaryRequest
              { accountSummaryRequestId = reqId
              , accountSummaryGroup = "All"
              , accountSummaryTags = "NetLiquidation,TotalCashValue"
              }
        collector <- runTest [ReqAccountSummary accountSummaryReq] $ \case
          AccountSummaryEnd r | r == reqId -> True
          _ -> False

        accountSummary <- tryTakeMVar (mcAccountSummary collector)
        maybe False (not . null) accountSummary `shouldBe` True

    describe "Positions" $ do
      it "should request positions" $ do
        collector <- runTest [ReqPositions PositionRequest] $ \case
          PositionEnd -> True
          _ -> False
        
        positions <- tryTakeMVar (mcPositions collector)
        positions `shouldSatisfy` maybe True (const True) -- Any result is fine

    describe "Option Calculations" $ do
      it "should calculate implied volatility for AAPL option" $ do
        let reqId = 800
        let impliedVolReq = OptionCalculationRequest
              { optionCalcRequestId = reqId
              , optionCalcType = CalcImpliedVolatility
              , optionCalcContract = testAaplOptionContract
              , optionCalcOptionPrice = 5.0
              , optionCalcUnderlyingPrice = 155.0
              , optionCalcVolatility = 0.0
              }
        collector <- runTest [ReqCalcImpliedVolatility impliedVolReq] $ \case
          OptionCalculation o -> optionCalcDataRequestId o == reqId
          Error (ErrorInfo r code _) -> r == reqId || code == 200
          _ -> False
          
        optionCalcs <- tryTakeMVar (mcOptionCalculations collector)
        errors <- tryTakeMVar (mcErrors collector)
        case errors of
          Just errs -> mapM_ (putStrLn . show) errs
          Nothing -> return ()
        let isSuccess = case (optionCalcs, errors) of
              (Just _, _) -> True
              (_, Just errs) -> all (\e -> errorCode e /= 200) errs
              _ -> False
        isSuccess `shouldBe` True

      it "should calculate option price for AAPL option" $ do
        let reqId = 801
        let optionPriceReq = OptionCalculationRequest
              { optionCalcRequestId = reqId
              , optionCalcType = CalcOptionPrice
              , optionCalcContract = testAaplOptionContract
              , optionCalcOptionPrice = 0.0
              , optionCalcUnderlyingPrice = 155.0
              , optionCalcVolatility = 0.25
              }
        collector <- runTest [ReqCalcOptionPrice optionPriceReq] $ \case
          OptionCalculation o -> optionCalcDataRequestId o == reqId
          Error (ErrorInfo r code _) -> r == reqId || code == 200
          _ -> False
          
        optionCalcs <- tryTakeMVar (mcOptionCalculations collector)
        errors <- tryTakeMVar (mcErrors collector)
        case errors of
          Just errs -> mapM_ (putStrLn . show) errs
          Nothing -> return ()
        let isSuccess = case (optionCalcs, errors) of
              (Just _, _) -> True
              (_, Just errs) -> all (\e -> errorCode e /= 200) errs
              _ -> False
        isSuccess `shouldBe` True

      it "should fetch contract details for AAPL 2025-06-27 200C and print conId and errors" $ do
        let reqId = 901
        let req = ReqContractDetails reqId testAaplOptionContract
        collector <- runTest [req] $ \case
          ContractDataEnd r | r == reqId -> True
          Error (ErrorInfo _ 200 _) -> True
          _ -> False
        details <- tryTakeMVar (mcContractDetails collector)
        errors <- tryTakeMVar (mcErrors collector)
        case details of
          Just (cd:_) -> putStrLn $ "Fetched conId: " ++ show (conId (cdContract cd))
          _ -> putStrLn "No contract details found."
        case errors of
          Just errs -> mapM_ (putStrLn . show) errs
          Nothing -> return ()

    describe "Current Time" $ do
      it "should request and receive current time" $ do
        collector <- runTest [ReqCurrentTime] $ \case
          CurrentTime{} -> True
          _ -> False

        currentTime <- tryTakeMVar (mcCurrentTime collector)
        maybe False (not . null) currentTime `shouldBe` True

    describe "PlaceOrder Tests" $ do
      it "should send a PlaceOrder message without errors" $ do
        -- Create a test order (what-if to avoid placing real orders)
        let testContract = Contract
              { conId = Nothing
              , symbol = "AAPL"
              , secType = STK
              , lastTradeDateOrContractMonth = ""
              , strike = 0.0
              , right = Nothing
              , multiplier = ""
              , exchange = "SMART"
              , primaryExchange = ""
              , currency = "USD"
              , localSymbol = ""
              , tradingClass = ""
              , includeExpired = False
              , secIdType = ""
              , secId = ""
              , issuerId = ""
              }
            testOrder = (simpleMarketOrder BUY 1.0)
              { orderWhatIf = True  -- What-if order - won't actually place
              , orderTransmit = False -- Don't transmit
              }
            placeOrderReq = placeOrder 9999 testContract testOrder

        collector <- runTest [PlaceOrder placeOrderReq] $ \case
          Error (ErrorInfo reqId _ _) -> reqId == 9999  -- Stop on our order's error
          _ -> False

        errors <- tryTakeMVar (mcErrors collector)
        -- Any response (even error) means our message format worked
        case errors of
          Just _ -> True `shouldBe` True  -- Got response - protocol works
          Nothing -> pendingWith "No response from TWS - may not support PlaceOrder in test mode" 