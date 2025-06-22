{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runIbClient
    ) where

import Conduit
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import IB.Codec.Decoder (decodeMessages)
import IB.Codec.Encoder (encodeMessages)
import IB.Network.Framing (frame, unframe)
import IB.Protocol.Types
import UnliftIO.Async (concurrently_)

runIbClient :: IO ()
runIbClient = do
  putStrLn "Connecting to IB Gateway/TWS..."
  runTCPClient (clientSettings 7497 "127.0.0.1") $ \app -> do
    putStrLn "Connection established."

    -- 1. Perform the handshake
    let clientVersion = LBS.toStrict $ B.toLazyByteString $
                          B.string7 "API\0" <>
                          B.word32BE (fromIntegral $ LBS.length versionPayload) <>
                          B.lazyByteString versionPayload
          where versionPayload = B.toLazyByteString $ B.string7 "v100..187"
    runConduit $ yield clientVersion .| appSink app

    -- Read the server handshake response
    serverHandshake <- runConduit $
      appSource app
        .| unframe
        .| takeC 1
        .| sinkList
    putStrLn $ "Server handshake received: " ++ show serverHandshake

    -- It's crucial to send StartApi right after the handshake.
    -- The server will then send a stream of initial messages, including NextValidId.
    let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing -- Using clientId 0
    runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app

    -- 2. Setup concurrent processing loops
    putStrLn "Starting message processing loops..."
    concurrently_
      -- Ingress: Read from socket, unframe, decode, and print
      (runConduit $
        appSource app
          .| unframe
          .| decodeMessages
          .| (printSink' .| awaitForever (liftIO . print))
          )
      -- Egress: Start sending other requests after a brief pause
      -- to allow the initial message stream to be processed.
      (runConduit $ do
          liftIO $ threadDelay 1000000 -- 1 second delay

          -- Request contract details for EUR.USD
          let eurUsd = Contract
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
              reqIdContract = 100
          yield (ReqContractDetails reqIdContract eurUsd)

          -- Request historical data for EUR.USD
          let histReq = HistoricalDataRequest
                { histRequestId = 200
                , histContract = eurUsd
                , histEndDate = ""
                , histBarSize = "1 day"
                , histDuration = "1 M"
                , histUseRth = True
                , histWhatToShow = "MIDPOINT"
                , histFormatDate = 2
                , histKeepUpToDate = False
                }
          yield (ReqHistoricalData histReq)

          -- Request market data for EUR.USD
          let mktDataReq = MarketDataRequest
                { mktDataRequestId = 300
                , mktDataContract = eurUsd
                , mktDataGenericTickList = ""
                , mktDataSnapshot = False
                , mktDataRegulatorySnapshot = False
                }
          let reqMkt = ReqMktData mktDataReq
          liftIO $ putStrLn "--> ReqMktData EUR.USD"
          yield reqMkt

          -- Request market depth for EUR.USD
          let mktDepthReq = MarketDepthRequest
                { mktDepthRequestId = 400
                , mktDepthContract = eurUsd
                , mktDepthNumRows = 5
                , mktDepthIsSmartDepth = False
                }
          liftIO $ putStrLn "--> ReqMktDepth EUR.USD"
          yield (ReqMktDepth mktDepthReq)

          -- Request real-time bars for EUR.USD
          let realTimeBarsReq = RealTimeBarsRequest
                { realTimeBarsRequestId = 500
                , realTimeBarsContract = eurUsd
                , realTimeBarsBarSize = 5 -- Only 5 seconds is supported
                , realTimeBarsWhatToShow = "MIDPOINT"
                , realTimeBarsUseRth = True
                }
          liftIO $ putStrLn "--> ReqRealTimeBars EUR.USD"
          yield (ReqRealTimeBars realTimeBarsReq)

          -- Request tick-by-tick data for EUR.USD
          let tickByTickReq = TickByTickRequest
                { tickByTickRequestId = 600
                , tickByTickContract = eurUsd
                , tickByTickType = TickBidAsk
                , tickByTickNumberOfTicks = 0 -- Unlimited
                , tickByTickIgnoreSize = False
                }
          liftIO $ putStrLn "--> ReqTickByTickData EUR.USD"
          yield (ReqTickByTickData tickByTickReq)

          -- Request account summary
          let accountSummaryReq = AccountSummaryRequest
                { accountSummaryRequestId = 700
                , accountSummaryGroup = "All"
                , accountSummaryTags = "NetLiquidation,TotalCashValue"
                }
          liftIO $ putStrLn "--> ReqAccountSummary"
          yield (ReqAccountSummary accountSummaryReq)

          -- Request positions
          liftIO $ putStrLn "--> ReqPositions"
          yield (ReqPositions PositionRequest)

          -- Example option calculation (for AAPL options)
          let aaplOption = Contract
                { conId = Nothing
                , symbol = "AAPL"
                , secType = OPT
                , lastTradeDateOrContractMonth = "20241220" -- December 20, 2024
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

          -- Calculate implied volatility for a given option price
          let impliedVolReq = OptionCalculationRequest
                { optionCalcRequestId = 800
                , optionCalcType = CalcImpliedVolatility
                , optionCalcContract = aaplOption
                , optionCalcOptionPrice = 5.0 -- Option price
                , optionCalcUnderlyingPrice = 155.0 -- Current stock price
                , optionCalcVolatility = 0.0 -- Not used for implied vol calculation
                }
          liftIO $ putStrLn "--> ReqCalcImpliedVolatility AAPL Call"
          yield (ReqCalcImpliedVolatility impliedVolReq)

          -- Calculate option price for a given volatility
          let optionPriceReq = OptionCalculationRequest
                { optionCalcRequestId = 801
                , optionCalcType = CalcOptionPrice
                , optionCalcContract = aaplOption
                , optionCalcOptionPrice = 0.0 -- Not used for price calculation
                , optionCalcUnderlyingPrice = 155.0 -- Current stock price
                , optionCalcVolatility = 0.25 -- 25% volatility
                }
          liftIO $ putStrLn "--> ReqCalcOptionPrice AAPL Call"
          yield (ReqCalcOptionPrice optionPriceReq)

          -- Request the current time every 5 seconds
          forever (do
            yield ReqCurrentTime
            liftIO $ threadDelay 5000000 -- 5 seconds
            )
        .| encodeMessages
        .| frame
        .| appSink app)

    -- Keep the main thread alive to allow the concurrent loops to run
    _ <- forever $ threadDelay maxBound

    putStrLn "IB Client finished."


-- A simple sink to print received messages to the console.
printSink' :: MonadIO m => ConduitT ServerMessage ServerMessage m ()
printSink' = awaitForever $ \msg -> do
  case msg of
    HistoricalDataResponse _ bars -> do
      liftIO . putStrLn $ "Received " ++ show (length bars) ++ " historical bars."
      -- liftIO . print $ head bars -- Optionally print the first bar
    MarketDepth _ -> do
      liftIO . putStrLn $ "Received market depth update."
    RealTimeBars _ -> do
      liftIO . putStrLn $ "Received real-time bar."
    TickByTick -> do
      liftIO . putStrLn $ "Received tick-by-tick data."
    AccountSummary _ -> do
      liftIO . putStrLn $ "Received account summary."
    AccountSummaryEnd _ -> do
      liftIO . putStrLn $ "Account summary complete."
    Position _ -> do
      liftIO . putStrLn $ "Received position data."
    PositionEnd -> do
      liftIO . putStrLn $ "Positions complete."
    PnL _ -> do
      liftIO . putStrLn $ "Received PnL data."
    OptionCalculation _ -> do
      liftIO . putStrLn $ "Received option calculation."
    _ -> yield msg
