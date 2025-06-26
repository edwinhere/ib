{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import IB.Protocol.Types
import IB.Client
import IB.Codec.Encoder (encodeMessages)
import IB.Codec.Decoder (decodeMessages)
import IB.Network.Framing (frame, unframe)
import Conduit
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar, MVar, forkIO)
import Control.Monad (void, when, forever)
import Control.Exception (catch, SomeException)
import UnliftIO.Timeout (timeout)

-- TWS connection settings
twsHost :: ByteString
twsHost = "127.0.0.1"

twsPort :: Int
twsPort = 7497

-- Helper to create handshake message
createHandshakeMessage :: ByteString
createHandshakeMessage = LBS.toStrict $ B.toLazyByteString $
  B.string7 "API\0" <>
  B.word32BE (fromIntegral $ LBS.length versionPayload) <>
  B.lazyByteString versionPayload
  where versionPayload = B.toLazyByteString $ B.string7 "v100..187"

-- State for tracking our order
data OrderState = OrderState
  { osNextValidId :: MVar (Maybe Int)
  , osMessages :: MVar [ServerMessage]
  , osOrderPlaced :: MVar Bool
  } deriving ()

newOrderState :: IO OrderState
newOrderState = do
  nextValidId <- newEmptyMVar
  messages <- newEmptyMVar
  orderPlaced <- newEmptyMVar
  return OrderState
    { osNextValidId = nextValidId
    , osMessages = messages
    , osOrderPlaced = orderPlaced
    }

-- Create AAPL market order
createAAPLMarketOrder :: Int -> PlaceOrderRequest
createAAPLMarketOrder orderId = 
  let contract = Contract
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
      order = Order
        { orderAction = BUY
        , orderTotalQuantity = 1.0  -- 1 share
        , orderType = MKT           -- Market order
        , orderLmtPrice = Nothing
        , orderAuxPrice = Nothing
        , orderTif = DAY
        , orderOcaGroup = ""
        , orderAccount = ""
        , orderOpenClose = ""
        , orderOrigin = 0
        , orderRef = ""
        , orderTransmit = True      -- Actually place the order
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
        , orderWhatIf = False       -- Real order, not what-if
        , orderSolicited = False
        , orderRandomizeSize = False
        , orderRandomizePrice = False
        }
  in placeOrder orderId contract order

-- Message processor
processMessage :: OrderState -> Int -> ServerMessage -> IO ()
processMessage state expectedOrderId msg = do
  case msg of
    NextValidId orderId -> do
      putStrLn $ "✓ Received NextValidId: " ++ show orderId
      putMVar (osNextValidId state) (Just orderId)
      
    Error (ErrorInfo reqId code errorMsg) -> do
      putStrLn $ "✗ Error " ++ show code ++ " for request " ++ show reqId ++ ": " ++ show errorMsg
      
    _ -> do
      putStrLn $ "⚬ Message: " ++ show msg
      
  -- Add to message history
  existing <- readMVar (osMessages state) `catch` \(_ :: SomeException) -> return []
  putMVar (osMessages state) (msg : existing) `catch` \(_ :: SomeException) -> return ()

-- Main order placement function
placeAAPLOrder :: IO ()
placeAAPLOrder = do
  putStrLn "=== AAPL Market Order Test ==="
  putStrLn "Connecting to TWS at 127.0.0.1:7497..."
  putStrLn ""
  putStrLn "⚠️  WARNING: This will place a REAL market order for 1 share of AAPL!"
  putStrLn "   Make sure you're using paper trading or have sufficient funds."
  putStrLn ""
  
  result <- catch (runTCPClient (clientSettings twsPort twsHost) testConnection) handleError
  
  case result of
    Just success -> putStrLn $ "✓ " ++ success
    Nothing -> putStrLn "✗ Failed to place order"
  
  where
    handleError :: SomeException -> IO (Maybe String)
    handleError e = do
      putStrLn $ "✗ Connection error: " ++ show e
      putStrLn "Make sure TWS is running with API enabled on port 7497"
      return Nothing

testConnection app = do
  putStrLn "✓ Connected to TWS"
  
  -- Initialize state
  state <- newOrderState
  
  -- Send handshake
  putStrLn "Sending handshake..."
  let handshake = createHandshakeMessage
  runConduit $ yield handshake .| appSink app
  
  -- Send StartApi
  putStrLn "Sending StartApi..."
  let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing
  runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app
  
  -- Start message listener in background
  void $ forkIO $ forever $ do
    msg <- runConduit $ 
      appSource app .| unframe .| decodeMessages .| takeC 1 .| headC
    case msg of
      Just message -> processMessage state 0 message
      Nothing -> threadDelay 100000
  
  -- Wait for NextValidId
  putStrLn "Waiting for NextValidId..."
  result <- timeout 5000000 $ takeMVar (osNextValidId state) -- 5 second timeout
  
  case result of
    Nothing -> do
      putStrLn "✗ Timeout waiting for NextValidId"
      return Nothing
    Just Nothing -> do
      putStrLn "✗ No NextValidId received"
      return Nothing
    Just (Just orderId) -> do
      putStrLn $ "✓ Got NextValidId: " ++ show orderId
      
      -- Create and send AAPL market order
      let aaplOrder = createAAPLMarketOrder orderId
      putStrLn $ "Placing AAPL market order: " ++ show aaplOrder
      putStrLn ""
      
      -- Send the order
      runConduit $ yield (PlaceOrder aaplOrder) .| encodeMessages .| frame .| appSink app
      
      putStrLn "✓ Order sent to TWS!"
      putStrLn "Check TWS for order confirmation."
      putStrLn ""
      
      -- Wait a bit for responses
      threadDelay 3000000 -- 3 seconds
      
      return $ Just "AAPL market order placed successfully"

main :: IO ()
main = do
  putStrLn "Do you want to place a REAL market order for 1 share of AAPL? (y/N)"
  response <- getLine
  if response `elem` ["y", "Y", "yes", "YES"]
    then placeAAPLOrder
    else putStrLn "Order cancelled by user."