{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module LivePlaceOrderTest (testPlaceOrderLive) where

import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, tryTakeMVar, MVar)
import Control.Monad (void, when)
import Conduit
import Data.Conduit.Network (appSink, appSource, clientSettings, runTCPClient)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import UnliftIO.Timeout (timeout)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (catch, SomeException)
import Control.Monad.Catch (throwM, fromException, Exception)
import Data.Typeable (Typeable)

import IB.Codec.Decoder (decodeMessages)
import IB.Codec.Encoder (encodeMessages)
import IB.Network.Framing (frame, unframe)
import IB.Protocol.Types
import IB.Client

-- Test configuration
testHost :: ByteString
testHost = "127.0.0.1"

testPort :: Int
testPort = 7497

testTimeout :: Int
testTimeout = 10000000 -- 10 seconds

-- Helper to create handshake message
createHandshakeMessage :: ByteString
createHandshakeMessage = LBS.toStrict $ B.toLazyByteString $
  B.string7 "API\0" <>
  B.word32BE (fromIntegral $ LBS.length versionPayload) <>
  B.lazyByteString versionPayload
  where versionPayload = B.toLazyByteString $ B.string7 "v100..187"

-- Order response collector
data OrderResult = OrderResult
  { orNextValidId :: MVar (Maybe Int)
  , orMessages :: MVar [ServerMessage]
  , orOrderPlaced :: MVar Bool
  } deriving ()

newOrderResult :: IO OrderResult
newOrderResult = do
  nextValidId' <- newEmptyMVar
  messages' <- newEmptyMVar
  orderPlaced' <- newEmptyMVar
  return OrderResult
    { orNextValidId = nextValidId'
    , orMessages = messages'
    , orOrderPlaced = orderPlaced'
    }

-- Process server messages
processOrderMessage :: OrderResult -> ServerMessage -> IO Bool
processOrderMessage result msg = case msg of
  NextValidId orderId -> do
    putStrLn $ "✓ Received NextValidId: " ++ show orderId
    putMVar (orNextValidId result) (Just orderId)
    return False -- Continue listening
    
  Error err@(ErrorInfo reqId code errorMsg) -> do
    putStrLn $ "✗ Error " ++ show code ++ " for request " ++ show reqId ++ ": " ++ T.unpack errorMsg
    existing <- tryTakeMVar (orMessages result)
    putMVar (orMessages result) $ maybe [Error err] (++ [Error err]) existing
    return True -- Stop on error
    
  _ -> do
    putStrLn $ "⚬ Message: " ++ show msg
    existing <- tryTakeMVar (orMessages result)
    putMVar (orMessages result) $ maybe [msg] (++ [msg]) existing
    return False -- Continue listening

-- Create AAPL market order
createRealAAPLOrder :: Int -> PlaceOrderRequest
createRealAAPLOrder orderId = 
  let contract = (defaultContract "AAPL")
        { exchange = "SMART"
        , currency = "USD"
        }
      order = (simpleMarketOrder BUY 1.0) -- 1 share
        { orderTransmit = True -- REAL ORDER - will be executed!
        , orderTif = DAY
        , orderAccount = "" -- Use default account
        }
  in placeOrder orderId contract order

-- Test with connection error handling
testPlaceOrderLive :: IO ()
testPlaceOrderLive = do
  putStrLn "=== Live AAPL Order Test ==="
  putStrLn ""
  putStrLn "⚠️  WARNING: This will place a REAL market order for 1 share of AAPL!"
  putStrLn "   Make sure you're using paper trading or have sufficient funds."
  putStrLn ""
  putStrLn "Continue? (y/N)"
  
  response <- getLine
  if response `elem` ["y", "Y", "yes", "YES"]
    then do
      putStrLn "Connecting to TWS..."
      result <- catch (runTCPClient (clientSettings testPort testHost) testConnection) handleError
      case result of
        Just success -> putStrLn $ "✓ " ++ success
        Nothing -> putStrLn "✗ Test failed or connection error"
    else putStrLn "Test cancelled by user."
  
  where
    handleError :: SomeException -> IO (Maybe String)
    handleError e = do
      putStrLn $ "✗ Connection error: " ++ show e
      putStrLn "Make sure TWS is running with API enabled on port 7497"
      return Nothing

testConnection app = do
  putStrLn "✓ Connected to TWS"
  
  -- Send handshake
  let handshake = createHandshakeMessage
  runConduit $ yield handshake .| appSink app
  putStrLn "✓ Handshake sent"
  
  -- Send StartApi
  let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing
  runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app
  putStrLn "✓ StartApi sent"
  
  -- Setup result collector
  result <- newOrderResult
  
  -- Message handler
  let messageHandler = awaitForever $ \msg -> do
        shouldStop <- liftIO $ processOrderMessage result msg
        when shouldStop $ error "Stopping message handler"
  
  -- Start message processing (simplified - just process a few messages)
  void $ runConduit $ 
    appSource app 
      .| unframe 
      .| decodeMessages 
      .| takeC 10  -- Take first 10 messages to avoid infinite loop
      .| messageHandler `catchC` \(_ :: SomeException) -> return ()
  
  -- Wait for NextValidId
  putStrLn "Waiting for NextValidId..."
  nextIdResult <- timeout testTimeout $ takeMVar (orNextValidId result)
  
  case nextIdResult of
    Nothing -> do
      putStrLn "✗ Timeout waiting for NextValidId"
      return Nothing
    Just Nothing -> do
      putStrLn "✗ No NextValidId received"
      return Nothing
    Just (Just orderId) -> do
      putStrLn $ "✓ Got NextValidId: " ++ show orderId
      
      -- Create and place the order
      let aaplOrder = createRealAAPLOrder orderId
      putStrLn $ "Placing AAPL order: " ++ show (placeOrderId aaplOrder)
      putStrLn $ "  Symbol: " ++ T.unpack (symbol (placeOrderContract aaplOrder))
      putStrLn $ "  Action: " ++ show (orderAction (placeOrderOrder aaplOrder))
      putStrLn $ "  Quantity: " ++ show (orderTotalQuantity (placeOrderOrder aaplOrder))
      putStrLn $ "  Type: " ++ show (orderType (placeOrderOrder aaplOrder))
      putStrLn ""
      
      -- Send the order
      runConduit $ yield (PlaceOrder aaplOrder) .| encodeMessages .| frame .| appSink app
      
      putStrLn "✓ Order sent to TWS!"
      putStrLn ""
      putStrLn "🔍 Check TWS for order confirmation."
      putStrLn "   The order should appear in your TWS order book."
      putStrLn ""
      
      -- Wait for any immediate responses
      threadDelay 3000000 -- 3 seconds
      
      return $ Just "AAPL market order placed successfully"

-- Function to run the test
main :: IO ()
main = testPlaceOrderLive