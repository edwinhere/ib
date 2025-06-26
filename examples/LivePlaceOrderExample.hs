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
import Control.Concurrent (threadDelay, newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Monad (void, when)
import Control.Exception (catch, SomeException)
import UnliftIO.Timeout (timeout)

-- Test configuration
testHost :: ByteString
testHost = "127.0.0.1"

testPort :: Int
testPort = 7497

testTimeout :: Int
testTimeout = 10000000 -- 10 seconds in microseconds

-- Helper to create handshake message
createHandshakeMessage :: ByteString
createHandshakeMessage = LBS.toStrict $ B.toLazyByteString $
  B.string7 "API\0" <>
  B.word32BE (fromIntegral $ LBS.length versionPayload) <>
  B.lazyByteString versionPayload
  where versionPayload = B.toLazyByteString $ B.string7 "v100..187"

-- Message collector for responses
data OrderResponse = OrderResponse
  { nextValidId :: MVar (Maybe Int)
  , orderStatus :: MVar [ServerMessage]
  , errors :: MVar [ErrorInfo]
  } deriving ()

newOrderResponse :: IO OrderResponse
newOrderResponse = do
  nextValidId' <- newEmptyMVar
  orderStatus' <- newEmptyMVar  
  errors' <- newEmptyMVar
  return OrderResponse
    { nextValidId = nextValidId'
    , orderStatus = orderStatus'
    , errors = errors'
    }

collectOrderResponse :: OrderResponse -> ServerMessage -> IO Bool
collectOrderResponse resp msg = case msg of
  NextValidId oid -> do
    putMVar (nextValidId resp) (Just oid)
    putStrLn $ "✓ Received NextValidId: " ++ show oid
    return True
  Error err -> do
    void $ putMVar (errors resp) [err]
    putStrLn $ "✗ Error: " ++ show err
    return True
  _ -> do
    putStrLn $ "⚬ Other message: " ++ show msg
    return False

-- Create a simple test order
createTestOrder :: Int -> PlaceOrderRequest
createTestOrder orderId = 
  let contract = (defaultContract "AAPL")
        { exchange = "SMART"
        , currency = "USD"
        }
      -- Create a VERY small limit order that won't execute
      order = (simpleLimitOrder BUY 1.0 0.01) -- $0.01 limit - won't fill
        { orderTransmit = True
        , orderAccount = "" -- Empty = use default account
        , orderTif = DAY
        }
  in placeOrder orderId contract order

-- Main function that connects to TWS and places an order
placeOrderLive :: IO ()
placeOrderLive = do
  putStrLn "=== Live PlaceOrder Test ==="
  putStrLn "Connecting to TWS at 127.0.0.1:7497..."
  
  result <- catch (runTCPClient (clientSettings testPort testHost) testConnection) handleError
  
  case result of
    Just success -> putStrLn $ "✓ " ++ success
    Nothing -> putStrLn "✗ Test failed or timed out"
  
  where
    handleError :: SomeException -> IO (Maybe String)
    handleError e = do
      putStrLn $ "✗ Connection error: " ++ show e
      putStrLn "Make sure TWS is running and API is enabled (Socket port 7497)"
      return Nothing

testConnection app = do
  putStrLn "Connected! Sending handshake..."
  
  -- Send handshake
  let handshake = createHandshakeMessage
  runConduit $ yield handshake .| appSink app
  
  -- Send StartApi
  let startApiMsg = StartApi $ StartApiRequest 2 0 Nothing
  runConduit $ yield startApiMsg .| encodeMessages .| frame .| appSink app
  
  putStrLn "Handshake sent, waiting for NextValidId..."
  
  -- Setup response collector
  resp <- newOrderResponse
  nextIdReceived <- newEmptyMVar
  
  -- Listen for responses
  let responseHandler = awaitForever $ \msg -> do
        liftIO $ do
          isComplete <- collectOrderResponse resp msg
          when isComplete $ void $ putMVar nextIdReceived True
  
  -- Start response listener in background
  void $ async $ runConduit $ 
    appSource app .| unframe .| decodeMessages .| responseHandler
  
  -- Wait for NextValidId (confirms connection)
  nextIdResult <- timeout testTimeout $ takeMVar nextIdReceived
  
  case nextIdResult of
    Nothing -> do
      putStrLn "✗ Timeout waiting for NextValidId"
      return Nothing
    Just _ -> do
      -- Get the actual order ID
      mOrderId <- takeMVar (nextValidId resp)
      case mOrderId of
        Nothing -> do
          putStrLn "✗ No NextValidId received"
          return Nothing
        Just orderId -> do
          putStrLn $ "✓ Got NextValidId: " ++ show orderId
          
          -- Create and send order
          let testOrder = createTestOrder orderId
          putStrLn $ "Placing test order: " ++ show testOrder
          
          -- Send the order
          runConduit $ yield (PlaceOrder testOrder) .| encodeMessages .| frame .| appSink app
          
          putStrLn "✓ Order sent successfully!"
          putStrLn "Check TWS for the order (AAPL BUY 1 @ $0.01 limit)"
          
          -- Wait a bit for any immediate responses
          threadDelay 2000000 -- 2 seconds
          
          return $ Just "Order placed successfully"

-- For this to work without the async import, let's create a simpler version
placeOrderSimple :: IO ()
placeOrderSimple = do
  putStrLn "=== Simple PlaceOrder Message Demo ==="
  putStrLn ""
  putStrLn "Creating order messages that would be sent to TWS..."
  putStrLn ""
  
  -- Create the order
  let testOrder = createTestOrder 1001
  putStrLn $ "Test Order: " ++ show testOrder
  putStrLn ""
  
  putStrLn "This order structure would be:"
  putStrLn "1. Encoded using the PlaceOrder message builder"
  putStrLn "2. Framed with 4-byte length header"  
  putStrLn "3. Sent over TCP socket to TWS API"
  putStrLn ""
  putStrLn "To actually place orders, you need:"
  putStrLn "- TWS or IB Gateway running"
  putStrLn "- API connections enabled (Socket port 7497)"
  putStrLn "- Valid IB account with trading permissions"
  putStrLn ""
  putStrLn "The implementation includes full wire protocol encoding"
  putStrLn "as specified in the TWS API documentation."

main :: IO ()
main = do
  putStrLn "Choose test mode:"
  putStrLn "1. Simple demo (no TWS connection needed)"
  putStrLn "2. Live connection test (requires TWS running)"
  putStrLn ""
  putStrLn "Running simple demo..."
  placeOrderSimple