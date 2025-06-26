{-# LANGUAGE OverloadedStrings #-}

module Main where

import IB.Protocol.Types
import IB.Client
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word32)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, SomeException)

-- Connection settings
twsHost = "127.0.0.1"
twsPort = 7497

-- Manual message encoding (simplified version of our encoder)
encodeField :: Show a => a -> B.Builder
encodeField value = B.byteString (T.encodeUtf8 (T.pack (show value))) <> B.word8 0

encodeFieldT :: T.Text -> B.Builder  
encodeFieldT text = B.byteString (T.encodeUtf8 text) <> B.word8 0

encodeEmpty :: B.Builder
encodeEmpty = B.word8 0

-- Create handshake
createHandshake :: ByteString
createHandshake = LBS.toStrict $ B.toLazyByteString $
  B.string7 "API\0" <>
  B.word32BE (fromIntegral $ LBS.length versionPayload) <>
  B.lazyByteString versionPayload
  where versionPayload = B.toLazyByteString $ B.string7 "v100..187"

-- Create StartApi message
createStartApi :: ByteString
createStartApi = addLengthHeader $ LBS.toStrict $ B.toLazyByteString $
  encodeField (71 :: Int) <> -- START_API message type
  encodeField (2 :: Int) <>  -- Version
  encodeField (0 :: Int) <>  -- Client ID
  encodeEmpty                -- Optional capabilities (empty)

-- Create PlaceOrder message
createPlaceOrderMessage :: PlaceOrderRequest -> ByteString
createPlaceOrderMessage req = addLengthHeader $ LBS.toStrict $ B.toLazyByteString $
  let c = placeOrderContract req
      o = placeOrderOrder req
  in
  encodeField (3 :: Int) <> -- PLACE_ORDER message type
  encodeField (placeOrderId req) <>
  -- Contract fields
  encodeField (0 :: Int) <> -- conId (0 if not specified)
  encodeFieldT (symbol c) <>
  encodeField (secType c) <>
  encodeFieldT (lastTradeDateOrContractMonth c) <>
  encodeField (strike c) <>
  encodeFieldT (maybe "" (T.pack . show) (right c)) <>
  encodeFieldT (multiplier c) <>
  encodeFieldT (exchange c) <>
  encodeFieldT (primaryExchange c) <>
  encodeFieldT (currency c) <>
  encodeFieldT (localSymbol c) <>
  encodeFieldT (tradingClass c) <>
  encodeFieldT (secIdType c) <>
  encodeFieldT (secId c) <>
  -- Order fields
  encodeField (orderAction o) <>
  encodeField (orderTotalQuantity o) <>
  encodeField (orderType o) <>
  (case orderLmtPrice o of
    Nothing -> encodeEmpty
    Just price -> encodeField price) <>
  (case orderAuxPrice o of  
    Nothing -> encodeEmpty
    Just price -> encodeField price) <>
  encodeField (orderTif o) <>
  encodeFieldT (orderOcaGroup o) <>
  encodeFieldT (orderAccount o) <>
  encodeFieldT (orderOpenClose o) <>
  encodeField (orderOrigin o) <>
  encodeFieldT (orderRef o) <>
  encodeField (if orderTransmit o then (1 :: Int) else 0) <>
  encodeField (maybe 0 id (orderParentId o)) <>
  encodeField (if orderBlockOrder o then (1 :: Int) else 0) <>
  encodeField (if orderSweepToFill o then (1 :: Int) else 0) <>
  encodeField (maybe 0 id (orderDisplaySize o)) <>
  encodeField (orderTriggerMethod o) <>
  encodeField (if orderOutsideRth o then (1 :: Int) else 0) <>
  encodeField (if orderHidden o then (1 :: Int) else 0) <>
  -- Additional required fields (simplified - just the essential ones)
  encodeFieldT "" <> -- sharesAllocation (deprecated)
  encodeEmpty <> -- discretionaryAmt (MAX_VALUE)
  encodeFieldT (orderGoodAfterTime o) <>
  encodeFieldT (orderGoodTillDate o) <>
  encodeFieldT (orderFaGroup o) <>
  encodeFieldT (orderFaMethod o) <>
  encodeFieldT (orderFaPercentage o) <>
  encodeFieldT (orderModelCode o) <>
  encodeField (orderShortSaleSlot o) <>
  encodeFieldT (orderDesignatedLocation o) <>
  encodeField (orderExemptCode o) <>
  encodeField (orderOcaType o) <>
  encodeFieldT (orderRule80A o) <>
  encodeFieldT (orderSettlingFirm o) <>
  encodeField (if orderAllOrNone o then (1 :: Int) else 0) <>
  encodeEmpty <> -- minQty (MAX_VALUE)
  encodeEmpty <> -- percentOffset (MAX_VALUE)
  encodeField (0 :: Int) <> -- auctionStrategy
  encodeEmpty <> -- startingPrice (MAX_VALUE)
  encodeEmpty <> -- stockRefPrice (MAX_VALUE)
  encodeEmpty <> -- delta (MAX_VALUE)
  encodeEmpty <> -- stockRangeLower (MAX_VALUE)
  encodeEmpty <> -- stockRangeUpper (MAX_VALUE)
  encodeField (if orderOverridePercentageConstraints o then (1 :: Int) else 0) <>
  -- Volatility order fields (all defaults/MAX_VALUE)
  encodeEmpty <> -- volatility (MAX_VALUE)
  encodeEmpty <> -- volatilityType (MAX_VALUE)
  encodeFieldT "" <> -- deltaNeutralOrderType (empty)
  encodeEmpty <> -- deltaNeutralAuxPrice (MAX_VALUE)
  encodeField (0 :: Int) <> -- continuousUpdate
  encodeEmpty <> -- referencePriceType (MAX_VALUE)
  -- Additional fields
  encodeEmpty <> -- trailStopPrice (MAX_VALUE)
  encodeEmpty <> -- trailingPercent (MAX_VALUE)
  encodeEmpty <> -- scaleInitLevelSize (MAX_VALUE)
  encodeEmpty <> -- scaleSubsLevelSize (MAX_VALUE)
  encodeEmpty <> -- scalePriceIncrement (MAX_VALUE)
  encodeFieldT "" <> -- hedgeType (empty)
  encodeFieldT "" <> -- hedgeParam (empty)
  encodeField (0 :: Int) <> -- optOutSmartRouting (false)
  encodeFieldT (orderClearingAccount o) <>
  encodeFieldT (orderClearingIntent o) <>
  encodeField (if orderNotHeld o then (1 :: Int) else 0) <>
  encodeFieldT "" <> -- algoStrategy (empty)
  encodeFieldT "" <> -- algoId (empty)
  encodeField (if orderWhatIf o then (1 :: Int) else 0) <>
  encodeField (if orderSolicited o then (1 :: Int) else 0) <>
  encodeField (if orderRandomizeSize o then (1 :: Int) else 0) <>
  encodeField (if orderRandomizePrice o then (1 :: Int) else 0)

-- Add 4-byte length header
addLengthHeader :: ByteString -> ByteString
addLengthHeader payload = 
  let len = fromIntegral (BS.length payload) :: Word32
      lenBytes = LBS.toStrict $ B.toLazyByteString (B.word32BE len)
  in lenBytes <> payload

-- Create AAPL market order
createAAPLOrder :: Int -> PlaceOrderRequest
createAAPLOrder orderId = 
  let contract = (defaultContract "AAPL")
        { exchange = "SMART"
        , currency = "USD"
        }
      order = (simpleMarketOrder BUY 1.0)
        { orderTransmit = True
        , orderTif = DAY
        }
  in placeOrder orderId contract order

-- Connect and place order
placeOrder' :: IO ()
placeOrder' = do
  putStrLn "=== Simple AAPL Order Test ==="
  putStrLn "⚠️  WARNING: This places a REAL market order for 1 share of AAPL!"
  putStrLn ""
  putStrLn "Continue? (y/N)"
  response <- getLine
  
  if response `elem` ["y", "Y", "yes", "YES"]
    then do
      putStrLn "Connecting to TWS..."
      connectAndOrder
    else putStrLn "Cancelled."

connectAndOrder :: IO ()
connectAndOrder = bracket
  (do
    sock <- socket AF_INET Stream 0
    addr <- resolve twsHost (show twsPort)
    connect sock addr
    return sock)
  close
  (\sock -> do
    putStrLn "✓ Connected to TWS"
    
    -- Send handshake
    putStrLn "Sending handshake..."
    sendAll sock createHandshake
    
    -- Read handshake response (server version info)
    _ <- recv sock 4096
    putStrLn "✓ Handshake complete"
    
    -- Send StartApi
    putStrLn "Sending StartApi..."
    sendAll sock createStartApi
    
    -- Wait for NextValidId
    putStrLn "Waiting for NextValidId..."
    threadDelay 1000000 -- 1 second
    
    response <- recv sock 4096
    putStrLn $ "Received: " ++ show (BS.take 50 response)
    
    -- Parse the order ID (simplified - assumes NextValidId is first message)
    let orderId = 1001 -- Use fixed ID for this test
    
    -- Create and send order
    putStrLn $ "Placing order with ID: " ++ show orderId
    let aaplOrder = createAAPLOrder orderId
    let orderMessage = createPlaceOrderMessage aaplOrder
    
    sendAll sock orderMessage
    putStrLn "✓ Order sent!"
    
    -- Wait for response
    threadDelay 2000000 -- 2 seconds
    response2 <- recv sock 4096
    putStrLn $ "Order response: " ++ show (BS.take 100 response2)
    
    putStrLn "✓ Check TWS for order status"
  )
  `catch` \(e :: SomeException) -> do
    putStrLn $ "✗ Error: " ++ show e
    putStrLn "Make sure TWS is running with API enabled on port 7497"

resolve :: String -> String -> IO SockAddr
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return $ addrAddress addr

main :: IO ()
main = placeOrder'