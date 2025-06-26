{-# LANGUAGE OverloadedStrings #-}

module Main where

import IB.Protocol.Types
import IB.Client
import qualified IB.Protocol.Constants as C
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Numeric (showHex)

-- | Encode a field for TWS API transmission
encodeField :: Show a => a -> B.Builder
encodeField value = B.byteString (T.encodeUtf8 (T.pack (show value))) <> B.word8 0

-- | Encode a text field
encodeFieldT :: Text -> B.Builder  
encodeFieldT text = B.byteString (T.encodeUtf8 text) <> B.word8 0

-- | Encode empty field (for MAX_VALUE)
encodeEmpty :: B.Builder
encodeEmpty = B.word8 0

-- | Manual PlaceOrder message encoding to demonstrate wire format
encodePlaceOrderManual :: PlaceOrderRequest -> ByteString
encodePlaceOrderManual req = LBS.toStrict $ B.toLazyByteString $
  -- Message type (3 = PLACE_ORDER)
  encodeField (3 :: Int) <>
  -- Order ID  
  encodeField (placeOrderId req) <>
  -- Contract fields
  encodeField (0 :: Int) <> -- conId (0 if not specified)
  encodeFieldT (symbol contract) <>
  encodeField (secType contract) <>
  encodeFieldT (lastTradeDateOrContractMonth contract) <>
  encodeField (strike contract) <>
  encodeFieldT (maybe "" (T.pack . show) (right contract)) <>
  encodeFieldT (multiplier contract) <>
  encodeFieldT (exchange contract) <>
  encodeFieldT (primaryExchange contract) <>
  encodeFieldT (currency contract) <>
  encodeFieldT (localSymbol contract) <>
  encodeFieldT (tradingClass contract) <>
  encodeFieldT (secIdType contract) <>
  encodeFieldT (secId contract) <>
  -- Order fields
  encodeField (orderAction order) <>
  encodeField (orderTotalQuantity order) <>
  encodeField (orderType order) <>
  (case orderLmtPrice order of
    Nothing -> encodeEmpty
    Just price -> encodeField price) <>
  (case orderAuxPrice order of  
    Nothing -> encodeEmpty
    Just price -> encodeField price) <>
  encodeField (orderTif order) <>
  encodeFieldT (orderOcaGroup order) <>
  encodeFieldT (orderAccount order) <>
  encodeFieldT (orderOpenClose order) <>
  encodeField (orderOrigin order) <>
  encodeFieldT (orderRef order) <>
  encodeField (if orderTransmit order then (1 :: Int) else 0) <>
  encodeField (maybe 0 id (orderParentId order)) <>
  encodeField (if orderBlockOrder order then (1 :: Int) else 0) <>
  encodeField (if orderSweepToFill order then (1 :: Int) else 0) <>
  encodeField (maybe 0 id (orderDisplaySize order)) <>
  encodeField (orderTriggerMethod order) <>
  encodeField (if orderOutsideRth order then (1 :: Int) else 0) <>
  encodeField (if orderHidden order then (1 :: Int) else 0) <>
  -- Additional required fields (simplified)
  encodeFieldT "" <> -- sharesAllocation (deprecated)
  encodeEmpty <> -- discretionaryAmt (MAX_VALUE)
  encodeFieldT (orderGoodAfterTime order) <>
  encodeFieldT (orderGoodTillDate order) <>
  encodeFieldT (orderFaGroup order) <>
  encodeFieldT (orderFaMethod order) <>
  encodeFieldT (orderFaPercentage order) <>
  encodeFieldT (orderModelCode order)
  where
    contract = placeOrderContract req
    order = placeOrderOrder req

-- | Show bytes in hex format for debugging
showBytes :: ByteString -> String
showBytes bs = concatMap (\b -> showHex b " ") (BS.unpack bs)

-- | Create example orders and show their wire format
demonstrateWireFormat :: IO ()
demonstrateWireFormat = do
  putStrLn "=== TWS API PlaceOrder Wire Format Demonstration ==="
  putStrLn ""
  
  -- Simple market order
  let marketOrder = createTestOrder 1001 "AAPL" BUY 100.0 MKT Nothing
  putStrLn "1. Market Order for AAPL:"
  putStrLn $ "   " ++ show marketOrder
  putStrLn ""
  
  let marketBytes = encodePlaceOrderManual marketOrder
  putStrLn $ "   Wire format (" ++ show (BS.length marketBytes) ++ " bytes):"
  putStrLn $ "   " ++ showBytes marketBytes
  putStrLn ""
  
  -- Limit order
  let limitOrder = createTestOrder 1002 "MSFT" BUY 50.0 LMT (Just 150.0)
  putStrLn "2. Limit Order for MSFT:"
  putStrLn $ "   " ++ show limitOrder
  putStrLn ""
  
  let limitBytes = encodePlaceOrderManual limitOrder
  putStrLn $ "   Wire format (" ++ show (BS.length limitBytes) ++ " bytes):"
  putStrLn $ "   " ++ showBytes limitBytes
  putStrLn ""
  
  putStrLn "3. Field Breakdown (Market Order):"
  demonstrateFieldBreakdown marketOrder
  putStrLn ""
  
  putStrLn "4. Ready to Send:"
  putStrLn "   • Add 4-byte length header (big-endian)"
  putStrLn "   • Send over TCP socket to TWS port 7497"
  putStrLn "   • TWS will respond with order confirmation or error"
  putStrLn ""

-- | Create a test order with given parameters
createTestOrder :: Int -> Text -> OrderAction -> Double -> OrderType -> Maybe Double -> PlaceOrderRequest
createTestOrder orderId sym action qty oType limitPrice = 
  let contract = (defaultContract sym)
        { exchange = "SMART"
        , currency = "USD"
        }
      order = (defaultOrder action qty oType)
        { orderLmtPrice = limitPrice
        , orderTransmit = True
        , orderTif = DAY
        }
  in placeOrder orderId contract order

-- | Show field-by-field breakdown
demonstrateFieldBreakdown :: PlaceOrderRequest -> IO ()
demonstrateFieldBreakdown req = do
  let contract = placeOrderContract req
      order = placeOrderOrder req
  
  putStrLn "   Field Encoding (each field ends with null byte \\0):"
  putStrLn $ "   Message Type:    3\\0"
  putStrLn $ "   Order ID:        " ++ show (placeOrderId req) ++ "\\0"
  putStrLn $ "   Contract ID:     0\\0"
  putStrLn $ "   Symbol:          " ++ T.unpack (symbol contract) ++ "\\0"
  putStrLn $ "   Security Type:   " ++ show (secType contract) ++ "\\0"
  putStrLn $ "   Expiry:          \\0 (empty)"
  putStrLn $ "   Strike:          " ++ show (strike contract) ++ "\\0"
  putStrLn $ "   Right:           \\0 (empty)"
  putStrLn $ "   Exchange:        " ++ T.unpack (exchange contract) ++ "\\0"
  putStrLn $ "   Currency:        " ++ T.unpack (currency contract) ++ "\\0"
  putStrLn $ "   Action:          " ++ show (orderAction order) ++ "\\0"
  putStrLn $ "   Quantity:        " ++ show (orderTotalQuantity order) ++ "\\0"
  putStrLn $ "   Order Type:      " ++ show (orderType order) ++ "\\0"
  putStrLn $ "   Limit Price:     " ++ (case orderLmtPrice order of 
                                          Nothing -> "\\0 (empty = MAX_VALUE)"
                                          Just p -> show p ++ "\\0") 
  putStrLn $ "   Aux Price:       \\0 (empty = MAX_VALUE)"
  putStrLn $ "   Time in Force:   " ++ show (orderTif order) ++ "\\0"
  putStrLn "   ... (additional fields follow)"

main :: IO ()
main = demonstrateWireFormat