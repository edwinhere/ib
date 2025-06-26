{-# LANGUAGE OverloadedStrings #-}

module Main where

import IB.Protocol.Types
import IB.Client
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word (Word32)
import Numeric (showHex)
import Data.Char (isPrint)

-- Manual encoding functions (simplified version of our encoder)
encodeField :: Show a => a -> B.Builder
encodeField value = B.byteString (T.encodeUtf8 (T.pack (show value))) <> B.word8 0

encodeFieldT :: T.Text -> B.Builder  
encodeFieldT text = B.byteString (T.encodeUtf8 text) <> B.word8 0

encodeEmpty :: B.Builder
encodeEmpty = B.word8 0

-- Create minimal PlaceOrder message for AAPL
createMinimalPlaceOrder :: Int -> ByteString
createMinimalPlaceOrder orderId = addLengthHeader $ LBS.toStrict $ B.toLazyByteString $
  encodeField (3 :: Int) <> -- PLACE_ORDER message type
  -- No version field for modern servers (>= 145)
  encodeField orderId <>
  -- Contract fields
  encodeField (0 :: Int) <> -- conId (0 = not specified)
  encodeFieldT "AAPL" <>
  encodeFieldT "STK" <>
  encodeFieldT "" <> -- lastTradeDateOrContractMonth (empty)
  encodeField (0.0 :: Double) <> -- strike
  encodeFieldT "" <> -- right (empty)
  encodeFieldT "" <> -- multiplier (empty)
  encodeFieldT "SMART" <>
  encodeFieldT "" <> -- primaryExchange (empty)
  encodeFieldT "USD" <>
  encodeFieldT "" <> -- localSymbol (empty)
  encodeFieldT "" <> -- tradingClass (empty)
  encodeFieldT "" <> -- secIdType (empty)
  encodeFieldT "" <> -- secId (empty)
  -- Order fields
  encodeFieldT "BUY" <>
  encodeField (1.0 :: Double) <> -- totalQuantity
  encodeFieldT "MKT" <>
  encodeEmpty <> -- lmtPrice (MAX_VALUE -> empty)
  encodeEmpty <> -- auxPrice (MAX_VALUE -> empty)
  encodeFieldT "DAY" <>
  encodeFieldT "" <> -- ocaGroup (empty)
  encodeFieldT "" <> -- account (empty)
  encodeFieldT "" <> -- openClose (empty)
  encodeField (0 :: Int) <> -- origin (0 = Customer)
  encodeFieldT "" <> -- orderRef (empty)
  encodeField (1 :: Int) <> -- transmit (true)
  encodeField (0 :: Int) <> -- parentId
  encodeField (0 :: Int) <> -- blockOrder (false)
  encodeField (0 :: Int) <> -- sweepToFill (false)
  encodeField (0 :: Int) <> -- displaySize
  encodeField (0 :: Int) <> -- triggerMethod
  encodeField (0 :: Int) <> -- outsideRth (false)
  encodeField (0 :: Int) <> -- hidden (false)
  -- Additional essential fields
  encodeFieldT "" <> -- sharesAllocation (deprecated, empty)
  encodeEmpty <> -- discretionaryAmt (MAX_VALUE)
  encodeFieldT "" <> -- goodAfterTime (empty)
  encodeFieldT "" <> -- goodTillDate (empty)
  encodeFieldT "" <> -- faGroup (empty)
  encodeFieldT "" <> -- faMethod (empty)
  encodeFieldT "" <> -- faPercentage (empty)
  encodeFieldT "" <> -- modelCode (empty)
  encodeField (0 :: Int) <> -- shortSaleSlot
  encodeFieldT "" <> -- designatedLocation (empty)
  encodeField (-1 :: Int) <> -- exemptCode
  encodeField (0 :: Int) <> -- ocaType
  encodeFieldT "" <> -- rule80A (empty)
  encodeFieldT "" <> -- settlingFirm (empty)
  encodeField (0 :: Int) <> -- allOrNone (false)
  encodeEmpty <> -- minQty (MAX_VALUE)
  encodeEmpty <> -- percentOffset (MAX_VALUE)
  encodeField (0 :: Int) <> -- auctionStrategy
  encodeEmpty <> -- startingPrice (MAX_VALUE)
  encodeEmpty <> -- stockRefPrice (MAX_VALUE)
  encodeEmpty <> -- delta (MAX_VALUE)
  encodeEmpty <> -- stockRangeLower (MAX_VALUE)
  encodeEmpty <> -- stockRangeUpper (MAX_VALUE)
  encodeField (0 :: Int) <> -- overridePercentageConstraints (false)
  encodeEmpty <> -- volatility (MAX_VALUE)
  encodeEmpty <> -- volatilityType (MAX_VALUE)
  encodeFieldT "" <> -- deltaNeutralOrderType (empty)
  encodeEmpty <> -- deltaNeutralAuxPrice (MAX_VALUE)
  encodeField (0 :: Int) <> -- continuousUpdate
  encodeEmpty <> -- referencePriceType (MAX_VALUE)
  encodeEmpty <> -- trailStopPrice (MAX_VALUE)
  encodeEmpty <> -- trailingPercent (MAX_VALUE)
  encodeEmpty <> -- scaleInitLevelSize (MAX_VALUE)
  encodeEmpty <> -- scaleSubsLevelSize (MAX_VALUE)
  encodeEmpty <> -- scalePriceIncrement (MAX_VALUE)
  encodeFieldT "" <> -- hedgeType (empty)
  encodeFieldT "" <> -- hedgeParam (empty)
  encodeField (0 :: Int) <> -- optOutSmartRouting (false)
  encodeFieldT "" <> -- clearingAccount (empty)
  encodeFieldT "" <> -- clearingIntent (empty)
  encodeField (0 :: Int) <> -- notHeld (false)
  encodeFieldT "" <> -- algoStrategy (empty)
  encodeFieldT "" <> -- algoId (empty)
  encodeField (0 :: Int) <> -- whatIf (false)
  encodeField (0 :: Int) <> -- solicited (false)
  encodeField (0 :: Int) <> -- randomizeSize (false)
  encodeField (0 :: Int)    -- randomizePrice (false)

-- Add 4-byte length header
addLengthHeader :: ByteString -> ByteString
addLengthHeader payload = 
  let len = fromIntegral (BS.length payload) :: Word32
      lenBytes = LBS.toStrict $ B.toLazyByteString (B.word32BE len)
  in lenBytes <> payload

-- Pretty print hex dump
hexDump :: ByteString -> String
hexDump bs = unlines $ zipWith formatLine [0,16..] (chunks 16 (map fromIntegral (BS.unpack bs)))
  where
    chunks n [] = []
    chunks n xs = take n xs : chunks n (drop n xs)
    
    formatLine offset bytes =
      let hexPart = unwords $ map (padHex . flip showHex "") bytes
          charPart = map (\b -> if isPrint (toEnum b) then toEnum b else '.') bytes
          paddedHex = take 48 (hexPart ++ repeat ' ')
      in printf "%08x  %s  |%s|" offset paddedHex charPart
    
    padHex s = if length s == 1 then '0':s else s
    printf fmt offset hex chars = 
      replaceFormat fmt [show offset, hex, chars]
    
    replaceFormat [] _ = []
    replaceFormat ('%':'0':'8':'x':rest) (arg:args) = 
      reverse (take 8 (reverse arg ++ repeat '0')) ++ replaceFormat rest args
    replaceFormat ('%':'s':rest) (arg:args) = arg ++ replaceFormat rest args
    replaceFormat (c:rest) args = c : replaceFormat rest args

-- Field-by-field breakdown
fieldBreakdown :: ByteString -> IO ()
fieldBreakdown bs = do
  putStrLn "Field-by-field breakdown:"
  putStrLn "========================"
  
  let bytes = map fromIntegral (BS.unpack bs)
      fields = parseFields (drop 4 bytes) -- Skip length header
      
  mapM_ printField $ zip [1..] fields
  
  where
    parseFields [] = []
    parseFields bytes = 
      let (field, rest) = break (== 0) bytes
      in field : parseFields (drop 1 rest)
    
    printField (n, field) = 
      let fieldStr = map (toEnum . fromEnum) field
          hex = unwords $ map (flip showHex "") field
      in putStrLn $ printf "Field %2d: %-20s (hex: %s)" n fieldStr hex
    
    printf fmt n str hex = 
      replaceFormat fmt [show n, str, hex]
    
    replaceFormat [] _ = []
    replaceFormat ('%':'2':'d':rest) (arg:args) = 
      reverse (take 2 (reverse arg ++ repeat ' ')) ++ replaceFormat rest args
    replaceFormat ('%':'-':'2':'0':'s':rest) (arg:args) = 
      take 20 (arg ++ repeat ' ') ++ replaceFormat rest args
    replaceFormat ('%':'s':rest) (arg:args) = arg ++ replaceFormat rest args
    replaceFormat (c:rest) args = c : replaceFormat rest args

main :: IO ()
main = do
  putStrLn "=== TWS PlaceOrder Wire Format Analysis ==="
  putStrLn ""
  
  let orderId = 1001
  let orderBytes = createMinimalPlaceOrder orderId
  
  putStrLn $ "Order ID: " ++ show orderId
  putStrLn $ "Message length: " ++ show (BS.length orderBytes) ++ " bytes"
  putStrLn ""
  
  putStrLn "Raw hex dump:"
  putStrLn "============="
  putStrLn $ hexDump orderBytes
  putStrLn ""
  
  putStrLn "Field breakdown:"
  putStrLn "================"
  putStrLn $ "Length header (4 bytes): " ++ 
    (unwords $ map (flip showHex "") $ map fromIntegral $ BS.unpack $ BS.take 4 orderBytes)
  putStrLn ""
  
  fieldBreakdown orderBytes
  putStrLn ""
  
  putStrLn "This message represents:"
  putStrLn "- BUY 1 share of AAPL"
  putStrLn "- Market order (MKT)"
  putStrLn "- SMART exchange"
  putStrLn "- USD currency"
  putStrLn "- Day order (DAY)"
  putStrLn "- Transmit = true (will be sent to exchange)"
  putStrLn ""
  putStrLn "Ready to send to TWS on port 7497!"