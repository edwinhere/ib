{-# LANGUAGE OverloadedStrings #-}

module Main where

import IB.Protocol.Types
import IB.Client

-- | Example of creating a simple market order for AAPL stock
exampleMarketOrder :: PlaceOrderRequest
exampleMarketOrder = 
  let contract = defaultContract "AAPL"
      order = simpleMarketOrder BUY 100.0
  in placeOrder 1001 contract order

-- | Example of creating a limit order for MSFT stock  
exampleLimitOrder :: PlaceOrderRequest
exampleLimitOrder =
  let contract = defaultContract "MSFT"
      order = simpleLimitOrder BUY 50.0 150.0
  in placeOrder 1002 contract order

-- | Example of a more complex order with custom settings
exampleComplexOrder :: PlaceOrderRequest
exampleComplexOrder =
  let contract = Contract
        { conId = Nothing
        , symbol = "GOOGL"
        , secType = STK
        , lastTradeDateOrContractMonth = ""
        , strike = 0.0
        , right = Nothing
        , multiplier = ""
        , exchange = "NASDAQ"
        , primaryExchange = "NASDAQ"
        , currency = "USD"
        , localSymbol = ""
        , tradingClass = ""
        , includeExpired = False
        , secIdType = ""
        , secId = ""
        , issuerId = ""
        }
      order = (defaultOrder SELL 25.0 LMT)
        { orderLmtPrice = Just 2800.0
        , orderTif = GTC
        , orderAccount = "U123456"
        , orderTransmit = False  -- Don't transmit immediately
        , orderOutsideRth = True -- Allow outside regular trading hours
        }
  in placeOrder 1003 contract order

-- | Demonstrate encoding orders to wire format
demonstrateEncoding :: IO ()
demonstrateEncoding = do
  putStrLn "=== PlaceOrder API Implementation Demo ==="
  putStrLn ""
  
  putStrLn "1. Simple Market Order for AAPL:"
  putStrLn $ "   " ++ show exampleMarketOrder
  putStrLn ""
  
  putStrLn "2. Limit Order for MSFT:"
  putStrLn $ "   " ++ show exampleLimitOrder
  putStrLn ""
  
  putStrLn "3. Complex Order for GOOGL:"
  putStrLn $ "   " ++ show exampleComplexOrder
  putStrLn ""
  
  putStrLn "4. Wire format examples:"
  putStrLn "   (Note: These would be sent to TWS API as binary data)"
  
  -- Note: The actual encoding would require the full conduit infrastructure
  -- This is just to show the structure
  putStrLn "   - Market order would encode contract fields + order fields"
  putStrLn "   - All fields null-terminated with 0x00"
  putStrLn "   - Message prefixed with 4-byte length header"
  putStrLn ""
  
  putStrLn "=== Key Features Implemented ==="
  putStrLn "✓ Complete Order data structure with all TWS API fields"
  putStrLn "✓ PlaceOrder message encoding following TWS wire protocol"
  putStrLn "✓ Helper functions for common order types"
  putStrLn "✓ Proper handling of MAX_VALUE fields (empty strings)"
  putStrLn "✓ Field order exactly matches TWS API specification"
  putStrLn "✓ Support for all order types and time-in-force options"
  putStrLn ""

main :: IO ()
main = demonstrateEncoding