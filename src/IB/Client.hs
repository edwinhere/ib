{-# LANGUAGE OverloadedStrings #-}

module IB.Client 
  ( placeOrder
  , simpleMarketOrder
  , simpleLimitOrder
  , defaultContract
  ) where

import IB.Protocol.Types
import Data.Text (Text)

-- | Place an order using the Interactive Brokers API
placeOrder :: Int -> Contract -> Order -> PlaceOrderRequest
placeOrder orderId contract order = PlaceOrderRequest
  { placeOrderId = orderId
  , placeOrderContract = contract
  , placeOrderOrder = order
  }

-- | Create a simple market order
simpleMarketOrder :: OrderAction -> Double -> Order
simpleMarketOrder action quantity = defaultOrder action quantity MKT

-- | Create a simple limit order
simpleLimitOrder :: OrderAction -> Double -> Double -> Order
simpleLimitOrder action quantity price = (defaultOrder action quantity LMT) { orderLmtPrice = Just price }

-- | Create a default stock contract
defaultContract :: Text -> Contract
defaultContract sym = Contract
  { conId = Nothing
  , symbol = sym
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
