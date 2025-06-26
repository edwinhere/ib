{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlaceOrderTest (spec) where

import Test.Hspec
import IB.Protocol.Types
import IB.Client
import IB.Codec.Encoder (encodeMessages, messageBuilder)
import IB.Network.Framing (frame)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Conduit

-- Test that PlaceOrder messages can be created and encoded
spec :: Spec
spec = do
  describe "PlaceOrder Implementation" $ do
    describe "Order Creation" $ do
      it "should create a simple market order" $ do
        let contract = defaultContract "AAPL"
            order = simpleMarketOrder BUY 100.0
            req = placeOrder 1001 contract order
        
        placeOrderId req `shouldBe` 1001
        symbol (placeOrderContract req) `shouldBe` "AAPL"
        orderAction (placeOrderOrder req) `shouldBe` BUY
        orderTotalQuantity (placeOrderOrder req) `shouldBe` 100.0
        orderType (placeOrderOrder req) `shouldBe` MKT
        orderLmtPrice (placeOrderOrder req) `shouldBe` Nothing

      it "should create a simple limit order" $ do
        let contract = defaultContract "MSFT"
            order = simpleLimitOrder BUY 50.0 150.0
            req = placeOrder 1002 contract order
        
        placeOrderId req `shouldBe` 1002
        symbol (placeOrderContract req) `shouldBe` "MSFT"
        orderAction (placeOrderOrder req) `shouldBe` BUY
        orderTotalQuantity (placeOrderOrder req) `shouldBe` 50.0
        orderType (placeOrderOrder req) `shouldBe` LMT
        orderLmtPrice (placeOrderOrder req) `shouldBe` Just 150.0

      it "should create a complex order with custom settings" $ do
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
              , orderTransmit = False
              , orderOutsideRth = True
              }
            req = placeOrder 1003 contract order
        
        placeOrderId req `shouldBe` 1003
        exchange (placeOrderContract req) `shouldBe` "NASDAQ"
        orderAction (placeOrderOrder req) `shouldBe` SELL
        orderTif (placeOrderOrder req) `shouldBe` GTC
        orderAccount (placeOrderOrder req) `shouldBe` "U123456"
        orderTransmit (placeOrderOrder req) `shouldBe` False
        orderOutsideRth (placeOrderOrder req) `shouldBe` True

    describe "Message Encoding" $ do
      it "should encode PlaceOrder message to bytes" $ do
        let contract = defaultContract "AAPL"
            order = simpleMarketOrder BUY 100.0
            req = placeOrder 1001 contract order
            clientMsg = PlaceOrder req
        
        -- Test that encoding produces non-empty bytes
        let encoded = runConduitPure $ yield clientMsg .| encodeMessages .| sinkList
        encoded `shouldSatisfy` (not . null)
        
        -- Test that encoded message contains expected data
        let firstMsg = head encoded
        firstMsg `shouldSatisfy` (BS.isInfixOf "AAPL")
        firstMsg `shouldSatisfy` (BS.isInfixOf "BUY")
        firstMsg `shouldSatisfy` (BS.isInfixOf "MKT")

      it "should encode PlaceOrder with frame wrapper" $ do
        let contract = defaultContract "MSFT"
            order = simpleLimitOrder BUY 50.0 150.0
            req = placeOrder 1002 contract order
            clientMsg = PlaceOrder req
        
        -- Test encoding with framing
        let framedEncoded = runConduitPure $ 
              yield clientMsg .| encodeMessages .| frame .| sinkList
        framedEncoded `shouldSatisfy` (not . null)
        
        -- Framed message should be longer than unfra med (due to length header)
        let unframed = runConduitPure $ yield clientMsg .| encodeMessages .| sinkList
        let framed = head framedEncoded
        let plain = head unframed
        
        BS.length framed `shouldSatisfy` (> BS.length plain)

      it "should handle different order types correctly" $ do
        let contract = defaultContract "TEST"
            testOrders = 
              [ (simpleMarketOrder BUY 100.0, MKT)
              , (simpleLimitOrder SELL 50.0 99.99, LMT)
              , ((defaultOrder BUY 25.0 STP) { orderAuxPrice = Just 101.0 }, STP)
              ]
        
        forM_ testOrders $ \(order, expectedType) -> do
          let req = placeOrder 9999 contract order
              clientMsg = PlaceOrder req
              encoded = runConduitPure $ yield clientMsg .| encodeMessages .| sinkList
          
          encoded `shouldSatisfy` (not . null)
          orderType order `shouldBe` expectedType

    describe "Default Values" $ do
      it "should have sensible defaults for orders" $ do
        let order = defaultOrder BUY 100.0 MKT
        
        orderAction order `shouldBe` BUY
        orderTotalQuantity order `shouldBe` 100.0
        orderType order `shouldBe` MKT
        orderTif order `shouldBe` DAY
        orderTransmit order `shouldBe` True
        orderOrigin order `shouldBe` 0  -- Customer
        orderExemptCode order `shouldBe` (-1)

      it "should have sensible defaults for contracts" $ do
        let contract = defaultContract "TEST"
        
        symbol contract `shouldBe` "TEST"
        secType contract `shouldBe` STK
        exchange contract `shouldBe` "SMART"
        currency contract `shouldBe` "USD"
        strike contract `shouldBe` 0.0
        right contract `shouldBe` Nothing

-- Helper function for testing
forM_ :: [a] -> (a -> IO b) -> IO ()
forM_ [] _ = return ()
forM_ (x:xs) f = f x >> forM_ xs f