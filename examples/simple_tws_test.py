#!/usr/bin/env python3

"""
Simple script to test TWS connectivity and place a real order
using the official Python API.
"""

import sys
import os
import time
import threading

# Add the official TWS Python API to path
sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order

class TestApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.nextValidOrderId = None
        self.connected = False
        
    def connectAck(self):
        print("✓ Connected to TWS")
        self.connected = True
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        self.nextValidOrderId = orderId
        
        # Place a small test order immediately
        self.place_test_order(orderId)
        
    def orderStatus(self, orderId, status, filled, remaining, avgFillPrice, 
                   permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice):
        print(f"📋 Order Status - ID: {orderId}, Status: {status}, Filled: {filled}")
        
    def openOrder(self, orderId, contract, order, orderState):
        print(f"📈 Open Order - ID: {orderId}, Symbol: {contract.symbol}, Action: {order.action}")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode == 202:  # Order cancelled
            print(f"ℹ️  Order {reqId}: {errorString}")
        elif errorCode < 2000:  # System errors
            print(f"❌ Error {errorCode}: {errorString}")
        else:  # Warnings
            print(f"⚠️  Warning {errorCode}: {errorString}")
        
    def place_test_order(self, order_id):
        print(f"\n🔄 Placing test order with ID: {order_id}")
        
        # Create AAPL contract
        contract = Contract()
        contract.symbol = "AAPL"
        contract.secType = "STK"
        contract.exchange = "SMART"
        contract.currency = "USD"
        
        # Create a VERY small limit order that won't execute
        order = Order()
        order.action = "BUY"
        order.totalQuantity = 1  # Just 1 share
        order.orderType = "LMT"
        order.lmtPrice = 0.01  # $0.01 - will never fill for AAPL
        order.transmit = True
        
        print(f"📊 Order details:")
        print(f"   Symbol: {contract.symbol}")
        print(f"   Action: {order.action}")
        print(f"   Quantity: {order.totalQuantity}")
        print(f"   Type: {order.orderType}")
        print(f"   Limit Price: ${order.lmtPrice}")
        print("")
        
        # Place the order
        self.placeOrder(order_id, contract, order)
        print("✅ Order placed! Check TWS for confirmation.")

def main():
    print("=== TWS Connection and Order Test ===")
    print("This will place a test limit order for 1 AAPL @ $0.01")
    print("(The order won't execute due to low price)")
    print("")
    
    app = TestApp()
    
    try:
        print("🔌 Connecting to TWS on localhost:7497...")
        app.connect("127.0.0.1", 7497, clientId=1)
        
        # Start the socket in a thread
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        # Wait for connection and order
        time.sleep(5)
        
        if app.connected and app.nextValidOrderId:
            print("\n✅ Test completed successfully!")
            print("📱 Check your TWS application:")
            print("   - You should see the order in the order book")
            print("   - Order should be pending (won't fill at $0.01)")
            print("   - You can cancel it manually from TWS")
        else:
            print("❌ Test failed - check TWS connection")
            
    except Exception as e:
        print(f"❌ Error: {e}")
        print("💡 Make sure TWS is running with API enabled on port 7497")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()