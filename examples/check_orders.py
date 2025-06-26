#!/usr/bin/env python3

"""
Check for any existing orders in TWS
"""

import sys
import time
import threading

sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper

class OrderCheckApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.order_count = 0
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        print("📋 Checking for existing orders...")
        self.reqAllOpenOrders()
        
    def openOrder(self, orderId, contract, order, orderState):
        self.order_count += 1
        print(f"📈 Found Order - ID: {orderId}, Symbol: {contract.symbol}, Action: {order.action}, Type: {order.orderType}, Status: {orderState.status}")
        
    def openOrderEnd(self):
        if self.order_count == 0:
            print("✅ No existing orders found - ready for testing!")
        else:
            print(f"📊 Found {self.order_count} existing orders")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")

def main():
    print("=== Checking Existing Orders ===")
    
    app = OrderCheckApp()
    
    try:
        app.connect("127.0.0.1", 7497, clientId=6)
        
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        time.sleep(4)
        
        if app.order_count == 0:
            print("\n🎯 TWS order book is clean!")
            print("📝 Ready to test Haskell order placement")
        else:
            print(f"\n📋 {app.order_count} orders still exist")
            print("🔧 Please cancel manually from TWS GUI if needed")
            
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()