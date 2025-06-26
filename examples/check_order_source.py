#!/usr/bin/env python3

"""
Check which implementation created the order by examining order IDs
"""

import sys
import time
import threading

sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper

class OrderSourceChecker(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        print("📋 Checking all pending orders...")
        self.reqAllOpenOrders()
        
    def openOrder(self, orderId, contract, order, orderState):
        source = "UNKNOWN"
        
        # Determine source based on order ID ranges we used
        if orderId == 1:
            source = "🐍 OFFICIAL PYTHON API (wire analyzer)"
        elif orderId == 11:
            source = "🏗️  OUR HASKELL IMPLEMENTATION (wire analyzer)"
        elif orderId >= 100 and orderId < 200:
            source = "🏗️  HASKELL IMPLEMENTATION (comprehensive test)"
        elif orderId >= 200:
            source = "🏗️  IMPROVED HASKELL IMPLEMENTATION"
        elif orderId >= 1000:
            source = "🏗️  STANDALONE HASKELL TEST"
        else:
            source = "🐍 OFFICIAL PYTHON API"
            
        print(f"📈 ORDER FOUND:")
        print(f"   Source: {source}")
        print(f"   Order ID: {orderId}")
        print(f"   Contract: {contract.symbol} {contract.secType}")
        print(f"   Order: {order.action} {order.totalQuantity} {order.orderType}")
        print(f"   Price: ${order.lmtPrice}")
        print(f"   Status: {orderState.status}")
        print(f"   Transmit: {order.transmit}")
        print()
        
    def openOrderEnd(self):
        print("📊 Order scan complete")
        print()
        if hasattr(self, '_found_orders'):
            print("🎯 CONCLUSION:")
            if any("HASKELL" in order for order in self._found_orders):
                print("   ✅ SUCCESS! Our Haskell implementation is working!")
                print("   🎉 The wire format is compatible with TWS!")
            else:
                print("   ℹ️  Only official Python API orders found")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")

def main():
    print("=== Order Source Identification ===")
    print("Checking which implementation created the AAPL order")
    print()
    
    app = OrderSourceChecker()
    
    try:
        app.connect("127.0.0.1", 7497, clientId=15)
        
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        time.sleep(5)
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()