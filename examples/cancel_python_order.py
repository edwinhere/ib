#!/usr/bin/env python3

"""
Cancel the order placed by the Python script and check order status
"""

import sys
import time
import threading

# Add the official TWS Python API to path
sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper

class CancelApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.orders_found = []
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        # Request all open orders
        print("📋 Requesting all open orders...")
        self.reqAllOpenOrders()
        
    def openOrder(self, orderId, contract, order, orderState):
        print(f"📈 Found Order - ID: {orderId}, Symbol: {contract.symbol}, Action: {order.action}, Status: {orderState.status}")
        self.orders_found.append(orderId)
        
        # Cancel this order if it's AAPL
        if contract.symbol == "AAPL":
            print(f"🗑️  Cancelling AAPL order {orderId}...")
            from ibapi.order_cancel import OrderCancel
            cancel = OrderCancel()
            self.cancelOrder(orderId, cancel)
        
    def orderStatus(self, orderId, status, filled, remaining, avgFillPrice, 
                   permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice):
        print(f"📋 Order Status - ID: {orderId}, Status: {status}")
        if status == "Cancelled":
            print(f"✅ Order {orderId} successfully cancelled")
        
    def openOrderEnd(self):
        print(f"📊 Found {len(self.orders_found)} total orders")
        if not self.orders_found:
            print("ℹ️  No open orders found")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode == 202:  # Order cancelled
            print(f"✅ Order {reqId}: {errorString}")
        elif errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")

def main():
    print("=== Cancelling Python Orders ===")
    print("Connecting to TWS to cancel existing orders...")
    print("")
    
    app = CancelApp()
    
    try:
        print("🔌 Connecting to TWS...")
        app.connect("127.0.0.1", 7497, clientId=3)
        
        # Start the socket in a thread
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        # Wait for operations to complete
        time.sleep(5)
        
        print("\n✅ Cancellation process completed!")
        print("📱 Check TWS - Python orders should be cancelled")
            
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()