#!/usr/bin/env python3

"""
Simple order cancellation script
"""

import sys
import time
import threading

sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper

class SimpleCancelApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        
        # Try to cancel order IDs 1 and 2 (the ones we know exist)
        from ibapi.order_cancel import OrderCancel
        cancel = OrderCancel()
        
        print("🗑️ Cancelling order ID 1...")
        self.cancelOrder(1, cancel)
        
        print("🗑️ Cancelling order ID 2...")  
        self.cancelOrder(2, cancel)
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode == 10148:  # Order already cancelled
            print(f"ℹ️ Order {reqId}: Already cancelled")
        elif errorCode == 10147:  # Order not found
            print(f"ℹ️ Order {reqId}: Not found (may already be cancelled)")
        elif "cancelled" in errorString.lower():
            print(f"✅ Order {reqId}: {errorString}")
        else:
            print(f"⚠️ Code {errorCode}: {errorString}")

def main():
    print("=== Manual Order Cancellation ===")
    
    app = SimpleCancelApp()
    
    try:
        app.connect("127.0.0.1", 7497, clientId=5)
        
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        time.sleep(3)
        print("✅ Cancellation attempts completed")
            
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()