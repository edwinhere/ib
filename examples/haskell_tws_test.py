#!/usr/bin/env python3

"""
Script to capture the exact wire format that the official Python API sends,
so we can compare it with our Haskell implementation.
"""

import sys
import socket
import struct
import threading
import time

# Add the official TWS Python API to path
sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order

class WireCapture:
    def __init__(self):
        self.captured_data = []
        
    def capture_send(self, original_send, data):
        """Capture data being sent to TWS"""
        self.captured_data.append(data)
        print(f"📡 Captured {len(data)} bytes: {data[:50].hex()}...")
        return original_send(data)

class TestApp(EWrapper, EClient):
    def __init__(self, wire_capture):
        EClient.__init__(self, self)
        self.wire_capture = wire_capture
        self.nextValidOrderId = None
        
        # Monkey patch the socket send to capture wire data
        original_send = self.conn.socket.send
        self.conn.socket.send = lambda data: wire_capture.capture_send(original_send, data)
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        self.nextValidOrderId = orderId
        
        # Place the same order as our Haskell test
        self.place_comparison_order(orderId)
        
    def orderStatus(self, orderId, status, filled, remaining, avgFillPrice, 
                   permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice):
        print(f"📋 Order Status - ID: {orderId}, Status: {status}")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")
        
    def place_comparison_order(self, order_id):
        print(f"🔄 Placing order for wire format comparison...")
        
        # Create exactly the same order as our Haskell implementation
        contract = Contract()
        contract.symbol = "AAPL"
        contract.secType = "STK"
        contract.exchange = "SMART"
        contract.currency = "USD"
        
        order = Order()
        order.action = "BUY"
        order.totalQuantity = 1.0
        order.orderType = "MKT"  # Market order like our Haskell version
        order.transmit = True
        
        print(f"📊 Placing: BUY 1 AAPL MKT")
        
        # This will be captured by our wire tap
        self.placeOrder(order_id, contract, order)
        
        print("✅ Order sent - wire format captured!")

def extract_place_order_message(captured_data):
    """Extract just the PlaceOrder message from captured data"""
    for data in captured_data:
        if len(data) > 8:  # Has length header + some content
            # Check if this looks like a PlaceOrder message
            if len(data) > 10:
                # Skip length header, check message type
                msg_start = data[4:]  # Skip 4-byte length
                if msg_start.startswith(b'3\x00'):  # PlaceOrder message type
                    print(f"\n🎯 Found PlaceOrder message ({len(data)} bytes):")
                    print(f"Full hex: {data.hex()}")
                    print(f"First 64 bytes: {data[:64].hex()}")
                    return data
    return None

def create_haskell_comparison():
    """Show what our Haskell implementation would generate"""
    print(f"\n🏗️  Our Haskell implementation generates:")
    print(f"   Message type: 3 (0x33)")
    print(f"   Order ID: {1} (0x31)")  
    print(f"   Symbol: AAPL (0x4141504c)")
    print(f"   SecType: STK (0x53544b)")
    print(f"   Action: BUY (0x425559)")
    print(f"   Quantity: 1.0 (0x312e30)")
    print(f"   Order type: MKT (0x4d4b54)")
    print(f"   All with null terminators (0x00)")

def main():
    print("=== Wire Format Comparison Test ===")
    print("Capturing official Python API wire format...")
    print("")
    
    wire_capture = WireCapture()
    app = TestApp(wire_capture)
    
    try:
        print("🔌 Connecting to TWS...")
        app.connect("127.0.0.1", 7497, clientId=2)
        
        # Start the socket in a thread
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        # Wait for the order to be placed
        time.sleep(3)
        
        print(f"\n📡 Captured {len(wire_capture.captured_data)} messages")
        
        # Find and analyze the PlaceOrder message
        place_order_msg = extract_place_order_message(wire_capture.captured_data)
        
        if place_order_msg:
            print(f"\n✅ Successfully captured PlaceOrder wire format!")
            create_haskell_comparison()
            print(f"\n🔍 Compare this with our Haskell output to verify compatibility")
        else:
            print(f"❌ PlaceOrder message not found in capture")
            
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()