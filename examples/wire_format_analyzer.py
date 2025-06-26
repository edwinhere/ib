#!/usr/bin/env python3

"""
Analyze the exact wire format differences between official Python API and our implementation
"""

import sys
import time
import threading
import socket
import struct

sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order

class WireFormatAnalyzer(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.next_order_id = None
        self.captured_messages = []
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        self.next_order_id = orderId
        self.start_analysis()
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")
    
    def start_analysis(self):
        print("\n=== Wire Format Analysis ===")
        
        # Create a minimal order first with official API
        print("📝 Step 1: Creating order with official Python API...")
        
        contract = Contract()
        contract.symbol = "AAPL"
        contract.secType = "STK"
        contract.exchange = "SMART"
        contract.currency = "USD"
        
        order = Order()
        order.action = "BUY"
        order.totalQuantity = 1
        order.orderType = "LMT"
        order.lmtPrice = 130.0
        order.transmit = False  # Don't actually transmit
        
        # This will generate the wire format we can analyze
        print(f"📤 Official API order details:")
        print(f"   Order ID: {self.next_order_id}")
        print(f"   Symbol: {contract.symbol}")
        print(f"   Action: {order.action} {order.totalQuantity} {order.orderType} ${order.lmtPrice}")
        
        # Place the order but don't transmit it
        self.placeOrder(self.next_order_id, contract, order)
        print("✅ Official API order created (not transmitted)")
        
        # Now let's create our version and compare
        threading.Timer(1.0, self.create_our_version).start()
    
    def create_our_version(self):
        print("\n📝 Step 2: Creating our implementation version...")
        
        # Connect via raw socket to place our order
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(("127.0.0.1", 7497))
            
            # Handshake
            api_sign = b"API\0"
            version_payload = b"v100..187"
            handshake = api_sign + struct.pack(">I", len(version_payload)) + version_payload
            sock.send(handshake)
            sock.recv(4096)
            
            # StartAPI
            start_api_payload = b"71\x00" + b"2\x00" + b"13\x00" + b"\x00"  # client ID 13
            start_api = struct.pack(">I", len(start_api_payload)) + start_api_payload
            sock.send(start_api)
            sock.recv(4096)
            
            # Create minimal order matching the official API exactly
            order_id = self.next_order_id + 10
            
            # MINIMAL FIELDS ONLY - let's try the absolute minimum
            minimal_fields = [
                b"3",        # PLACE_ORDER
                str(order_id).encode(),  # Order ID
                # Contract
                b"0",        # conId 
                b"AAPL",     # symbol
                b"STK",      # secType
                b"",         # expiry
                b"0.0",      # strike
                b"",         # right
                b"",         # multiplier
                b"SMART",    # exchange
                b"",         # primaryExchange
                b"USD",      # currency
                b"",         # localSymbol
                b"",         # tradingClass
                b"",         # secIdType
                b"",         # secId
                # Order fields
                b"BUY",      # action
                b"1",        # totalQuantity (try as int first)
                b"LMT",      # orderType
                b"130.0",    # lmtPrice
                b"",         # auxPrice
                b"DAY",      # tif
                b"",         # ocaGroup
                b"",         # account
                b"",         # openClose
                b"0",        # origin
                b"",         # orderRef
                b"0",        # transmit = false (like official API)
                b"0",        # parentId
                b"0",        # blockOrder
                b"0",        # sweepToFill
                b"0",        # displaySize
                b"0",        # triggerMethod
                b"0",        # outsideRth
                b"0",        # hidden
            ]
            
            message = b'\x00'.join(minimal_fields) + b'\x00'
            order_message = struct.pack(">I", len(message)) + message
            
            print(f"📤 Our implementation order details:")
            print(f"   Order ID: {order_id}")
            print(f"   Message size: {len(order_message)} bytes")
            print(f"   Field count: {len(minimal_fields)}")
            print(f"   Transmit: false (matching official API)")
            
            # Let's also print the hex dump for comparison
            print(f"📊 Message hex dump (first 100 bytes):")
            hex_dump = ' '.join(f'{b:02x}' for b in order_message[:100])
            print(f"   {hex_dump}")
            
            sock.send(order_message)
            print("✅ Our implementation order sent")
            
            try:
                response = sock.recv(4096)
                if response:
                    print(f"📥 Response: {len(response)} bytes")
                    # Print response hex
                    resp_hex = ' '.join(f'{b:02x}' for b in response[:50])
                    print(f"   Response hex: {resp_hex}")
            except:
                print("ℹ️  No response")
            
            sock.close()
            
        except Exception as e:
            print(f"❌ Socket error: {e}")
        
        # Check orders after a delay
        threading.Timer(2.0, self.check_final_orders).start()
    
    def check_final_orders(self):
        print("\n📝 Step 3: Checking all orders...")
        self.reqAllOpenOrders()
        
        # End test after checking
        threading.Timer(2.0, self.end_analysis).start()
    
    def openOrder(self, orderId, contract, order, orderState):
        print(f"📈 Found Order - ID: {orderId}, Symbol: {contract.symbol}, Action: {order.action}, Type: {order.orderType}, Status: {orderState.status}, Transmit: {order.transmit}")
    
    def openOrderEnd(self):
        print("📊 Order scan complete")
    
    def end_analysis(self):
        print("\n=== Analysis Complete ===")
        print("Key Observations:")
        print("1. Both orders use transmit=false to avoid actual execution")
        print("2. Compare field counts and message sizes")
        print("3. Check if minimal field set works")
        print("4. Official API orders should appear in order list")

def main():
    print("=== Wire Format Analysis ===")
    print("Comparing official Python API vs our implementation")
    print()
    
    app = WireFormatAnalyzer()
    
    try:
        app.connect("127.0.0.1", 7497, clientId=14)
        
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        time.sleep(10)
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()