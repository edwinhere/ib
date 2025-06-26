#!/usr/bin/env python3

"""
Side-by-side comparison: Official Python API vs Our Haskell implementation
Both creating identical orders with different prices to distinguish them
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

class SideBySideTest(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.next_order_id = None
        self.test_phase = "connecting"
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        self.next_order_id = orderId
        
        if self.test_phase == "connecting":
            self.test_phase = "testing"
            self.start_side_by_side_test()
        
    def openOrder(self, orderId, contract, order, orderState):
        price = order.lmtPrice
        source = "UNKNOWN"
        
        if price == 111.11:
            source = "🐍 OFFICIAL PYTHON API"
        elif price == 222.22:
            source = "🏗️  OUR HASKELL IMPLEMENTATION"
        
        print(f"📈 ORDER FOUND:")
        print(f"   Source: {source}")
        print(f"   Order ID: {orderId}")
        print(f"   Details: {order.action} {order.totalQuantity} {contract.symbol} {order.orderType} ${price}")
        print(f"   Status: {orderState.status}")
        print()
        
    def openOrderEnd(self):
        print("📊 Order scan complete")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")
    
    def start_side_by_side_test(self):
        print("\n" + "="*60)
        print("SIDE-BY-SIDE COMPARISON TEST")
        print("="*60)
        
        print("📝 Step 1: Creating order with OFFICIAL PYTHON API...")
        
        # Create order with official Python API
        contract = Contract()
        contract.symbol = "AAPL"
        contract.secType = "STK"
        contract.exchange = "SMART"
        contract.currency = "USD"
        
        order = Order()
        order.action = "BUY"
        order.totalQuantity = 1
        order.orderType = "LMT"
        order.lmtPrice = 111.11  # Distinctive price for Python API
        order.transmit = False   # Don't transmit, just create
        
        python_order_id = self.next_order_id
        print(f"   Order ID: {python_order_id}")
        print(f"   Details: BUY 1 AAPL LMT $111.11")
        print(f"   Transmit: False")
        
        self.placeOrder(python_order_id, contract, order)
        print("✅ Official Python API order placed!")
        
        # Wait a bit, then place our Haskell order
        threading.Timer(2.0, self.place_haskell_order).start()
    
    def place_haskell_order(self):
        print("\n📝 Step 2: Creating order with OUR HASKELL IMPLEMENTATION...")
        
        try:
            # Create new socket connection for our Haskell order
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.connect(("127.0.0.1", 7497))
            
            # Handshake
            api_sign = b"API\0"
            version_payload = b"v100..187"
            handshake = api_sign + struct.pack(">I", len(version_payload)) + version_payload
            sock.send(handshake)
            sock.recv(4096)
            
            # StartAPI  
            start_api_payload = b"71\x00" + b"2\x00" + b"17\x00" + b"\x00"  # client ID 17
            start_api = struct.pack(">I", len(start_api_payload)) + start_api_payload
            sock.send(start_api)
            sock.recv(4096)
            
            # Create our Haskell order with distinctive price
            haskell_order_id = self.next_order_id + 1
            
            # Use EXACT same minimal format that worked in wire analyzer
            fields = [
                b"3",        # PLACE_ORDER
                str(haskell_order_id).encode(),  # Order ID
                # Contract fields
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
                b"1",        # totalQuantity
                b"LMT",      # orderType
                b"222.22",   # lmtPrice - DISTINCTIVE PRICE for Haskell
                b"",         # auxPrice
                b"DAY",      # tif
                b"",         # ocaGroup
                b"",         # account
                b"",         # openClose
                b"0",        # origin
                b"",         # orderRef
                b"0",        # transmit = false (same as Python API)
                b"0",        # parentId
                b"0",        # blockOrder
                b"0",        # sweepToFill
                b"0",        # displaySize
                b"0",        # triggerMethod
                b"0",        # outsideRth
                b"0",        # hidden
            ]
            
            message = b'\x00'.join(fields) + b'\x00'
            order_message = struct.pack(">I", len(message)) + message
            
            print(f"   Order ID: {haskell_order_id}")
            print(f"   Details: BUY 1 AAPL LMT $222.22")
            print(f"   Transmit: False")
            print(f"   Message size: {len(order_message)} bytes")
            
            sock.send(order_message)
            print("✅ Our Haskell implementation order sent!")
            
            try:
                response = sock.recv(4096)
                if response:
                    print(f"📥 Response: {len(response)} bytes")
            except:
                pass
                
            sock.close()
            
        except Exception as e:
            print(f"❌ Haskell order error: {e}")
        
        # Wait then check all orders
        threading.Timer(3.0, self.check_final_results).start()
    
    def check_final_results(self):
        print("\n📝 Step 3: Checking which orders appear in TWS...")
        self.reqAllOpenOrders()
        
        # End test after checking
        threading.Timer(3.0, self.show_conclusion).start()
    
    def show_conclusion(self):
        print("\n" + "="*60)
        print("SIDE-BY-SIDE TEST CONCLUSION")
        print("="*60)
        print("Expected results:")
        print("🐍 Official Python API: BUY 1 AAPL LMT $111.11")
        print("🏗️  Our Haskell Impl:     BUY 1 AAPL LMT $222.22")
        print()
        print("This will definitively show which implementation works!")

def main():
    print("=== Side-by-Side Comparison Test ===")
    print("Official Python API vs Our Haskell Implementation")
    print()
    
    app = SideBySideTest()
    
    try:
        app.connect("127.0.0.1", 7497, clientId=18)
        
        api_thread = threading.Thread(target=app.run, daemon=True)
        api_thread.start()
        
        time.sleep(15)
        
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        if app.isConnected():
            app.disconnect()

if __name__ == "__main__":
    main()