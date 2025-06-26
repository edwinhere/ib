#!/usr/bin/env python3

"""
Wire Format Capture Tool
Intercepts the exact bytes sent by the official Python API and our implementation
"""

import sys
import time
import threading
import socket
import struct
from unittest.mock import patch

sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order

class WireFormatCapture(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.next_order_id = None
        self.captured_messages = []
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        self.next_order_id = orderId
        self.start_capture()
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        if errorCode < 2000:
            print(f"❌ Error {errorCode}: {errorString}")
        else:
            print(f"⚠️  Warning {errorCode}: {errorString}")
    
    def start_capture(self):
        print("\n=== WIRE FORMAT CAPTURE ===")
        print("Step 1: Capturing official Python API wire format...")
        
        # Patch the socket send method to capture outgoing data
        original_send = socket.socket.send
        captured_data = []
        
        def capture_send(self_sock, data):
            captured_data.append(data)
            print(f"📤 Captured {len(data)} bytes: {data[:50].hex()}...")
            return original_send(self_sock, data)
        
        with patch.object(socket.socket, 'send', capture_send):
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
            order.lmtPrice = 333.33  # Distinctive price for capture
            order.transmit = False
            
            print(f"📝 Creating order: BUY 1 AAPL LMT $333.33")
            print(f"   Order ID: {self.next_order_id}")
            
            self.placeOrder(self.next_order_id, contract, order)
            time.sleep(0.5)  # Give time for message to be sent
        
        # Analyze captured data
        print(f"\n📊 OFFICIAL API CAPTURED {len(captured_data)} messages:")
        for i, data in enumerate(captured_data):
            print(f"Message {i+1}: {len(data)} bytes")
            if len(data) > 8:  # Skip handshake/setup messages
                print(f"  Hex: {data.hex()}")
                print(f"  ASCII: {self.hex_to_ascii(data)}")
                
                # Check if this looks like a PlaceOrder message
                if data[4:6] == b'3\x00':  # Message ID 3 = PlaceOrder
                    print(f"  🎯 PLACEORDER MESSAGE FOUND!")
                    self.analyze_placeorder_message(data)
        
        # Now create our implementation and compare
        threading.Timer(2.0, self.create_our_implementation).start()
    
    def analyze_placeorder_message(self, data):
        """Analyze a PlaceOrder message byte by byte"""
        print(f"  📋 PlaceOrder Analysis:")
        
        # Skip length header (first 4 bytes)
        payload = data[4:]
        
        # Split by null bytes to see fields
        fields = payload.split(b'\x00')
        print(f"  📊 Found {len(fields)} fields:")
        
        for i, field in enumerate(fields[:20]):  # Show first 20 fields
            if field:
                ascii_repr = field.decode('utf-8', errors='replace')
                print(f"    Field {i:2d}: '{ascii_repr}' ({field.hex()})")
            else:
                print(f"    Field {i:2d}: <empty>")
        
        if len(fields) > 20:
            print(f"    ... and {len(fields) - 20} more fields")
    
    def hex_to_ascii(self, data):
        """Convert hex data to readable ASCII, replacing non-printable chars"""
        result = ""
        for byte in data:
            if 32 <= byte <= 126:  # Printable ASCII
                result += chr(byte)
            elif byte == 0:
                result += "\\0"
            else:
                result += f"\\x{byte:02x}"
        return result[:100] + "..." if len(result) > 100 else result
    
    def create_our_implementation(self):
        print(f"\n📝 Step 2: Creating our implementation message...")
        
        # Create our message with identical parameters
        order_id = self.next_order_id + 1
        
        fields = [
            b"3",        # PLACE_ORDER
            str(order_id).encode(),  # Order ID
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
            b"333.33",   # lmtPrice - SAME as official API
            b"",         # auxPrice
            b"DAY",      # tif
            b"",         # ocaGroup
            b"",         # account
            b"",         # openClose
            b"0",        # origin
            b"",         # orderRef
            b"0",        # transmit = false
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
        
        print(f"📤 OUR IMPLEMENTATION:")
        print(f"   Order ID: {order_id}")
        print(f"   Message size: {len(order_message)} bytes")
        print(f"   Field count: {len(fields)}")
        print(f"   Hex: {order_message.hex()}")
        print(f"   ASCII: {self.hex_to_ascii(order_message)}")
        
        # Analyze our message
        payload = order_message[4:]
        our_fields = payload.split(b'\x00')
        print(f"  📊 Our {len(our_fields)} fields:")
        
        for i, field in enumerate(our_fields[:20]):
            if field:
                ascii_repr = field.decode('utf-8', errors='replace')
                print(f"    Field {i:2d}: '{ascii_repr}' ({field.hex()})")
            else:
                print(f"    Field {i:2d}: <empty>")
        
        # Send our message
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
            start_api_payload = b"71\x00" + b"2\x00" + b"19\x00" + b"\x00"
            start_api = struct.pack(">I", len(start_api_payload)) + start_api_payload
            sock.send(start_api)
            sock.recv(4096)
            
            # Send our order
            sock.send(order_message)
            print(f"✅ Our implementation message sent!")
            
            try:
                response = sock.recv(4096)
                if response:
                    print(f"📥 Response: {len(response)} bytes - {response.hex()}")
            except:
                pass
            
            sock.close()
            
        except Exception as e:
            print(f"❌ Error sending our message: {e}")
        
        threading.Timer(2.0, self.show_comparison).start()
    
    def show_comparison(self):
        print(f"\n=== COMPARISON RESULTS ===")
        print(f"Now check TWS for:")
        print(f"🐍 Official API: Should see BUY 1 AAPL LMT $333.33 (working)")
        print(f"🏗️  Our Impl:     Should see another $333.33 order (if working)")
        print(f"")
        print(f"📊 Analysis:")
        print(f"- Compare field counts between official API and our implementation")
        print(f"- Look for missing or extra fields")
        print(f"- Check field encoding differences")
        print(f"- Examine field ordering")

def main():
    print("=== Wire Format Capture Tool ===")
    print("Capturing exact bytes from official Python API")
    print()
    
    app = WireFormatCapture()
    
    try:
        app.connect("127.0.0.1", 7497, clientId=20)
        
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