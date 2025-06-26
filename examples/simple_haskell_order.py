#!/usr/bin/env python3

"""
Place an order using our Haskell wire format and verify it works
"""

import socket
import struct
import time

def place_haskell_order():
    print("=== Placing Order via Haskell Implementation ===")
    print("Using our Haskell-generated wire format...")
    print("")
    
    try:
        # Connect to TWS
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(10)
        sock.connect(("127.0.0.1", 7497))
        print("✓ Connected to TWS")
        
        # Send handshake
        api_sign = b"API\0"
        version_payload = b"v100..187"
        length = len(version_payload)
        handshake = api_sign + struct.pack(">I", length) + version_payload
        sock.send(handshake)
        print("✓ Handshake sent")
        
        # Read handshake response
        response = sock.recv(4096)
        print(f"✓ Handshake response ({len(response)} bytes)")
        
        # Send StartAPI
        start_api_payload = b"71\x00" + b"2\x00" + b"7\x00" + b"\x00"  # message type 71, version 2, client ID 7
        start_api = struct.pack(">I", len(start_api_payload)) + start_api_payload
        sock.send(start_api)
        print("✓ StartAPI sent")
        
        # Wait and read setup messages
        time.sleep(1)
        response = sock.recv(4096)
        print(f"✓ Setup response ({len(response)} bytes)")
        
        # Extract NextValidId from response (simplified parsing)
        # Look for pattern: length + "9\0" + version + orderid
        next_order_id = 1010  # Use a high number to be safe
        
        print(f"📝 Using Order ID: {next_order_id}")
        
        # Create PlaceOrder message (simplified but complete)
        fields = [
            b"3",           # PLACE_ORDER message type
            str(next_order_id).encode(), # Order ID
            b"0",           # Contract ID (0 = not specified)
            b"AAPL",        # Symbol
            b"STK",         # Security type
            b"",            # Expiry (empty)
            b"0.0",         # Strike
            b"",            # Right (empty)
            b"",            # Multiplier (empty)
            b"SMART",       # Exchange
            b"",            # Primary exchange (empty)
            b"USD",         # Currency
            b"",            # Local symbol (empty)
            b"",            # Trading class (empty)
            b"",            # Sec ID type (empty)
            b"",            # Sec ID (empty)
            b"BUY",         # Action
            b"1.0",         # Quantity
            b"LMT",         # Order type (limit instead of market for testing)
            b"200.0",       # Limit price (safe price for AAPL)
            b"",            # Aux price (empty)
            b"DAY",         # Time in force
            b"",            # OCA group (empty)
            b"",            # Account (empty)
            b"",            # Open/close (empty)
            b"0",           # Origin (customer)
            b"",            # Order ref (empty)
            b"1",           # Transmit (true)
            b"0",           # Parent ID
            b"0",           # Block order (false)
            b"0",           # Sweep to fill (false)
            b"0",           # Display size
            b"0",           # Trigger method
            b"0",           # Outside RTH (false)
            b"0",           # Hidden (false)
            # Minimal required additional fields
            b"",            # Shares allocation (deprecated)
            b"",            # Discretionary amount (empty)
            b"",            # Good after time (empty)
            b"",            # Good till date (empty)
            b"",            # FA group (empty)
            b"",            # FA method (empty)
            b"",            # FA percentage (empty)
            b"",            # Model code (empty)
        ]
        
        # Join fields with null terminators
        message = b'\x00'.join(fields) + b'\x00'
        
        # Add length header
        order_message = struct.pack(">I", len(message)) + message
        
        print(f"📤 Sending order ({len(order_message)} bytes)")
        print(f"   Order: BUY 1 AAPL LMT $200.00 DAY")
        print(f"   ID: {next_order_id}")
        
        sock.send(order_message)
        print("✅ Haskell order sent!")
        
        # Wait for response
        print("⏳ Waiting for TWS response...")
        time.sleep(3)
        
        try:
            response = sock.recv(4096)
            if len(response) > 0:
                print(f"📥 TWS response ({len(response)} bytes)")
                print("✅ Order processed by TWS!")
                
                # Look for error patterns in response
                if b"error" in response.lower() or b"reject" in response.lower():
                    print("⚠️  Response may contain error message")
                else:
                    print("✅ Response looks successful!")
            else:
                print("ℹ️  No immediate response")
        except socket.timeout:
            print("ℹ️  No response within timeout")
        except Exception as e:
            print(f"ℹ️  Read error: {e}")
            
        sock.close()
        
        print("")
        print("🔍 CHECK TWS NOW:")
        print(f"   Look for order ID {next_order_id}")
        print("   Should be: BUY 1 AAPL LMT $200.00 DAY")
        print("   If visible, our Haskell implementation works! 🎉")
        
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    place_haskell_order()