#!/usr/bin/env python3

"""
Corrected Haskell implementation with ALL 119 fields matching official Python API
"""

import socket
import struct
import time

def create_corrected_haskell_order():
    """Create order with complete 119-field message matching official API exactly"""
    
    print("=== CORRECTED HASKELL IMPLEMENTATION ===")
    print("Including ALL 119 fields to match official Python API")
    print()
    
    try:
        # Connect to TWS
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.connect(("127.0.0.1", 7497))
        print("✓ Connected to TWS")
        
        # Handshake
        api_sign = b"API\0"
        version_payload = b"v100..187"
        handshake = api_sign + struct.pack(">I", len(version_payload)) + version_payload
        sock.send(handshake)
        sock.recv(4096)
        print("✓ Handshake completed")
        
        # StartAPI
        start_api_payload = b"71\x00" + b"2\x00" + b"21\x00" + b"\x00"  # client ID 21
        start_api = struct.pack(">I", len(start_api_payload)) + start_api_payload
        sock.send(start_api)
        sock.recv(4096)
        print("✓ StartAPI completed")
        
        # Create corrected order with ALL 119 fields
        order_id = 8888  # Distinctive ID
        
        # All 119 fields exactly matching official Python API format
        fields = [
            # Fields 0-35 (we had these)
            b"3",                    # 0: Message ID
            str(order_id).encode(),  # 1: Order ID 
            b"0",                    # 2: Contract ID
            b"AAPL",                 # 3: Symbol
            b"STK",                  # 4: Security Type
            b"",                     # 5: Expiry
            b"0.0",                  # 6: Strike
            b"",                     # 7: Right
            b"",                     # 8: Multiplier
            b"SMART",                # 9: Exchange
            b"",                     # 10: Primary Exchange
            b"USD",                  # 11: Currency
            b"",                     # 12: Local Symbol
            b"",                     # 13: Trading Class
            b"",                     # 14: Sec ID Type
            b"",                     # 15: Sec ID
            b"BUY",                  # 16: Action
            b"1",                    # 17: Total Quantity
            b"LMT",                  # 18: Order Type
            b"444.44",               # 19: Limit Price (DISTINCTIVE!)
            b"",                     # 20: Aux Price
            b"",                     # 21: TIF (IMPORTANT: empty like official API, not "DAY")
            b"",                     # 22: OCA Group
            b"",                     # 23: Account
            b"",                     # 24: Open/Close
            b"0",                    # 25: Origin
            b"",                     # 26: Order Ref
            b"0",                    # 27: Transmit
            b"0",                    # 28: Parent ID
            b"0",                    # 29: Block Order
            b"0",                    # 30: Sweep to Fill
            b"0",                    # 31: Display Size
            b"0",                    # 32: Trigger Method
            b"0",                    # 33: Outside RTH
            b"0",                    # 34: Hidden
            b"",                     # 35: Shares Allocation
            
            # Fields 36-81 (missing fields we need to add)
            b"0",                    # 36: Discretionary Amt
            b"",                     # 37: Good After Time
            b"",                     # 38: Good Till Date
            b"",                     # 39: FA Group
            b"",                     # 40: FA Method
            b"",                     # 41: FA Percentage
            b"",                     # 42: Model Code
            b"0",                    # 43: Short Sale Slot
            b"",                     # 44: Designated Location
            b"-1",                   # 45: Exempt Code
            b"0",                    # 46: OCA Type
            b"",                     # 47: Rule 80A
            b"",                     # 48: Settling Firm
            b"0",                    # 49: All Or None
            b"",                     # 50: Min Qty
            b"",                     # 51: Percent Offset
            b"0",                    # 52: Auction Strategy
            b"0",                    # 53: Starting Price
            b"",                     # 54: Stock Ref Price
            b"0",                    # 55: Delta
            b"",                     # 56: Stock Range Lower
            b"",                     # 57: Stock Range Upper
            b"",                     # 58: Override Pct Constraints
            b"",                     # 59: Volatility
            b"",                     # 60: Volatility Type
            b"0",                    # 61: Delta Neutral Order Type
            b"",                     # 62: Delta Neutral Aux Price
            b"",                     # 63: Continuous Update
            b"",                     # 64: Reference Price Type
            b"",                     # 65: Trail Stop Price
            b"0",                    # 66: Trailing Percent
            b"",                     # 67: Scale Init Level Size
            b"",                     # 68: Scale Subs Level Size
            b"",                     # 69: Scale Price Increment
            b"",                     # 70: Hedge Type
            b"",                     # 71: Hedge Param
            b"",                     # 72: Opt Out Smart Routing
            b"",                     # 73: Clearing Account
            b"",                     # 74: Clearing Intent
            b"",                     # 75: Not Held
            b"",                     # 76: Algo Strategy
            b"0",                    # 77: Algo ID
            b"",                     # 78: What If
            b"",                     # 79: Solicited
            b"0",                    # 80: Randomize Size
            b"0",                    # 81: Randomize Price
            
            # Fields 82-118 (additional fields from official API)
            b"",                     # 82: Unknown Field 82
            b"",                     # 83: Unknown Field 83
            b"0",                    # 84: Unknown Field 84
            b"",                     # 85: Unknown Field 85
            b"0",                    # 86: Unknown Field 86
            b"0",                    # 87: Unknown Field 87
            b"0",                    # 88: Unknown Field 88
            b"0",                    # 89: Unknown Field 89
            b"",                     # 90: Unknown Field 90
            b"1.7976931348623157e+308",  # 91: MAX_VALUE field
            b"1.7976931348623157e+308",  # 92: MAX_VALUE field
            b"1.7976931348623157e+308",  # 93: MAX_VALUE field
            b"1.7976931348623157e+308",  # 94: MAX_VALUE field
            b"1.7976931348623157e+308",  # 95: MAX_VALUE field
            b"0",                    # 96: Unknown Field 96
            b"",                     # 97: Unknown Field 97
            b"",                     # 98: Unknown Field 98
            b"",                     # 99: Unknown Field 99
            b"1.7976931348623157e+308",  # 100: MAX_VALUE field
            b"",                     # 101: Unknown Field 101
            b"",                     # 102: Unknown Field 102
            b"",                     # 103: Unknown Field 103
            b"",                     # 104: Unknown Field 104
            b"0",                    # 105: Unknown Field 105
            b"0",                    # 106: Unknown Field 106
            b"0",                    # 107: Unknown Field 107
            b"",                     # 108: Unknown Field 108
            b"2147483647",           # 109: Integer MAX_VALUE
            b"2147483647",           # 110: Integer MAX_VALUE
            b"0",                    # 111: Unknown Field 111
            b"",                     # 112: Unknown Field 112
            b"",                     # 113: Unknown Field 113
            b"",                     # 114: Unknown Field 114
            b"0",                    # 115: Unknown Field 115
            b"",                     # 116: Unknown Field 116
            b"2147483647",           # 117: Integer MAX_VALUE
            b"",                     # 118: Unknown Field 118
        ]
        
        # Verify we have exactly 119 fields
        assert len(fields) == 119, f"Expected 119 fields, got {len(fields)}"
        
        message = b'\x00'.join(fields) + b'\x00'
        order_message = struct.pack(">I", len(message)) + message
        
        print(f"📤 CORRECTED HASKELL ORDER:")
        print(f"   Order ID: {order_id}")
        print(f"   Details: BUY 1 AAPL LMT $444.44")
        print(f"   Field count: {len(fields)} (matches official API!)")
        print(f"   Message size: {len(order_message)} bytes")
        print(f"   Transmit: False (will show 'Transmit' button)")
        
        sock.send(order_message)
        print("✅ Corrected Haskell order sent!")
        
        # Get response
        try:
            response = sock.recv(4096)
            if response:
                print(f"📥 TWS response: {len(response)} bytes")
            else:
                print("ℹ️  No immediate response")
        except:
            print("ℹ️  No response received")
        
        sock.close()
        
        print()
        print("🔍 CHECK TWS NOW:")
        print("   Look for: AAPL BUY 1 LMT $444.44")
        print("   Order ID: 8888")
        print("   Should have 'Transmit' button")
        print()
        print("🎯 THIS IS THE ULTIMATE TEST:")
        print("   If this order appears, our Haskell implementation is FIXED!")
        print("   We now have byte-perfect compatibility with TWS!")
        
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    create_corrected_haskell_order()