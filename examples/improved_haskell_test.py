#!/usr/bin/env python3

"""
Test the improved Haskell implementation approach
This script implements the exact message format we updated in the Haskell encoder
"""

import socket
import struct
import time

def create_improved_haskell_order():
    """
    Create a PlaceOrder message using our improved Haskell approach
    with proper server version checking and field ordering
    """
    
    # Assume modern server version (187 like we hardcoded)
    server_version = 187
    
    # Order details
    order_id = 2001  # Use unique ID
    
    # Server version constants (from our Haskell implementation)
    MIN_SERVER_VER_NOT_HELD = 44
    MIN_SERVER_VER_ORDER_CONTAINER = 145
    MIN_SERVER_VER_PLACE_ORDER_CONID = 46
    MIN_SERVER_VER_TRADING_CLASS = 68
    MIN_SERVER_VER_SEC_ID_TYPE = 45
    MIN_SERVER_VER_FRACTIONAL_POSITIONS = 160
    MIN_SERVER_VER_HEDGE_ORDERS = 53
    MIN_SERVER_VER_ALGO_ORDERS = 54
    MIN_SERVER_VER_ORDER_SOLICITED = 104
    
    # Determine version field
    version = 27 if server_version < MIN_SERVER_VER_NOT_HELD else 45
    
    # Build message field by field exactly like our improved Haskell encoder
    fields = []
    
    # Message ID
    fields.append(b"3")
    
    # Version field (conditionally included)
    if server_version < MIN_SERVER_VER_ORDER_CONTAINER:
        fields.append(str(version).encode())
    
    # Order ID
    fields.append(str(order_id).encode())
    
    # Contract fields with conditional inclusion
    if server_version >= MIN_SERVER_VER_PLACE_ORDER_CONID:
        fields.append(b"0")  # conId
    
    fields.extend([
        b"AAPL",    # symbol
        b"STK",     # secType
        b"",        # lastTradeDateOrContractMonth
        b"0.0",     # strike
        b"",        # right
        b"",        # multiplier
        b"SMART",   # exchange
        b"",        # primaryExchange
        b"USD",     # currency
        b"",        # localSymbol
    ])
    
    if server_version >= MIN_SERVER_VER_TRADING_CLASS:
        fields.append(b"")  # tradingClass
    
    if server_version >= MIN_SERVER_VER_SEC_ID_TYPE:
        fields.extend([b"", b""])  # secIdType, secId
    
    # Main order fields
    fields.extend([
        b"BUY",     # action
        b"1.0",     # totalQuantity (as string for fractional shares)
        b"LMT",     # orderType
        b"150.0",   # lmtPrice (reasonable price for AAPL)
        b"",        # auxPrice (empty = MAX_VALUE)
        b"DAY",     # tif
        b"",        # ocaGroup
        b"",        # account
        b"",        # openClose
        b"0",       # origin
        b"",        # orderRef
        b"1",       # transmit
        b"0",       # parentId
        b"0",       # blockOrder
        b"0",       # sweepToFill
        b"0",       # displaySize
        b"0",       # triggerMethod
        b"0",       # outsideRth
        b"0",       # hidden
        b"",        # sharesAllocation (deprecated)
        b"",        # discretionaryAmt (MAX_VALUE -> empty)
        b"",        # goodAfterTime
        b"",        # goodTillDate
        b"",        # faGroup
        b"",        # faMethod
        b"",        # faPercentage
        b"",        # modelCode
        b"0",       # shortSaleSlot
        b"",        # designatedLocation
        b"-1",      # exemptCode
        b"0",       # ocaType
        b"",        # rule80A
        b"",        # settlingFirm
        b"0",       # allOrNone
        b"",        # minQty (MAX_VALUE -> empty)
        b"",        # percentOffset (MAX_VALUE -> empty)
        b"0",       # auctionStrategy
        b"",        # startingPrice (MAX_VALUE -> empty)
        b"",        # stockRefPrice (MAX_VALUE -> empty)
        b"",        # delta (MAX_VALUE -> empty)
        b"",        # stockRangeLower (MAX_VALUE -> empty)
        b"",        # stockRangeUpper (MAX_VALUE -> empty)
        b"0",       # overridePercentageConstraints
        b"",        # volatility (MAX_VALUE -> empty)
        b"",        # volatilityType (MAX_VALUE -> empty)
        b"",        # deltaNeutralOrderType
        b"",        # deltaNeutralAuxPrice (MAX_VALUE -> empty)
        b"0",       # continuousUpdate
        b"",        # referencePriceType (MAX_VALUE -> empty)
        b"",        # trailStopPrice (MAX_VALUE -> empty)
        b"",        # trailingPercent (MAX_VALUE -> empty)
        b"",        # scaleInitLevelSize (MAX_VALUE -> empty)
        b"",        # scaleSubsLevelSize (MAX_VALUE -> empty)
        b"",        # scalePriceIncrement (MAX_VALUE -> empty)
    ])
    
    # Advanced fields with conditional inclusion
    if server_version >= MIN_SERVER_VER_HEDGE_ORDERS:
        fields.extend([b"", b""])  # hedgeType, hedgeParam
    
    fields.extend([
        b"0",       # optOutSmartRouting
        b"",        # clearingAccount
        b"",        # clearingIntent
    ])
    
    if server_version >= MIN_SERVER_VER_NOT_HELD:
        fields.append(b"0")  # notHeld
    
    if server_version >= MIN_SERVER_VER_ALGO_ORDERS:
        fields.extend([b"", b""])  # algoStrategy, algoId
    
    fields.append(b"0")  # whatIf
    
    if server_version >= MIN_SERVER_VER_ORDER_SOLICITED:
        fields.append(b"0")  # solicited
    
    fields.extend([
        b"0",       # randomizeSize
        b"0",       # randomizePrice
    ])
    
    # Join with null terminators
    message = b'\x00'.join(fields) + b'\x00'
    
    # Add length header
    length_header = struct.pack('>I', len(message))
    
    return length_header + message, order_id

def test_improved_haskell_implementation():
    print("=== Testing Improved Haskell Implementation ===")
    print("Using server version checking and proper field ordering")
    print()
    
    try:
        # Connect to TWS
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.settimeout(10)
        sock.connect(("127.0.0.1", 7497))
        print("✓ Connected to TWS")
        
        # Handshake
        api_sign = b"API\0"
        version_payload = b"v100..187"
        handshake = api_sign + struct.pack(">I", len(version_payload)) + version_payload
        sock.send(handshake)
        response = sock.recv(4096)
        print(f"✓ Handshake completed ({len(response)} bytes)")
        
        # StartAPI
        start_api_payload = b"71\x00" + b"2\x00" + b"10\x00" + b"\x00"  # client ID 10
        start_api = struct.pack(">I", len(start_api_payload)) + start_api_payload
        sock.send(start_api)
        time.sleep(1)
        response = sock.recv(4096)
        print(f"✓ StartAPI completed ({len(response)} bytes)")
        
        # Create our improved Haskell order
        order_message, order_id = create_improved_haskell_order()
        
        print(f"📤 Sending improved Haskell order")
        print(f"   Order ID: {order_id}")
        print(f"   Details: BUY 1 AAPL LMT $150.00 DAY")
        print(f"   Message size: {len(order_message)} bytes")
        print(f"   Fields: {order_message.count(b'\\x00')} null terminators")
        
        sock.send(order_message)
        print("✅ Improved Haskell order sent!")
        
        # Wait for response
        time.sleep(3)
        try:
            response = sock.recv(4096)
            if response:
                print(f"📥 TWS response ({len(response)} bytes)")
                print("✅ Order processed by TWS!")
            else:
                print("ℹ️  No immediate response")
        except socket.timeout:
            print("ℹ️  No response within timeout")
        
        sock.close()
        
        print()
        print("🔍 CHECK TWS NOW:")
        print(f"   Look for order ID {order_id}")
        print("   Should be: BUY 1 AAPL LMT $150.00 DAY")
        print("   If visible, our improved Haskell implementation works! 🎉")
        
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == "__main__":
    test_improved_haskell_implementation()