#!/usr/bin/env python3

"""
Script to place an AAPL order using the official TWS Python API
and capture the wire format to verify our Haskell implementation.
"""

import sys
import os
import time
import socket
import struct

# Add the official TWS Python API to path
sys.path.append('/tmp/IBJts/source/pythonclient')

from ibapi.client import EClient
from ibapi.wrapper import EWrapper
from ibapi.contract import Contract
from ibapi.order import Order

class TestApp(EWrapper, EClient):
    def __init__(self):
        EClient.__init__(self, self)
        self.nextValidOrderId = None
        self.order_placed = False
        
    def nextValidId(self, orderId: int):
        print(f"✓ NextValidId: {orderId}")
        self.nextValidOrderId = orderId
        
        # Place AAPL order once we have valid ID
        self.place_aapl_order(orderId)
        
    def orderStatus(self, orderId, status, filled, remaining, avgFillPrice, 
                   permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice):
        print(f"Order Status - ID: {orderId}, Status: {status}")
        
    def openOrder(self, orderId, contract, order, orderState):
        print(f"Open Order - ID: {orderId}, Symbol: {contract.symbol}")
        
    def error(self, reqId, errorCode, errorString, advancedOrderRejectJson=""):
        print(f"Error {errorCode}: {errorString}")
        
    def place_aapl_order(self, order_id):
        print(f"Placing AAPL order with ID: {order_id}")
        
        # Create AAPL contract
        contract = Contract()
        contract.symbol = "AAPL"
        contract.secType = "STK"
        contract.exchange = "SMART"
        contract.currency = "USD"
        
        # Create market order
        order = Order()
        order.action = "BUY"
        order.totalQuantity = 1
        order.orderType = "MKT"
        order.transmit = True
        
        print("Order details:")
        print(f"  Symbol: {contract.symbol}")
        print(f"  Action: {order.action}")
        print(f"  Quantity: {order.totalQuantity}")
        print(f"  Type: {order.orderType}")
        print("")
        
        # Place the order
        self.placeOrder(order_id, contract, order)
        self.order_placed = True
        print("✓ Order placed!")

def capture_wire_traffic():
    """
    Alternative: Create a raw socket to capture traffic to TWS
    This would show the actual bytes being sent.
    """
    print("Note: To see the actual wire format, you could use:")
    print("sudo tcpdump -i lo -X port 7497")
    print("or wireshark to capture the TCP traffic.")
    print("")

def main():
    print("=== Official TWS API Order Test ===")
    print("This will place a REAL market order for 1 share of AAPL!")
    print("")
    
    response = input("Continue? (y/N): ")
    if response.lower() not in ['y', 'yes']:
        print("Cancelled.")
        return
        
    capture_wire_traffic()
    
    # Connect to TWS
    app = TestApp()
    
    try:
        print("Connecting to TWS...")
        app.connect("127.0.0.1", 7497, clientId=0)
        
        print("✓ Connected to TWS")
        print("Waiting for NextValidId...")
        
        # Run for a few seconds
        app.run()
        
    except Exception as e:
        print(f"Error: {e}")
        print("Make sure TWS is running with API enabled on port 7497")

if __name__ == "__main__":
    main()