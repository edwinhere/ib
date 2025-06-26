#!/usr/bin/env python3

"""
Analyze the captured official API message to identify all missing fields
"""

def analyze_official_api_message():
    # The captured hex from official Python API
    official_hex = "000001623300310030004141504c0053544b0000302e30000000534d415254000055534400000000004255590031004c4d54003333332e3333000000000000300000300030003000300030003000300030000030000000000000003000002d3100300000003000000030003000003000000000000030000000000030000000000000000000000030000000300030000000300000300030003000300000312e37393736393331333438363233313537652b33303800312e37393736393331333438363233313537652b33303800312e37393736393331333438363233313537652b33303800312e37393736393331333438363233313537652b33303800312e37393736393331333438363233313537652b333038003000000000312e37393736393331333438363233313537652b3330380000000000300030003000003231343734383336343700323134373438333634370030000000003000003231343734383336343700"
    
    # Convert hex to bytes
    official_bytes = bytes.fromhex(official_hex)
    
    # Skip length header (first 4 bytes)
    payload = official_bytes[4:]
    
    # Split by null bytes
    fields = payload.split(b'\x00')
    
    print("=== OFFICIAL PYTHON API MESSAGE ANALYSIS ===")
    print(f"Total message size: {len(official_bytes)} bytes")
    print(f"Payload size: {len(payload)} bytes") 
    print(f"Number of fields: {len(fields)}")
    print()
    
    print("📊 ALL FIELDS FROM OFFICIAL API:")
    print("=" * 60)
    
    # Field descriptions based on TWS API documentation
    field_names = [
        "Message ID (3)",
        "Order ID", 
        "Contract ID",
        "Symbol",
        "Security Type",
        "Expiry",
        "Strike",
        "Right",
        "Multiplier", 
        "Exchange",
        "Primary Exchange",
        "Currency",
        "Local Symbol",
        "Trading Class",
        "Sec ID Type",
        "Sec ID",
        "Action",
        "Total Quantity",
        "Order Type",
        "Limit Price",
        "Aux Price",
        "TIF",
        "OCA Group",
        "Account",
        "Open/Close",
        "Origin",
        "Order Ref",
        "Transmit",
        "Parent ID",
        "Block Order",
        "Sweep to Fill",
        "Display Size",
        "Trigger Method",
        "Outside RTH",
        "Hidden",
        "Shares Allocation", # 35
        "Discretionary Amt",
        "Good After Time",
        "Good Till Date", 
        "FA Group",
        "FA Method", # 40
        "FA Percentage",
        "Model Code",
        "Short Sale Slot",
        "Designated Location",
        "Exempt Code", # 45
        "OCA Type",
        "Rule 80A",
        "Settling Firm",
        "All Or None",
        "Min Qty", # 50
        "Percent Offset",
        "Auction Strategy",
        "Starting Price",
        "Stock Ref Price",
        "Delta", # 55
        "Stock Range Lower",
        "Stock Range Upper",
        "Override Pct Constraints",
        "Volatility",
        "Volatility Type", # 60
        "Delta Neutral Order Type",
        "Delta Neutral Aux Price",
        "Continuous Update",
        "Reference Price Type",
        "Trail Stop Price", # 65
        "Trailing Percent",
        "Scale Init Level Size", 
        "Scale Subs Level Size",
        "Scale Price Increment",
        "Hedge Type", # 70
        "Hedge Param",
        "Opt Out Smart Routing",
        "Clearing Account",
        "Clearing Intent",
        "Not Held", # 75
        "Algo Strategy",
        "Algo ID",
        "What If",
        "Solicited",
        "Randomize Size", # 80
        "Randomize Price",
        # Beyond this point, we need to identify what the extra fields are
    ]
    
    for i, field in enumerate(fields):
        field_name = field_names[i] if i < len(field_names) else f"Unknown Field {i}"
        
        if field:
            try:
                ascii_repr = field.decode('utf-8', errors='replace')
                print(f"Field {i:3d}: {field_name:25} = '{ascii_repr}' ({field.hex()})")
            except:
                print(f"Field {i:3d}: {field_name:25} = <binary> ({field.hex()})")
        else:
            print(f"Field {i:3d}: {field_name:25} = <empty>")
        
        # Highlight the critical boundary
        if i == 35:
            print("-" * 60)
            print("⬆️ FIELDS WE INCLUDE (0-35)")
            print("⬇️ FIELDS WE'RE MISSING (36+)")
            print("-" * 60)
    
    print()
    print("🎯 KEY FINDINGS:")
    print(f"- Official API sends {len(fields)} fields")
    print(f"- Our implementation sends ~36 fields")  
    print(f"- We're missing {len(fields) - 36} fields!")
    print()
    print("📋 MISSING FIELDS ANALYSIS:")
    
    # Show the missing fields specifically
    missing_fields = fields[36:]
    print(f"Missing fields (36-{len(fields)-1}):")
    
    for i, field in enumerate(missing_fields, 36):
        field_name = field_names[i] if i < len(field_names) else f"Unknown Field {i}"
        if field:
            try:
                ascii_repr = field.decode('utf-8', errors='replace')
                print(f"  {i:3d}: {field_name:25} = '{ascii_repr}'")
            except:
                print(f"  {i:3d}: {field_name:25} = <binary>")
        else:
            print(f"  {i:3d}: {field_name:25} = <empty>")

def analyze_our_implementation():
    print("\n" + "=" * 60)
    print("OUR IMPLEMENTATION MESSAGE ANALYSIS")
    print("=" * 60)
    
    # Our captured hex
    our_hex = "000000513300320030004141504c0053544b0000302e30000000534d415254000055534400000000004255590031004c4d54003333332e333300004441590000000030000030003000300030003000300030003000"
    
    our_bytes = bytes.fromhex(our_hex)
    payload = our_bytes[4:]
    fields = payload.split(b'\x00')
    
    print(f"Our message size: {len(our_bytes)} bytes")
    print(f"Our field count: {len(fields)}")
    print()
    
    print("📊 OUR FIELDS:")
    field_names = [
        "Message ID (3)", "Order ID", "Contract ID", "Symbol", "Security Type",
        "Expiry", "Strike", "Right", "Multiplier", "Exchange", 
        "Primary Exchange", "Currency", "Local Symbol", "Trading Class", "Sec ID Type", 
        "Sec ID", "Action", "Total Quantity", "Order Type", "Limit Price",
        "Aux Price", "TIF", "OCA Group", "Account", "Open/Close",
        "Origin", "Order Ref", "Transmit", "Parent ID", "Block Order",
        "Sweep to Fill", "Display Size", "Trigger Method", "Outside RTH", "Hidden"
    ]
    
    for i, field in enumerate(fields):
        field_name = field_names[i] if i < len(field_names) else f"Field {i}"
        if field:
            ascii_repr = field.decode('utf-8', errors='replace')
            print(f"Field {i:2d}: {field_name:25} = '{ascii_repr}'")
        else:
            print(f"Field {i:2d}: {field_name:25} = <empty>")

if __name__ == "__main__":
    analyze_official_api_message()
    analyze_our_implementation()
    
    print("\n" + "=" * 60)
    print("CONCLUSION")
    print("=" * 60)
    print("🎯 ROOT CAUSE IDENTIFIED:")
    print("   Our implementation is missing ~83 fields that the official API includes!")
    print("   TWS requires ALL fields to be present for order acceptance.")
    print()
    print("📋 NEXT STEPS:")
    print("   1. Add all missing fields (36-119) to our Haskell implementation")
    print("   2. Use empty strings or default values for unused fields")
    print("   3. Ensure exact field ordering matches official API")
    print("   4. Test with complete field set")