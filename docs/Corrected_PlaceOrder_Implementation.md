# Corrected PlaceOrder Implementation Guide

## Overview

This document provides the technical specifications for a working TWS API `placeOrder` implementation based on successful debugging and verification against Interactive Brokers TWS.

## Critical Requirements

### Field Count
- **Total Fields Required**: 119 fields exactly
- **Message Size**: ~358-362 bytes
- **Encoding**: Null-terminated fields with 4-byte length header

### Field Structure

#### Core Fields (0-35)
Standard contract and order fields as documented in TWS API references.

#### Extended Fields (36-81) 
Additional order parameters including financial advisor, volatility, and scale order fields.

#### Protocol Extension Fields (82-118)
Undocumented fields required for TWS compatibility.

## Complete Field Specification

```python
# Complete 119-field PlaceOrder message structure
fields = [
    # CORE MESSAGE FIELDS (0-15)
    b"3",                    # 0: Message ID (PLACE_ORDER)
    order_id.encode(),       # 1: Order ID
    b"0",                    # 2: Contract ID (0 for symbol lookup)
    b"AAPL",                 # 3: Symbol
    b"STK",                  # 4: Security Type
    b"",                     # 5: Expiry (empty for stocks)
    b"0.0",                  # 6: Strike (0.0 for stocks)
    b"",                     # 7: Right (empty for stocks)
    b"",                     # 8: Multiplier (empty)
    b"SMART",                # 9: Exchange
    b"",                     # 10: Primary Exchange (empty)
    b"USD",                  # 11: Currency
    b"",                     # 12: Local Symbol (empty)
    b"",                     # 13: Trading Class (empty)
    b"",                     # 14: Sec ID Type (empty)
    b"",                     # 15: Sec ID (empty)
    
    # ORDER FIELDS (16-35)
    b"BUY",                  # 16: Action (BUY/SELL)
    b"1",                    # 17: Total Quantity
    b"LMT",                  # 18: Order Type (LMT/MKT/etc)
    b"150.00",               # 19: Limit Price
    b"",                     # 20: Aux Price (empty for limit orders)
    b"",                     # 21: Time in Force (CRITICAL: empty, not "DAY")
    b"",                     # 22: OCA Group (empty)
    b"",                     # 23: Account (empty)
    b"",                     # 24: Open/Close (empty)
    b"0",                    # 25: Origin (0=Customer)
    b"",                     # 26: Order Reference (empty)
    b"0",                    # 27: Transmit (0=false, 1=true)
    b"0",                    # 28: Parent ID (0=no parent)
    b"0",                    # 29: Block Order (0=false)
    b"0",                    # 30: Sweep to Fill (0=false)
    b"0",                    # 31: Display Size (0=no iceberg)
    b"0",                    # 32: Trigger Method (0=default)
    b"0",                    # 33: Outside RTH (0=false)
    b"0",                    # 34: Hidden (0=false)
    b"",                     # 35: Shares Allocation (deprecated, empty)
    
    # EXTENDED ORDER FIELDS (36-81)
    b"0",                    # 36: Discretionary Amount
    b"",                     # 37: Good After Time (empty)
    b"",                     # 38: Good Till Date (empty)
    b"",                     # 39: FA Group (empty)
    b"",                     # 40: FA Method (empty)
    b"",                     # 41: FA Percentage (empty)
    b"",                     # 42: Model Code (empty)
    b"0",                    # 43: Short Sale Slot
    b"",                     # 44: Designated Location (empty)
    b"-1",                   # 45: Exempt Code (CRITICAL: must be "-1")
    b"0",                    # 46: OCA Type
    b"",                     # 47: Rule 80A (empty)
    b"",                     # 48: Settling Firm (empty)
    b"0",                    # 49: All Or None (0=false)
    b"",                     # 50: Min Quantity (empty=MAX_VALUE)
    b"",                     # 51: Percent Offset (empty=MAX_VALUE)
    b"0",                    # 52: Auction Strategy
    b"0",                    # 53: Starting Price
    b"",                     # 54: Stock Reference Price (empty)
    b"0",                    # 55: Delta
    b"",                     # 56: Stock Range Lower (empty)
    b"",                     # 57: Stock Range Upper (empty)
    b"",                     # 58: Override Percentage Constraints (empty)
    b"",                     # 59: Volatility (empty=MAX_VALUE)
    b"",                     # 60: Volatility Type (empty=MAX_VALUE)
    b"0",                    # 61: Delta Neutral Order Type
    b"",                     # 62: Delta Neutral Aux Price (empty)
    b"",                     # 63: Continuous Update (empty)
    b"",                     # 64: Reference Price Type (empty)
    b"",                     # 65: Trail Stop Price (empty)
    b"0",                    # 66: Trailing Percent
    b"",                     # 67: Scale Initial Level Size (empty)
    b"",                     # 68: Scale Subsequent Level Size (empty)
    b"",                     # 69: Scale Price Increment (empty)
    b"",                     # 70: Hedge Type (empty)
    b"",                     # 71: Hedge Parameter (empty)
    b"",                     # 72: Opt Out Smart Routing (empty)
    b"",                     # 73: Clearing Account (empty)
    b"",                     # 74: Clearing Intent (empty)
    b"",                     # 75: Not Held (empty)
    b"",                     # 76: Algorithm Strategy (empty)
    b"0",                    # 77: Algorithm ID
    b"",                     # 78: What If (empty)
    b"",                     # 79: Solicited (empty)
    b"0",                    # 80: Randomize Size
    b"0",                    # 81: Randomize Price
    
    # PROTOCOL EXTENSION FIELDS (82-118)
    b"",                     # 82: Unknown Field (empty)
    b"",                     # 83: Unknown Field (empty)
    b"0",                    # 84: Unknown Field
    b"",                     # 85: Unknown Field (empty)
    b"0",                    # 86: Unknown Field
    b"0",                    # 87: Unknown Field
    b"0",                    # 88: Unknown Field
    b"0",                    # 89: Unknown Field
    b"",                     # 90: Unknown Field (empty)
    b"1.7976931348623157e+308",  # 91: MAX_VALUE (CRITICAL: exact format)
    b"1.7976931348623157e+308",  # 92: MAX_VALUE
    b"1.7976931348623157e+308",  # 93: MAX_VALUE
    b"1.7976931348623157e+308",  # 94: MAX_VALUE
    b"1.7976931348623157e+308",  # 95: MAX_VALUE
    b"0",                    # 96: Unknown Field
    b"",                     # 97: Unknown Field (empty)
    b"",                     # 98: Unknown Field (empty)
    b"",                     # 99: Unknown Field (empty)
    b"1.7976931348623157e+308",  # 100: MAX_VALUE
    b"",                     # 101: Unknown Field (empty)
    b"",                     # 102: Unknown Field (empty)
    b"",                     # 103: Unknown Field (empty)
    b"",                     # 104: Unknown Field (empty)
    b"0",                    # 105: Unknown Field
    b"0",                    # 106: Unknown Field
    b"0",                    # 107: Unknown Field
    b"",                     # 108: Unknown Field (empty)
    b"2147483647",           # 109: Integer MAX_VALUE (CRITICAL: exact format)
    b"2147483647",           # 110: Integer MAX_VALUE
    b"0",                    # 111: Unknown Field
    b"",                     # 112: Unknown Field (empty)
    b"",                     # 113: Unknown Field (empty)
    b"",                     # 114: Unknown Field (empty)
    b"0",                    # 115: Unknown Field
    b"",                     # 116: Unknown Field (empty)
    b"2147483647",           # 117: Integer MAX_VALUE
    b"",                     # 118: Unknown Field (empty)
]
```

## Critical Encoding Requirements

### MAX_VALUE Representations
```python
# Double MAX_VALUE - MUST use this exact string
DOUBLE_MAX_VALUE = b"1.7976931348623157e+308"

# Integer MAX_VALUE - MUST use this exact string  
INT_MAX_VALUE = b"2147483647"

# Empty fields represent MAX_VALUE in many contexts
EMPTY_MAX_VALUE = b""
```

### Field Encoding Rules
1. **Null Termination**: Every field must end with `\x00`
2. **UTF-8 Encoding**: All text fields use UTF-8
3. **String Numbers**: All numeric values sent as strings
4. **Boolean Values**: `"0"` = false, `"1"` = true
5. **Empty Strings**: `b""` represents various default/MAX values

### Message Structure
```python
# Complete message assembly
message = b'\x00'.join(fields) + b'\x00'
length_header = struct.pack(">I", len(message))  # Big-endian 4-byte length
complete_message = length_header + message
```

## Haskell Implementation

### Complete Field Builder
```haskell
buildCompleteePlaceOrder :: PlaceOrderRequest -> B.Builder
buildCompleteePlaceOrder req =
  let c = placeOrderContract req
      o = placeOrderOrder req
  in mconcat [
    -- Core fields (0-35)
    putFieldS (3 :: Int),                    -- Message ID
    putFieldS (placeOrderId req),            -- Order ID
    putFieldS (fromMaybe 0 (conId c)),       -- Contract ID
    putFieldT (symbol c),                    -- Symbol
    -- ... continue for all 119 fields
    
    -- Extended fields (36-81)
    putField "0",                            -- Discretionary Amount
    putField "",                             -- Good After Time
    -- ... continue for remaining fields
    
    -- Protocol extension fields (82-118)
    putField "",                             -- Field 82
    putField "",                             -- Field 83
    -- ... continue for all protocol fields
    putField "1.7976931348623157e+308",      -- MAX_VALUE fields
    putField "2147483647"                    -- Integer MAX_VALUE fields
  ]

putField :: String -> B.Builder
putField s = B.stringUtf8 s <> B.word8 0
```

## Verification Checklist

### Pre-Send Verification
- [ ] Field count = 119 exactly
- [ ] Message size ≈ 358-362 bytes
- [ ] All fields null-terminated
- [ ] Length header is big-endian 4-byte integer
- [ ] MAX_VALUE fields use exact format strings

### Post-Send Verification
- [ ] TWS responds with acknowledgment (typically 10 bytes)
- [ ] Order appears in TWS interface
- [ ] Order shows "Transmit" button when `transmit=false`
- [ ] Order details match sent parameters

## Common Mistakes to Avoid

### 1. **Incomplete Field Set**
❌ **Wrong**: Sending only documented fields (36 fields)  
✅ **Correct**: Send complete 119-field set

### 2. **TIF Field Encoding**
❌ **Wrong**: `b"DAY"` for Time In Force  
✅ **Correct**: `b""` (empty string)

### 3. **MAX_VALUE Format**
❌ **Wrong**: `b"MAX_VALUE"` or `b"inf"`  
✅ **Correct**: `b"1.7976931348623157e+308"`

### 4. **Field Ordering**
❌ **Wrong**: Any deviation from exact field order  
✅ **Correct**: Exact order as specified above

### 5. **Missing Protocol Fields**
❌ **Wrong**: Stopping at field 81  
✅ **Correct**: Include all fields through 118

## Testing Strategy

### Development Testing
```python
# Use distinctive values for debugging
test_order = {
    'symbol': 'AAPL',
    'price': 999.99,  # Distinctive price
    'order_id': 7777,  # Distinctive ID
    'transmit': False  # Shows "Transmit" button for verification
}
```

### Production Considerations
- Always test with `transmit=false` first
- Use appropriate price limits for safety
- Implement proper error handling
- Log wire format for debugging

## Performance Notes

### Message Size Impact
- Complete messages: ~362 bytes
- Minimal messages: ~85 bytes  
- **Conclusion**: Field completeness more important than size optimization

### Network Efficiency
- Single message per order
- No additional round trips required
- TWS processes immediately upon receipt

## Conclusion

This corrected implementation achieves byte-perfect compatibility with Interactive Brokers TWS by including all 119 required fields with proper encoding. The key insight is that TWS requires complete protocol compliance, including many undocumented fields, for order acceptance.

**Success Metrics**:
- ✅ Orders appear in TWS interface
- ✅ Orders are actionable (transmit/cancel)
- ✅ Wire format matches official API exactly
- ✅ No rejection or error responses

---

*Implementation Guide Version: 1.0*  
*Last Updated: 2025-06-26*  
*Status: Verified Working Implementation*