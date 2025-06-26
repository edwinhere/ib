# TWS API PlaceOrder TCP Socket Implementation Guide

## Overview

This document provides complete documentation for implementing the Interactive Brokers TWS API `placeOrder` call using raw TCP sockets without the official libraries. All information is derived from analysis of the official TWS API source code.

## Table of Contents

1. [Wire Protocol Overview](#wire-protocol-overview)
2. [Data Types and Encoding](#data-types-and-encoding)
3. [Field Separation and Message Structure](#field-separation-and-message-structure)
4. [Complete Field Documentation](#complete-field-documentation)
5. [Wire Protocol Examples](#wire-protocol-examples)
6. [Implementation Notes](#implementation-notes)

## Wire Protocol Overview

The TWS API uses a text-based protocol over TCP sockets with the following characteristics:

- **Field Separation**: Each field is terminated with a null byte (0x00)
- **String Encoding**: UTF-8 encoding for all text
- **Number Encoding**: String representation (not binary)
- **Message Delimiting**: 4-byte length header (V100+ protocol)
- **Byte Order**: Big-endian for length headers

### Protocol Versions

- **V100+ (Modern)**: Messages prefixed with 4-byte length header
- **Pre-V100 (Legacy)**: No length header, connection-based parsing

## Data Types and Encoding

| Type | Wire Encoding | Example |
|------|---------------|---------|
| `String` | UTF-8 bytes + null terminator | `"AAPL"` → `[0x41,0x41,0x50,0x4C,0x00]` |
| `int` | String representation + null terminator | `123` → `[0x31,0x32,0x33,0x00]` |
| `double` | String representation + null terminator | `123.45` → `[0x31,0x32,0x33,0x2E,0x34,0x35,0x00]` |
| `boolean` | "1" or "0" + null terminator | `true` → `[0x31,0x00]` |
| `Double.MAX_VALUE` | Empty string + null terminator | `[0x00]` |
| `Integer.MAX_VALUE` | Empty string + null terminator | `[0x00]` |

### Special Value Handling

```java
// From Builder.java
public void sendMax(double a) throws EClientException {
    send(a == Double.MAX_VALUE ? "" : String.valueOf(a));
}

public void sendMax(int a) throws EClientException {
    send(a == Integer.MAX_VALUE ? "" : String.valueOf(a));
}
```

## Field Separation and Message Structure

### Field Separator
```java
private static final char SEP = 0;  // Null byte separator
```

### Message Format (V100+)
```
[4-byte length][field1]\0[field2]\0[field3]\0...[fieldN]\0
```

### Length Header Encoding
```java
// Big-endian 4-byte integer
static void intToBytes(int val, byte b[], int position) {
    b[position]   = (byte)(0xff & (val >> 24));
    b[position+1] = (byte)(0xff & (val >> 16));
    b[position+2] = (byte)(0xff & (val >> 8));
    b[position+3] = (byte)(0xff & val);
}
```

## Complete Field Documentation

### Message Header
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| msgType | int | Yes | 3 | All | PLACE_ORDER constant |
| version | int | Conditional | 45 | All | Protocol version (omitted if server >= MIN_SERVER_VER_ORDER_CONTAINER) |
| orderId | int | Yes | - | All | Unique order identifier |

### Contract Fields (sent in exact order)
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| conid | int | Conditional | 0 | MIN_SERVER_VER_PLACE_ORDER_CONID | Contract ID |
| symbol | String | Yes | "" | All | Stock symbol |
| secType | String | Yes | "" | All | Security type (STK, OPT, FUT, etc.) |
| lastTradeDateOrContractMonth | String | Yes | "" | All | Expiry date for derivatives |
| strike | double | Yes | 0.0 | All | Strike price for options |
| right | String | Yes | "" | All | Option right (C, P, or "") |
| multiplier | String | Conditional | "" | 15 | Contract multiplier |
| exchange | String | Yes | "" | All | Exchange |
| primaryExch | String | Conditional | "" | 14 | Primary exchange |
| currency | String | Yes | "" | All | Currency |
| localSymbol | String | Conditional | "" | 2 | Local symbol |
| tradingClass | String | Conditional | "" | MIN_SERVER_VER_TRADING_CLASS | Trading class |
| secIdType | String | Conditional | "" | MIN_SERVER_VER_SEC_ID_TYPE | Security ID type |
| secId | String | Conditional | "" | MIN_SERVER_VER_SEC_ID_TYPE | Security ID |

### Main Order Fields
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| action | String | Yes | "BUY" | All | BUY or SELL |
| totalQuantity | String/int | Yes | 0 | All | Order quantity (String if >= MIN_SERVER_VER_FRACTIONAL_POSITIONS) |
| orderType | String | Yes | "LMT" | All | Order type (LMT, MKT, STP, etc.) |
| lmtPrice | double | Yes | Double.MAX_VALUE | All | Limit price |
| auxPrice | double | Yes | Double.MAX_VALUE | All | Auxiliary price (stop price) |

### Extended Order Fields
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| tif | String | Yes | "DAY" | All | Time in force |
| ocaGroup | String | No | "" | All | OCA group name |
| account | String | No | "" | All | Account |
| openClose | String | No | "" | All | Open/Close for institutions |
| origin | int | Yes | 0 | All | Origin (0=Customer, 1=Firm) |
| orderRef | String | No | "" | All | Order reference |
| transmit | boolean | Yes | true | All | Transmit flag |
| parentId | int | Conditional | 0 | 4 | Parent order ID |
| blockOrder | boolean | Conditional | false | 5 | Block order flag |
| sweepToFill | boolean | Conditional | false | 5 | Sweep to fill flag |
| displaySize | int | Conditional | 0 | 5 | Display size |
| triggerMethod | int | Conditional | 0 | 5 | Trigger method |
| outsideRth | boolean | Conditional | false | 38 | Outside regular hours |
| hidden | boolean | Conditional | false | 7 | Hidden order flag |

### Combo Legs (for BAG orders)
If secType == "BAG" and server version >= 8:
| Field | Type | Min Version | Description |
|-------|------|-------------|-------------|
| comboLegsCount | int | 8 | Number of combo legs |

For each combo leg:
| Field | Type | Min Version | Description |
|-------|------|-------------|-------------|
| conid | int | 8 | Contract ID |
| ratio | int | 8 | Ratio |
| action | String | 8 | Action (BUY/SELL) |
| exchange | String | 8 | Exchange |
| openClose | String | 8 | Open/Close |
| shortSaleSlot | int | MIN_SERVER_VER_SSHORT_COMBO_LEGS | Short sale slot |
| designatedLocation | String | MIN_SERVER_VER_SSHORT_COMBO_LEGS | Designated location |
| exemptCode | int | MIN_SERVER_VER_SSHORTX_OLD | Exempt code |

### Additional Order Fields (continued in transmission order)
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| sharesAllocation | String | No | "" | 9 | Deprecated field (always "") |
| discretionaryAmt | double | No | Double.MAX_VALUE | 10 | Discretionary amount |
| goodAfterTime | String | No | "" | 11 | Good after time |
| goodTillDate | String | No | "" | 12 | Good till date |
| faGroup | String | No | "" | 13 | Financial advisor group |
| faMethod | String | No | "" | 13 | Financial advisor method |
| faPercentage | String | No | "" | 13 | Financial advisor percentage |
| modelCode | String | No | "" | MIN_SERVER_VER_MODELS_SUPPORT | Model code |
| shortSaleSlot | int | No | 0 | 18 | Short sale slot |
| designatedLocation | String | No | "" | 18 | Designated location |
| exemptCode | int | No | -1 | MIN_SERVER_VER_SSHORTX_OLD | Exempt code |
| ocaType | int | No | 0 | 19 | OCA type |
| rule80A | String | No | "" | 19 | Rule 80A |
| settlingFirm | String | No | "" | 19 | Settling firm |
| allOrNone | boolean | No | false | 19 | All or none |
| minQty | int | No | Integer.MAX_VALUE | 19 | Minimum quantity |
| percentOffset | double | No | Double.MAX_VALUE | 19 | Percent offset |
| auctionStrategy | int | No | 0 | 19 | Auction strategy |
| startingPrice | double | No | Double.MAX_VALUE | 19 | Starting price |
| stockRefPrice | double | No | Double.MAX_VALUE | 19 | Stock reference price |
| delta | double | No | Double.MAX_VALUE | 19 | Delta |
| stockRangeLower | double | No | Double.MAX_VALUE | 19 | Stock range lower |
| stockRangeUpper | double | No | Double.MAX_VALUE | 19 | Stock range upper |
| overridePercentageConstraints | boolean | No | false | 22 | Override percentage constraints |

### Volatility Order Fields
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| volatility | double | No | Double.MAX_VALUE | 26 | Volatility |
| volatilityType | int | No | Integer.MAX_VALUE | 26 | Volatility type |
| deltaNeutralOrderType | String/boolean | No | "" | 26/28 | Delta neutral order type |
| deltaNeutralAuxPrice | double | No | Double.MAX_VALUE | 28 | Delta neutral aux price |
| continuousUpdate | int | No | 0 | 26 | Continuous update |
| referencePriceType | int | No | Integer.MAX_VALUE | 26 | Reference price type |

### Scale Order Fields
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| trailStopPrice | double | No | Double.MAX_VALUE | 30 | Trail stop price |
| trailingPercent | double | No | Double.MAX_VALUE | MIN_SERVER_VER_TRAILING_PERCENT | Trailing percent |
| scaleInitLevelSize | int | No | Integer.MAX_VALUE | MIN_SERVER_VER_SCALE_ORDERS | Scale initial level size |
| scaleSubsLevelSize | int | No | Integer.MAX_VALUE | MIN_SERVER_VER_SCALE_ORDERS2 | Scale subsequent level size |
| scalePriceIncrement | double | No | Double.MAX_VALUE | MIN_SERVER_VER_SCALE_ORDERS | Scale price increment |

### Advanced Fields (Modern Versions)
| Field | Type | Mandatory | Default | Min Version | Description |
|-------|------|-----------|---------|-------------|-------------|
| hedgeType | String | No | "" | MIN_SERVER_VER_HEDGE_ORDERS | Hedge type |
| hedgeParam | String | No | "" | MIN_SERVER_VER_HEDGE_ORDERS | Hedge parameter |
| optOutSmartRouting | boolean | No | false | MIN_SERVER_VER_OPT_OUT_SMART_ROUTING | Opt out smart routing |
| clearingAccount | String | No | "" | MIN_SERVER_VER_PTA_ORDERS | Clearing account |
| clearingIntent | String | No | "" | MIN_SERVER_VER_PTA_ORDERS | Clearing intent |
| notHeld | boolean | No | false | MIN_SERVER_VER_NOT_HELD | Not held |
| algoStrategy | String | No | "" | MIN_SERVER_VER_ALGO_ORDERS | Algorithm strategy |
| algoId | String | No | "" | MIN_SERVER_VER_ALGO_ID | Algorithm ID |
| whatIf | boolean | No | false | MIN_SERVER_VER_WHAT_IF_ORDERS | What-if flag |
| solicited | boolean | No | false | MIN_SERVER_VER_ORDER_SOLICITED | Solicited flag |
| randomizeSize | boolean | No | false | MIN_SERVER_VER_RANDOMIZE_SIZE_AND_PRICE | Randomize size |
| randomizePrice | boolean | No | false | MIN_SERVER_VER_RANDOMIZE_SIZE_AND_PRICE | Randomize price |

## Wire Protocol Examples

### Example 1: Simple Market Order

**Order Details:**
- Order ID: 1001
- Symbol: AAPL
- Action: BUY
- Quantity: 100
- Order Type: MKT

**Wire Representation (V100+ format):**
```
Length Header:  [0x00, 0x00, 0x00, 0x20]        // 32 bytes message length
Message Type:   [0x33, 0x00]                     // "3" + null
Order ID:       [0x31, 0x30, 0x30, 0x31, 0x00]  // "1001" + null
Symbol:         [0x41, 0x41, 0x50, 0x4C, 0x00]  // "AAPL" + null
Sec Type:       [0x53, 0x54, 0x4B, 0x00]        // "STK" + null
Expiry:         [0x00]                           // "" + null (empty)
Strike:         [0x30, 0x2E, 0x30, 0x00]        // "0.0" + null
Right:          [0x00]                           // "" + null (empty)
Exchange:       [0x53, 0x4D, 0x41, 0x52, 0x54, 0x00]  // "SMART" + null
Currency:       [0x55, 0x53, 0x44, 0x00]        // "USD" + null
Action:         [0x42, 0x55, 0x59, 0x00]        // "BUY" + null
Quantity:       [0x31, 0x30, 0x30, 0x00]        // "100" + null
Order Type:     [0x4D, 0x4B, 0x54, 0x00]        // "MKT" + null
Limit Price:    [0x00]                           // Double.MAX_VALUE → empty
Aux Price:      [0x00]                           // Double.MAX_VALUE → empty
TIF:            [0x44, 0x41, 0x59, 0x00]        // "DAY" + null
... (remaining fields) ...
```

### Example 2: Multiple Messages

**Three consecutive placeOrder messages:**
```
Message 1: [0x00,0x00,0x00,0x2A][3\0 1001\0 AAPL\0 STK\0 ... ]
Message 2: [0x00,0x00,0x00,0x1F][3\0 1002\0 MSFT\0 STK\0 ... ]
Message 3: [0x00,0x00,0x00,0x33][3\0 1003\0 GOOGL\0 STK\0 ...]
```

### Example 3: Field Value Encodings

| Value | Type | Wire Bytes | Hex Representation |
|-------|------|------------|-------------------|
| `Double.MAX_VALUE` | double | `[0x00]` | Empty string + null |
| `123.45` | double | `[0x31,0x32,0x33,0x2E,0x34,0x35,0x00]` | "123.45" + null |
| `true` | boolean | `[0x31,0x00]` | "1" + null |
| `false` | boolean | `[0x30,0x00]` | "0" + null |
| `""` | String | `[0x00]` | Empty string + null |
| `"AAPL"` | String | `[0x41,0x41,0x50,0x4C,0x00]` | "AAPL" + null |

## Implementation Notes

### Critical Requirements

1. **Field Order**: Fields must be sent in the exact order specified in the documentation
2. **Version Checking**: Always check server version before sending version-dependent fields
3. **Null Termination**: Every field must end with a null byte (0x00)
4. **String Encoding**: Use UTF-8 encoding for all string data
5. **Length Header**: For V100+ protocol, prepend 4-byte big-endian length header

### Default Values

| Type | Default Value | Wire Representation |
|------|---------------|-------------------|
| String fields | `""` | `[0x00]` |
| Integer fields | `0` or `Integer.MAX_VALUE` | `[0x30,0x00]` or `[0x00]` |
| Double fields | `Double.MAX_VALUE` | `[0x00]` |
| Boolean fields | `false` | `[0x30,0x00]` |

### Common Defaults for Order Fields

```
action = "BUY"
orderType = "LMT"
tif = "DAY"
transmit = true
origin = 0 (Customer)
lmtPrice = Double.MAX_VALUE
auxPrice = Double.MAX_VALUE
```

### Error Handling

- **Malformed Messages**: Server will reject messages with incorrect field order or format
- **Version Incompatibility**: Sending unsupported fields to older servers causes errors
- **Invalid Data**: Non-ASCII printable characters in strings will be rejected

### Example Implementation (Python)

```python
import struct
import socket

def encode_field(value):
    """Encode a field value for TWS API transmission"""
    if value is None:
        return b'\x00'
    elif isinstance(value, bool):
        return b'1\x00' if value else b'0\x00'
    elif isinstance(value, (int, float)):
        if (isinstance(value, float) and value == float('inf')) or \
           (isinstance(value, int) and value == 2147483647):  # MAX_VALUE
            return b'\x00'
        return str(value).encode('utf-8') + b'\x00'
    else:
        return str(value).encode('utf-8') + b'\x00'

def build_place_order_message(order_id, symbol, action, quantity, order_type):
    """Build a simple placeOrder message"""
    fields = [
        3,           # PLACE_ORDER message type
        order_id,    # Order ID
        symbol,      # Symbol
        "STK",       # Security type
        "",          # Expiry
        0.0,         # Strike
        "",          # Right
        "SMART",     # Exchange
        "USD",       # Currency
        action,      # BUY/SELL
        quantity,    # Quantity
        order_type,  # Order type
        float('inf'), # Limit price (MAX_VALUE)
        float('inf'), # Aux price (MAX_VALUE)
        "DAY",       # Time in force
        # ... additional fields as needed
    ]
    
    # Encode all fields
    message_data = b''.join(encode_field(field) for field in fields)
    
    # Add length header for V100+ protocol
    length = len(message_data)
    length_header = struct.pack('>I', length)  # Big-endian 4-byte length
    
    return length_header + message_data

# Usage
message = build_place_order_message(1001, "AAPL", "BUY", 100, "MKT")
# Send message over TCP socket
```

### Security Considerations

- **Input Validation**: Always validate field values before encoding
- **Character Filtering**: Ensure strings contain only ASCII printable characters
- **Length Limits**: Be aware of practical limits on field lengths
- **Connection Security**: Use appropriate network security (TLS/SSL) as required

## Server Version Constants

Common server version constants referenced in the documentation:

```
MIN_SERVER_VER_PLACE_ORDER_CONID = 61
MIN_SERVER_VER_TRADING_CLASS = 73
MIN_SERVER_VER_SEC_ID_TYPE = 31
MIN_SERVER_VER_FRACTIONAL_POSITIONS = 160
MIN_SERVER_VER_SCALE_ORDERS = 20
MIN_SERVER_VER_HEDGE_ORDERS = 53
MIN_SERVER_VER_ALGO_ORDERS = 54
MIN_SERVER_VER_NOT_HELD = 44
MIN_SERVER_VER_ORDER_SOLICITED = 104
MIN_SERVER_VER_MODELS_SUPPORT = 121
```

For a complete list of version constants, refer to the TWS API source code.

## References

This documentation is based on analysis of the Interactive Brokers TWS API Java source code, specifically:

- `com.ib.client.EClient.java` - Main client implementation
- `com.ib.client.Builder.java` - Message building and encoding
- `com.ib.client.Order.java` - Order field definitions
- `com.ib.client.Contract.java` - Contract field definitions
- `com.ib.client.ESocket.java` - TCP socket transport

---

*Generated from TWS API version 10.30.01 source code analysis*