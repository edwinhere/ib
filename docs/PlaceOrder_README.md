# PlaceOrder API Implementation

This document describes the complete implementation of the TWS API `placeOrder` functionality based on the reverse-engineered wire protocol documentation.

## Overview

The implementation provides a complete, type-safe interface for placing orders through the Interactive Brokers TWS API, including:

- ✅ **Complete Order data structure** with all TWS API fields
- ✅ **Wire protocol encoding** following exact TWS specification  
- ✅ **Helper functions** for common order types
- ✅ **Proper field ordering** as required by TWS
- ✅ **MAX_VALUE handling** (empty strings for infinite values)
- ✅ **Type safety** with Haskell's strong type system

## Quick Start

### Basic Market Order

```haskell
import IB.Protocol.Types
import IB.Client

-- Create a simple market order for 100 shares of AAPL
let contract = defaultContract "AAPL"
    order = simpleMarketOrder BUY 100.0
    request = placeOrder 1001 contract order
```

### Basic Limit Order

```haskell
-- Create a limit order for 50 shares of MSFT at $150.00
let contract = defaultContract "MSFT"
    order = simpleLimitOrder BUY 50.0 150.0
    request = placeOrder 1002 contract order
```

### Complex Order

```haskell
-- Create a more sophisticated order with custom settings
let contract = Contract
      { conId = Nothing
      , symbol = "GOOGL"
      , secType = STK
      , exchange = "NASDAQ"
      , primaryExchange = "NASDAQ"
      , currency = "USD"
      , lastTradeDateOrContractMonth = ""
      , strike = 0.0
      , right = Nothing
      , multiplier = ""
      , localSymbol = ""
      , tradingClass = ""
      , includeExpired = False
      , secIdType = ""
      , secId = ""
      , issuerId = ""
      }
    order = (defaultOrder SELL 25.0 LMT)
      { orderLmtPrice = Just 2800.0
      , orderTif = GTC                -- Good Till Canceled
      , orderAccount = "U123456"      -- Specific account
      , orderTransmit = False         -- Don't transmit immediately
      , orderOutsideRth = True        -- Allow outside regular hours
      }
    request = placeOrder 1003 contract order
```

## Data Types

### Order Actions
```haskell
data OrderAction = BUY | SELL
```

### Order Types
```haskell
data OrderType = 
    MKT           -- Market order
  | LMT           -- Limit order  
  | STP           -- Stop order
  | STP_LMT       -- Stop-limit order
  | TRAIL         -- Trailing stop
  -- ... and many more
```

### Time in Force
```haskell
data TimeInForce =
    DAY           -- Day order (default)
  | GTC           -- Good Till Canceled
  | IOC           -- Immediate or Cancel
  | GTD           -- Good Till Date
  | FOK           -- Fill or Kill
  -- ... etc
```

## Wire Protocol Details

The implementation follows the exact TWS API wire protocol:

### Message Structure
```
[4-byte length header][Message Type=3][Order ID][Contract Fields][Order Fields]
```

### Field Encoding
- All fields are null-terminated (end with `\0`)
- String encoding: UTF-8
- Number encoding: String representation 
- MAX_VALUE fields: Empty string + null terminator
- Boolean: "1" or "0" + null terminator

### Example Wire Format
For a market order: `BUY 100 AAPL MKT`
```
Length Header:  [0x00, 0x00, 0x00, 0x5A]     // 90 bytes
Message Type:   [0x33, 0x00]                 // "3" + null  
Order ID:       [0x31, 0x30, 0x30, 0x31, 0x00] // "1001" + null
Contract ID:    [0x30, 0x00]                 // "0" + null
Symbol:         [0x41, 0x41, 0x50, 0x4C, 0x00] // "AAPL" + null  
Security Type:  [0x53, 0x54, 0x4B, 0x00]     // "STK" + null
...
Action:         [0x42, 0x55, 0x59, 0x00]     // "BUY" + null
Quantity:       [0x31, 0x30, 0x30, 0x2E, 0x30, 0x00] // "100.0" + null
Order Type:     [0x4D, 0x4B, 0x54, 0x00]     // "MKT" + null
Limit Price:    [0x00]                       // Empty (MAX_VALUE)
...
```

## Integration with Existing Codebase

The placeOrder implementation integrates seamlessly with the existing project structure:

### Add to ClientMessage
```haskell
data ClientMessage = 
    -- ... existing messages
  | PlaceOrder PlaceOrderRequest
```

### Encoding Pipeline
```haskell
-- Send order through existing infrastructure
runConduit $ 
  yield (PlaceOrder orderRequest) 
    .| encodeMessages    -- Converts to ByteString
    .| frame            -- Adds length header  
    .| appSink app      -- Sends to TWS
```

## Testing

### Unit Tests
```bash
# Run PlaceOrder-specific tests
stack test --test-arguments "--match PlaceOrder"
```

### Wire Format Demo
```bash
# See actual wire protocol encoding
ghc -isrc examples/WireFormatDemo.hs -o wire_demo && ./wire_demo
```

### Integration Tests  
```bash
# Test with live TWS connection (requires TWS running)
stack test --test-arguments "--match Integration"
```

## Examples

The `examples/` directory contains:

1. **`PlaceOrderExample.hs`** - Basic order creation examples
2. **`WireFormatDemo.hs`** - Shows actual wire protocol encoding
3. **`LivePlaceOrderExample.hs`** - Live TWS connection example (requires dependencies)

## Implementation Files

### Core Implementation
- `src/IB/Protocol/Types.hs` - Order and Contract data types
- `src/IB/Codec/Encoder.hs` - PlaceOrder message encoding
- `src/IB/Client.hs` - High-level client functions

### Tests
- `test/PlaceOrderTest.hs` - Comprehensive unit tests
- `test/Spec.hs` - Test suite integration

### Documentation
- `docs/TWS_API_PlaceOrder_Documentation.md` - Complete wire protocol spec
- `docs/PlaceOrder_README.md` - This file

## Advanced Usage

### Custom Order Properties
```haskell
let order = (defaultOrder BUY 100.0 LMT)
      { orderLmtPrice = Just 99.50
      , orderTif = GTC
      , orderAllOrNone = True
      , orderHidden = True
      , orderOutsideRth = True
      , orderGoodAfterTime = "20241201 09:30:00"
      , orderGoodTillDate = "20241231"
      }
```

### Options Trading
```haskell
let optionContract = Contract
      { symbol = "AAPL"
      , secType = OPT
      , lastTradeDateOrContractMonth = "20250627"
      , strike = 200.0
      , right = Just Call
      , multiplier = "100"
      , exchange = "SMART"
      , currency = "USD"
      -- ... other fields
      }
```

### Financial Advisor Orders
```haskell
let faOrder = (defaultOrder BUY 1000.0 MKT)
      { orderFaGroup = "AllClients"
      , orderFaMethod = "EqualQuantity"
      }
```

## Error Handling

The TWS API will respond with error messages for invalid orders:

- **Error 201**: Order rejected - Invalid contract
- **Error 202**: Order rejected - Invalid order type
- **Error 103**: Duplicate order ID
- **Error 434**: Order size is too small
- **Error 201**: No security definition found

## Security Considerations

- ✅ **Input validation**: All order fields are type-checked
- ✅ **No injection attacks**: Binary protocol prevents injection
- ✅ **Account isolation**: Orders sent to authenticated account only
- ✅ **Transmission control**: `orderTransmit` flag controls immediate execution

## Compatibility

- **TWS API Version**: Compatible with TWS API v9.72+
- **Server Versions**: Supports all modern TWS server versions
- **Message Protocol**: V100+ (with length headers)
- **Field Support**: All documented order fields included

## Contributing

When extending the order functionality:

1. Follow the exact field order from the TWS documentation
2. Use proper MAX_VALUE encoding (empty strings) 
3. Add comprehensive tests for new order types
4. Update the wire format documentation

---

The implementation provides a complete, production-ready interface for placing orders through the Interactive Brokers TWS API with full protocol compliance and type safety.