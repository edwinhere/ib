# Interactive Brokers Haskell Client

A Haskell implementation of the Interactive Brokers TWS/Gateway API client with comprehensive testing.

## Features

This client implements the core Interactive Brokers API protocol, including:

### Core Functionality
- **Connection & Handshake**: Establishes TCP connection with proper protocol handshake
- **Message Framing**: Handles length-prefixed message encoding/decoding
- **Basic Requests**: Current time, contract details, historical data, market data

### Market Data Features
- **Market Data Streaming**: Real-time price and size updates
- **Market Depth (Order Book)**: Level 2 market depth data
- **Real-time Bars**: 5-second real-time bar data
- **Tick-by-Tick Data**: High-resolution tick data (Last, AllLast, BidAsk, MidPoint)

### Trading & Analysis Features
- **Option Calculations**: 
  - Calculate implied volatility from option price
  - Calculate option price from volatility
  - Greeks calculation (Delta, Gamma, Vega, Theta)
- **Account Information**: Account summaries, positions, P&L data

### Advanced Features
- **Contract Details**: Full contract specification and lookup
- **Historical Data**: Time-series data with various bar sizes
- **Managed Accounts**: Support for Financial Advisor accounts

## Usage

### Prerequisites
- Interactive Brokers TWS or IB Gateway running
- Paper trading account recommended for testing
- Haskell Stack build system

### Building and Running

```bash
# Build the project
stack build

# Run the client (connects to paper trading port 7497)
stack run
```

### Configuration

The client connects to:
- **Paper Trading**: `localhost:7497` (default)
- **Live Trading**: `localhost:7496`

Edit `src/Lib.hs` to change connection settings.

## Testing

The project includes comprehensive tests that convert the main application functionality to testable assertions:

### Running Tests

```bash
# Run unit tests only
stack test ib:test:ib-test

# Run integration tests only (requires IB Gateway/TWS running)
stack test ib:test:ib-integration-test

# Run all tests
stack test
```

### Test Strategy

#### Unit Tests (`test/Spec.hs`)
- **Codec Testing**: Message encoding/decoding validation
- **Framing Testing**: Network message framing
- **Protocol Testing**: Individual message type validation

#### Integration Tests (`test/IntegrationTest.hs`)
Integration tests convert the main application functionality to testable assertions:

- **Connection Testing**: Handshake and connection establishment
- **Contract Details**: Request and validate contract information
- **Historical Data**: Request and validate historical price data
- **Market Data**: Real-time market data subscription
- **Market Depth**: Order book data validation
- **Real-time Bars**: Time-based bar data
- **Tick-by-Tick Data**: High-frequency tick data
- **Account Summary**: Account information retrieval
- **Positions**: Current position data
- **Option Calculations**: Implied volatility and option pricing
- **Current Time**: Server time synchronization

### Test Best Practices

1. **Assertion-Based Testing**: All tests use proper assertions (`shouldBe`, `shouldSatisfy`)
2. **Timeout Handling**: Integration tests use timeouts to prevent hanging
3. **Error Handling**: Tests validate both success and error conditions
4. **Message Collection**: Tests collect and validate all response types
5. **Connection Management**: Proper setup and teardown of connections

### Test Configuration

```haskell
-- Test configuration
testHost :: ByteString
testHost = "127.0.0.1"

testPort :: Int
testPort = 7497

testTimeout :: Int
testTimeout = 30000000 -- 30 seconds
```

### Running Specific Tests

```bash
# Run specific test pattern
stack test --test-arguments="-m 'Connection'"

# Run with verbose output
stack test --test-arguments="--verbose"

# Run integration tests with specific focus
stack test ib:test:ib-integration-test --test-arguments="-m 'Market Data'"
```

## Protocol Implementation

This client implements the IB API protocol as documented in `protocol.md`, including:

- **Message Framing**: 4-byte big-endian length prefix
- **Field Encoding**: Null-terminated ASCII strings
- **Handshake Protocol**: Multi-step connection establishment
- **Message Types**: All major request/response message types

## Architecture

The codebase is organized into several modules:

- `IB.Protocol.Types`: Data types for contracts, messages, and requests
- `IB.Protocol.Constants`: Message ID constants
- `IB.Codec.Encoder`: Message encoding for client requests
- `IB.Codec.Decoder`: Message decoding for server responses
- `IB.Network.Connection`: TCP connection handling
- `IB.Network.Framing`: Message framing and unframing

## Example Usage

The main application demonstrates several features:

1. **Connection**: Establishes connection with proper handshake
2. **Contract Details**: Requests EUR/USD forex contract details
3. **Historical Data**: Retrieves 1-month daily bars
4. **Market Data**: Subscribes to real-time price updates
5. **Market Depth**: Requests order book data
6. **Real-time Bars**: Subscribes to 5-second bars
7. **Tick-by-Tick**: Requests high-resolution tick data
8. **Account Data**: Requests account summaries and positions
9. **Option Calculations**: Calculates implied volatility and option prices

## Development

### Adding New Features

To add new API features:

1. **Extend Types**: Add new data types in `IB.Protocol.Types`
2. **Add Constants**: Define message IDs in `IB.Protocol.Constants`
3. **Implement Encoder**: Add encoding logic in `IB.Codec.Encoder`
4. **Implement Decoder**: Add decoding logic in `IB.Codec.Decoder`
5. **Update Main**: Add example usage in `src/Lib.hs`
6. **Add Tests**: Include both unit and integration tests

### Adding New Tests

1. **Unit Tests**: Add to `test/Spec.hs` for isolated component testing
2. **Integration Tests**: Add to `test/IntegrationTest.hs` for end-to-end testing

### Test Structure

```haskell
describe "Feature Name" $ do
  it "should perform expected behavior" $ do
    -- Setup
    -- Action
    -- Assertion
    result `shouldBe` expectedValue
```

### Testing Against

The client can be tested against:
- IB Paper Trading Gateway
- TWS Paper Trading
- IB Gateway (paper or live)

## License

This project is provided as-is for educational and development purposes. Please ensure compliance with Interactive Brokers' API usage terms.
