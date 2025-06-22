# Client Implementation Themes

This section extracts common patterns and themes from the Python implementation examples. It provides a high-level overview of how a client is structured and interacts with the TWS/Gateway API, complementing the detailed protocol specification below.

### 1. The Core Components

A typical client implementation consists of several key components:

*   **Connection & Handshake Logic**: A dedicated module or function to establish the TCP socket connection and perform the mandatory multi-step handshake.
*   **Message Framing**: A pair of functions (`send_message`, `read_message`) responsible for the protocol's length-prefixing. `send_message` prepends the payload size, while `read_message` reads the size first to determine how many bytes to receive for the full message.
*   **Request Encoders**: Functions that construct the specific payload for each API request (e.g., `reqCurrentTime`, `reqContractDetails`). These functions assemble the required fields in the correct order.
*   **Response Decoders/Handlers**: Logic that processes incoming messages. This usually involves a main loop that reads messages, checks the incoming message ID, and dispatches the message to the appropriate handler (e.g., a message with ID `49` is handled by the `CURRENT_TIME` processor).
*   **Main Application Loop**: An event loop that listens for incoming socket data, calls the decoders, and manages the overall state of the client.

### 2. The Handshake: A Special Case

The initial connection is a special sequence that must be followed precisely.

1.  **Client Greeting**: Immediately upon connection, the client sends a non-standard message: the raw bytes `API\0` followed by a length-prefixed version string (e.g., `v100..187`).
2.  **Server Greeting**: The server replies with a single, standard length-prefixed message containing two null-terminated fields: the server version and the connection time.
3.  **Start API**: The client sends its first standard message, `START_API` (ID 71), to finalize the connection. No other API requests will be accepted before this message is sent.

### 3. Standard Message Structure

After the handshake, all subsequent messages follow a consistent structure:

*   **Direction**: Client <-> Server
*   **Framing**: `[4-byte Length Prefix (Big-Endian)] [Payload]`
*   **Payload**: A series of fields separated by the ASCII null character (`\0`).
*   **Field Encoding**: All fields, including numbers like message IDs, request IDs, and version numbers, are converted to their ASCII string representation before being joined with null terminators.

**Example: `reqCurrentTime`**
The client wants to send the fields `[49, 1]`. This is encoded as:
1.  Payload String: `"49\01\0"`
2.  Payload Bytes: `b'49\x001\x00'`
3.  Length Prefix: `struct.pack('!I', len(payload_bytes))`
4.  Final Message on Wire: `length_prefix + payload_bytes`

### 4. The Event Loop and Asynchronous Nature

The API is asynchronous. The client sends a request and then must listen for one or more response messages.

*   A central event loop continuously checks the socket for incoming data.
*   Using a non-blocking socket with a timeout is essential. This allows the client to check for data without getting stuck, handle timeouts if the server doesn't respond, and perform other tasks.
*   The server can send messages at any time, not just in direct response to a request. These include:
    *   **Error/Status Messages (ID 4)**: Provide information about connection status or report errors. These are often identifiable by a Request ID of `-1`.
    *   **Data Subscriptions**: For requests like `reqMktData` or `reqRealTimeBars`, the server sends a continuous stream of updates until the subscription is cancelled.
    *   **Initial State**: Upon successful connection, the server sends unsolicited messages like `NEXT_VALID_ID` (ID 9) and `MANAGED_ACCTS` (ID 15).

A robust client must be able to handle these unsolicited and streamed messages by inspecting the message ID of every incoming message and acting accordingly.

# Interactive Brokers TCP Socket API Protocol

This document reverse-engineers the TCP socket protocol used by Interactive Brokers for their API. The goal is to provide a clear and concise reference for implementing a client in any programming language.

## Message Framing

All messages sent between the client and the server are length-prefixed. The payload of the message is preceded by a 4-byte, big-endian (network byte order) unsigned integer that specifies the length of the payload.

Fields within a message payload are typically separated by a null terminator character (`\0`).

## Connection and Handshake

The connection process involves a handshake to establish the protocol version and client identity.

### 1. TCP Connection

The client initiates a standard TCP connection to the TWS or IB Gateway server. The default ports are:
-   `7496` for TWS (live account)
-   `7497` for TWS (paper account)
-   `4001` for IB Gateway (live account)
-   `4002` for IB Gateway (paper account)

### 2. Client Version

Immediately after the TCP connection is established, the client must send its API version information.

-   **Direction**: Client -> Server
-   **Format**: This message is sent in two parts.
    1.  The ASCII string `API\0`.
    2.  A length-prefixed version string.
        -   The version string is `v<min_version>..<max_version>`. Based on the Python client, this is `v100..187`.
        -   This version string is then prepended with its length (4-byte big-endian integer).

So, the full message on the wire looks like:
`'API\0' + struct.pack('!I', len(version_string)) + version_string.encode('ascii')`

### 3. Server Version and Connection Time

The server responds with its version and the time the connection was established.

-   **Direction**: Server -> Client
-   **Format**: A single length-prefixed message. The payload contains two null-terminated strings (fields).
    1.  **Server Version**: An integer representing the server's version.
    2.  **Connection Time**: A string with the date and time of the connection (e.g., `20230510 12:00:00 GMT`).

### 4. Start API

The client sends a final message to complete the handshake and start the API session.

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing several null-terminated fields.
    1.  **Message ID**: `71` (for `START_API`).
    2.  **Version**: The client API version to use. The Python client uses `2`.
    3.  **Client ID**: An integer that uniquely identifies this client connection.
    4.  **Optional Capabilities**: An optional string for advertising extra capabilities. Can be an empty string if not used.

## API Requests

Once the handshake is complete, the client can send requests to the server.

### Request Current Server Time

-   **Request Message ID**: `49` (`REQ_CURRENT_TIME`)
-   **Response Message ID**: `49` (`CURRENT_TIME`)

#### Client Request

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing two null-terminated fields.
    1.  **Message ID**: `49`
    2.  **Version**: `1`

#### Server Response

-   **Direction**: Server -> Client
-   **Format**: A length-prefixed message with a payload containing two null-terminated fields.
    1.  **Message ID**: `49`
    2.  **Time**: The current server time as a Unix timestamp (integer).

### Request Next Valid Order ID

-   **Request Message ID**: `8` (`REQ_IDS`)
-   **Response Message ID**: `9` (`NEXT_VALID_ID`)

This is used to get a valid ID from the server that can be used when placing an order.

#### Client Request (reqIds)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing three null-terminated fields.
    1.  **Message ID**: `8`
    2.  **Version**: `1`
    3.  **numIds**: `1` (Integer, deprecated but required by the client implementation).

#### Server Response (nextValidId)

-   **Direction**: Server -> Client
-   **Format**: A length-prefixed message with a payload containing three null-terminated fields.
    1.  **Message ID**: `9`
    2.  **Version**: `1` (This field is present but ignored by the official Python client decoder).
    3.  **Order ID**: The next valid order ID (integer).

**Note**: This message is also sent unsolicited by the server upon successful connection to provide the initial valid order ID.

### Request Contract Details

-   **Request Message ID**: `9` (`REQ_CONTRACT_DATA`)
-   **Response Message ID**: `10` (`CONTRACT_DATA`)
-   **End Message ID**: `52` (`CONTRACT_DATA_END`)

#### Client Request (reqContractDetails)

This request asks for all details for a given contract. The client sends a simplified `Contract` object to identify the instrument.

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The payload consists of the following null-terminated fields in order:
    1.  **Message ID**: `9`
    2.  **Version**: `8`
    3.  **Request ID**: A unique integer to identify this request.
    4.  **conId**: The unique contract identifier (0 if not known).
    5.  **symbol**: The ticker symbol (e.g., "EUR").
    6.  **secType**: The security type ("CASH", "STK", "FUT", "OPT", etc.).
    7.  **lastTradeDateOrContractMonth**: For derivatives. Empty for CASH.
    8.  **strike**: The option strike price (0.0 for non-options).
    9.  **right**: The option right ("C" or "P"). Empty for non-options.
    10. **multiplier**: The contract multiplier. Empty for CASH.
    11. **exchange**: The destination exchange (e.g., "IDEALPRO").
    12. **primaryExchange**: The primary exchange.
    13. **currency**: The contract's currency (e.g., "USD").
    14. **localSymbol**: The local trading symbol.
    15. **tradingClass**: The trading class.
    16. **includeExpired**: `0` or `1`.
    17. **secIdType**: CUSIP, SEDOL, etc.
    18. **secId**: The identifier value.
    19. **issuerId**: The bond issuer ID.

#### Server Response

The server may send one or more `CONTRACT_DATA` messages in response. For a single valid contract, it will send one `CONTRACT_DATA` and one `BOND_CONTRACT_DATA` (if applicable), followed by a `CONTRACT_DATA_END`.

-   **Direction**: Server -> Client
-   **Format (`CONTRACT_DATA`)**: A length-prefixed message with a large number of null-terminated fields representing the full `ContractDetails`. The order of fields is complex and depends on the server version. Refer to the `processContractDataMsg` function in the official client's `decoder.py` for the exact structure.
-   **Format (`CONTRACT_DATA_END`)**: A length-prefixed message indicating the end of the data stream for this request.
    1.  **Message ID**: `52`
    2.  **Version**: `1`
    3.  **Request ID**: The ID from the original request.

### Request Market Data

-   **Request Message ID**: `1` (`REQ_MKT_DATA`)
-   **Cancel Message ID**: `2` (`CANCEL_MKT_DATA`)
-   **Response Message IDs**: `1` (`TICK_PRICE`), `2` (`TICK_SIZE`), `45` (`TICK_GENERIC`), `46` (`TICK_STRING`), etc.

#### Client Request (reqMktData)

This request asks for a stream of market data for a contract.

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The payload consists of a large number of null-terminated fields, starting with the same contract definition as `reqContractDetails` and followed by parameters specific to the market data request.
    1.  **Message ID**: `1`
    2.  **Version**: `11`
    3.  **Request ID**: A unique integer for this request.
    4.  **Contract Fields**: The same 16 fields used in `reqContractDetails` (from `conId` to `issuerId`).
    5.  **genericTickList**: A comma-separated list of generic tick types to subscribe to (e.g., "233" for RTVolume). Can be empty.
    6.  **snapshot**: `0` for streaming data, `1` for a single snapshot.
    7.  **regulatorySnapshot**: `0` or `1`.
    8.  **mktDataOptions**: A list of tag-value pairs (for internal use, typically empty).

#### Server Response (TICK_PRICE)

The server responds with a stream of messages. The most common is `TICK_PRICE`.

-   **Direction**: Server -> Client
-   **Format (`TICK_PRICE`)**: A length-prefixed message.
    1.  **Message ID**: `1`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.
    4.  **Tick Type**: An integer indicating the type of price (e.g., `1` for bid, `2` for ask, `4` for last).
    5.  **Price**: The price value.
    6.  **Size**: The size corresponding to the price.
    7.  **Attributes**: A bitmask with tick attributes (e.g., `canAutoExecute`).

### Request Market Depth

-   **Request Message ID**: `10` (`REQ_MKT_DEPTH`)
-   **Cancel Message ID**: `11` (`CANCEL_MKT_DEPTH`)
-   **Response Message IDs**: `12` (`UPDATE_MKT_DEPTH`), `13` (`UPDATE_MKT_DEPTH_L2`)

This subscription requests the order book for a contract.

#### Client Request (reqMktDepth)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `10`
2.  **Version**: `5`
3.  **Request ID**: A unique integer for this request.
4.  **conId**: The unique contract identifier.
5.  **symbol**: The ticker symbol.
6.  **secType**: The security type.
7.  **lastTradeDateOrContractMonth**: For derivatives.
8.  **strike**: The option strike price.
9.  **right**: The option right ("C" or "P").
10. **multiplier**: The contract multiplier.
11. **exchange**: The destination exchange (must be a direct-routed exchange, not "SMART").
12. **primaryExchange**: The primary listing exchange.
13. **currency**: The contract's currency.
14. **localSymbol**: The local trading symbol.
15. **tradingClass**: The trading class.
16. **numRows**: The number of market depth rows to return.
17. **isSmartDepth**: `0` or `1`. Specifies if the request is for SMART depth.
18. **mktDepthOptions**: A list of tag-value pairs (for internal use, typically empty).

#### Server Response (updateMktDepth)

The server sends a stream of `UPDATE_MKT_DEPTH` messages to build and maintain the order book.

-   **Direction**: Server -> Client
-   **Format (`UPDATE_MKT_DEPTH`)**: A length-prefixed message.
    1.  **Message ID**: `12`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.
    4.  **Position**: The row of the order book to be updated (0-based).
    5.  **Operation**: `0` for insert, `1` for update, `2` for delete.
    6.  **Side**: `0` for ask, `1` for bid.
    7.  **Price**: The price for the level.
    8.  **Size**: The size for the level.

### Place Order

-   **Request Message ID**: `3` (`PLACE_ORDER`)
-   **Response**: Order status updates are sent via messages like `OPEN_ORDER` and `ORDER_STATUS`.

#### Client Request (placeOrder)

This is one of the most complex messages, with a large number of fields that depend on the server version, contract type, and order type. The following is a simplified structure for placing a simple "Market" order for a stock. Many fields for advanced orders (Algo, Scale, Hedge, etc.) are omitted for brevity.

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The payload consists of the following null-terminated fields in order. The exact version number sent can vary, but modern clients send a large number of fields.

1.  **Message ID**: `3`
2.  **Order ID**: A unique integer identifying the order.
3.  **conId**: The unique contract identifier.
4.  **symbol**: The ticker symbol.
5.  **secType**: The security type (e.g., "STK").
6.  **lastTradeDateOrContractMonth**: Empty for stocks.
7.  **strike**: `0.0` for stocks.
8.  **right**: Empty for stocks.
9.  **multiplier**: Empty for stocks.
10. **exchange**: The destination exchange (e.g., "SMART").
11. **primaryExchange**: The primary listing exchange (e.g., "ISLAND").
12. **currency**: The contract's currency (e.g., "USD").
13. **localSymbol**: The local trading symbol.
14. **tradingClass**: The trading class.
15. **secIdType**: Empty.
16. **secId**: Empty.
17. **action**: "BUY" or "SELL".
18. **totalQuantity**: The number of shares.
19. **orderType**: The order type (e.g., "MKT", "LMT").
20. **lmtPrice**: The limit price for LMT orders, `0` for MKT orders.
21. **auxPrice**: The stop price for STP or TRAIL orders, `0` for MKT orders.
22. **tif**: The time in force ("DAY", "GTC", "IOC", etc.).
23. **account**: The trading account number.
24. **transmit**: `1` to transmit the order immediately, `0` to save it.

... and many more optional/conditional fields follow.

### Request Executions

-   **Request Message ID**: `7` (`REQ_EXECUTIONS`)
-   **Response Message ID**: `11` (`EXECUTION_DATA`)
-   **End Message ID**: `55` (`EXECUTION_DATA_END`)

This request retrieves execution reports that match a given filter.

#### Client Request (reqExecutions)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The payload consists of the following null-terminated fields.

1.  **Message ID**: `7`
2.  **Version**: `3`
3.  **Request ID**: A unique integer for this request.
4.  **Client ID**: Filter by client ID.
5.  **Account Code**: Filter by account number.
6.  **Time**: Filter by time (format `yyyymmdd-hh:mm:ss`).
7.  **Symbol**: Filter by symbol.
8.  **Security Type**: Filter by security type.
9.  **Exchange**: Filter by exchange.
10. **Side**: Filter by side ("BUY" or "SELL").

#### Server Response (execDetails)

The server sends a stream of `EXECUTION_DATA` messages, one for each matching execution, followed by an `EXECUTION_DATA_END` message.

-   **Direction**: Server -> Client
-   **Format (`EXECUTION_DATA`)**: A length-prefixed message containing two parts: the contract details and the execution details.
    1.  **Message ID**: `11`
    2.  **Version**: The message version.
    3.  **Request ID**: The ID from the original request.
    4.  **Order ID**: The parent order's ID.
    5.  **...Contract Fields...**: A block of fields describing the contract (`conId`, `symbol`, `secType`, `exchange`, `currency`, etc.).
    6.  **...Execution Fields...**: A block of fields describing the execution (`execId`, `time`, `acctNumber`, `exchange`, `side`, `shares`, `price`, `permId`, etc.).

#### Server Response (execDetailsEnd)

-   **Direction**: Server -> Client
-   **Format (`EXECUTION_DATA_END`)**: A length-prefixed message indicating the end of the data stream.
    1.  **Message ID**: `55`
    2.  **Version**: (unused)
    3.  **Request ID**: The ID from the original request.

### Request Managed Accounts

-   **Request Message ID**: `17` (`REQ_MANAGED_ACCTS`)
-   **Response Message ID**: `15` (`MANAGED_ACCTS`)

This is a client-initiated request for the list of Financial Advisor (FA) managed accounts.

#### Client Request (reqManagedAccts)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message.
    1.  **Message ID**: `17`
    2.  **Version**: `1`

#### Server Response (managedAccounts)

The server replies with a `MANAGED_ACCTS` message, which is the same message sent unsolicited upon connection to an FA account.

-   **Direction**: Server -> Client
-   **Format**: See the `Managed Accounts List` section under "Common Server-side Messages" for the full format.
-   **Context**: This message is sent unsolicited upon connection to a Financial Advisor (FA) account, and is also sent as a direct response to a client's `reqManagedAccts` (ID 17) request.
-   **Note on Non-FA Accounts**: Our testing shows that if the connected user is not a Financial Advisor, the server **does not** send an error message in response to this request. It simply does not respond, and the client must rely on a timeout to handle this case.

### Request Market Scanner

-   **Request Message ID**: `22` (`REQ_SCANNER_SUBSCRIPTION`)
-   **Cancel Message ID**: `23` (`CANCEL_SCANNER_SUBSCRIPTION`)
-   **Response Message ID**: `20` (`SCANNER_DATA`)

This allows the client to subscribe to a market scanner that provides a list of contracts based on specified criteria (e.g., top gainers, most active).

#### Client Request (reqScannerSubscription)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The payload consists of the following null-terminated fields.

1.  **Message ID**: `22`
2.  **Version**: `4` (Note: This field is only sent if the server version is less than 143 (`MIN_SERVER_VER_SCANNER_GENERIC_OPTS`). For modern servers, this field is omitted).
3.  **Request ID**: A unique integer for this request.
4.  **numberOfRows**: The number of rows to return.
5.  **instrument**: The instrument type (e.g., "STK").
6.  **locationCode**: The location code (e.g., "STK.US.MAJOR").
7.  **scanCode**: The type of scan (e.g., "TOP_PERC_GAIN").
8.  **abovePrice**: Filter by price.
9.  **belowPrice**: Filter by price.
10. **aboveVolume**: Filter by volume.
11. **marketCapAbove**: Filter by market cap.
12. **marketCapBelow**: Filter by market cap.
13. **moodyRatingAbove**: Filter by Moody's rating.
14. **moodyRatingBelow**: Filter by Moody's rating.
15. **spRatingAbove**: Filter by S&P rating.
16. **spRatingBelow**: Filter by S&P rating.
17. **maturityDateAbove**: Filter by maturity date.
18. **maturityDateBelow**: Filter by maturity date.
19. **couponRateAbove**: Filter by coupon rate.
20. **couponRateBelow**: Filter by coupon rate.
21. **excludeConvertible**: `0` or `1`.
22. **averageOptionVolumeAbove**: Filter by option volume.
23. **scannerSettingPairs**: For advanced settings.
24. **stockTypeFilter**: "ALL", "CORP", "ADR", etc.

#### Server Response (SCANNER_DATA)

The server responds with a stream of `SCANNER_DATA` messages, one for each matching contract, followed by a `SCANNER_DATA_END` message.

-   **Direction**: Server -> Client
-   **Format (`SCANNER_DATA`)**: A length-prefixed message.
    1.  **Message ID**: `20`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.
    4.  **Rank**: The rank of this result.
    5.  ... a large number of fields describing the contract ...
    6.  **Distance**: How far the contract is from the scanner's benchmark.
    7.  **Benchmark**: The benchmark the contract is compared against.
    8.  **Projection**: A projection of future performance.
    9.  **LegsStr**: Describes combo legs if any.

### Request Historical Data

-   **Request Message ID**: `20` (`REQ_HISTORICAL_DATA`)
-   **Cancel Message ID**: `25` (`CANCEL_HISTORICAL_DATA`)
-   **Response Message ID**: `17` (`HISTORICAL_DATA`)

This request retrieves historical data for a contract.

#### Client Request (reqHistoricalData)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `20`
2.  **Version**: `6` (Note: This field is only sent if the server version is less than 101 (`MIN_SERVER_VER_SYNT_REALTIME_BARS`). For modern servers, this field is omitted).
3.  **Request ID**: A unique integer for this request.
4.  **Contract Fields**: The standard 16 fields describing the contract (from `conId` to `issuerId`).
5.  **Include Expired**: `0` or `1`.
6.  **End Date/Time**: The last date and time for the data request (format: `yyyymmdd HH:mm:ss ttt`).
7.  **Bar Size Setting**: The granularity of the data (e.g., "1 day", "5 mins").
8.  **Duration String**: The length of the period to retrieve (e.g., "1 Y", "10 D").
9.  **Use RTH**: `0` for all data, `1` for regular trading hours only.
10. **What To Show**: The type of data (e.g., "TRADES", "MIDPOINT", "BID", "ASK"). Note: For spot Forex ("CASH"), only "MIDPOINT", "BID", or "ASK" are available, not "TRADES".
11. **Format Date**: `1` for `yyyymmdd...` string, `2` for Unix timestamp.
12. **Keep Up To Date**: `0` for a static snapshot, `1` to receive live updates for the head of the series.
13. **Chart Options**: A list of tag-value pairs (for internal use, typically empty).


#### Server Response (HISTORICAL_DATA)

The server sends a stream of messages containing the historical bars. The stream starts with a header row and is followed by one message per bar. It concludes with a `HISTORICAL_DATA_END` message.

-   **Direction**: Server -> Client
-   **Format (`HISTORICAL_DATA`)**: A length-prefixed message containing fields for a single bar.
    1.  **Message ID**: `17`
    2.  **Request ID**: The ID from the original request.
    3.  **Date**: The bar's date/time.
    4.  **Open**: The opening price.
    5.  **High**: The high price.
    6.  **Low**: The low price.
    7.  **Close**: The closing price.
    8.  **Volume**: The volume for the bar.
    9.  **WAP**: The weighted average price.
    10. **Bar Count**: The number of trades in the bar.

### Request Head Timestamp

-   **Request Message ID**: `87` (`REQ_HEAD_TIMESTAMP`)
-   **Response Message ID**: `88` (`HEAD_TIMESTAMP`)

This request retrieves the timestamp of the earliest available historical data for a given contract and data type. This is useful for determining the backfill range before making a large `reqHistoricalData` request.

#### Client Request (reqHeadTimeStamp)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `87`
2.  **Request ID**: A unique integer for this request.
3.  **Contract Fields**: The standard 13 fields describing the contract (from `conId` to `includeExpired`).
    - `conId`, `symbol`, `secType`, `lastTradeDateOrContractMonth`, `strike`, `right`, `multiplier`, `exchange`, `primaryExchange`, `currency`, `localSymbol`, `tradingClass`, `includeExpired`.
4.  **Use RTH**: `1` for regular trading hours only, `0` for all data.
5.  **What To Show**: The type of data (e.g., "TRADES", "MIDPOINT", "BID", "ASK"). Note: For spot Forex ("CASH"), only "MIDPOINT", "BID", or "ASK" are available, not "TRADES".
6.  **Format Date**: `1` for a `yyyymmdd hh:mm:ss` string, `2` for a Unix timestamp.

#### Server Response (headTimestamp)

The server sends a single `HEAD_TIMESTAMP` message in response.

-   **Direction**: Server -> Client
-   **Format (`HEAD_TIMESTAMP`)**: A length-prefixed message with the following fields:
    1.  **Message ID**: `88`
    2.  **Request ID**: The ID from the original request.
    3.  **Head Timestamp**: A string containing the earliest available data point's timestamp in the requested format.

### Calculate Implied Volatility

-   **Request Message ID**: `54` (`REQ_CALC_IMPLIED_VOLAT`)
-   **Cancel Message ID**: `56` (`CANCEL_CALC_IMPLIED_VOLAT`)
-   **Response Message ID**: `21` (`TICK_OPTION_COMPUTATION`)

This request calculates the implied volatility of an option and its Greeks based on a provided option price and underlying price. It is the reverse of `calculateOptionPrice`.

#### Client Request (calculateImpliedVolatility)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `54`
2.  **Version**: `3`
3.  **Request ID**: A unique integer for this request.
4.  **conId**: The unique contract identifier. **Note**: Should be an empty string (`""`) if looking up the contract by its other attributes.
5.  **symbol**: The ticker symbol.
6.  **secType**: The security type (must be "OPT" or a combo type).
7.  **lastTradeDateOrContractMonth**: The option's expiration date.
8.  **strike**: The option strike price.
9.  **right**: The option right ("C" or "P").
10. **multiplier**: The contract multiplier.
11. **exchange**: The destination exchange.
12. **primaryExchange**: The primary listing exchange.
13. **currency**: The contract's currency.
14. **localSymbol**: The local trading symbol.
15. **tradingClass**: The trading class.
16. **optionPrice**: The price of the option contract.
17. **underlyingPrice**: The price of the underlying asset.
18. **implVolOptions**: A list of tag-value pairs (for internal use, typically empty).

#### Server Response (tickOptionComputation)

The server sends a `TICK_OPTION_COMPUTATION` message in response. The format is identical to the response for `calculateOptionPrice`. See that section for details. The primary field of interest is `Implied Volatility`.

### Calculate Option Price

-   **Request Message ID**: `55` (`REQ_CALC_OPTION_PRICE`)
-   **Cancel Message ID**: `57` (`CANCEL_CALC_OPTION_PRICE`)
-   **Response Message ID**: `21` (`TICK_OPTION_COMPUTATION`)

This request calculates the price of an option and its Greeks based on a provided volatility and underlying price.

#### Client Request (calculateOptionPrice)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `55`
2.  **Version**: `4`
3.  **Request ID**: A unique integer for this request.
4.  **conId**: The unique contract identifier. **Note**: Should be an empty string (`""`) if looking up the contract by its other attributes (e.g., symbol, secType).
5.  **symbol**: The ticker symbol.
6.  **secType**: The security type (must be "OPT" or a combo type).
7.  **lastTradeDateOrContractMonth**: The option's expiration date.
8.  **strike**: The option strike price.
9.  **right**: The option right ("C" or "P").
10. **multiplier**: The contract multiplier.
11. **exchange**: The destination exchange.
12. **primaryExchange**: The primary listing exchange.
13. **currency**: The contract's currency.
14. **localSymbol**: The local trading symbol.
15. **tradingClass**: The trading class.
16. **volatility**: The volatility to use for the calculation (e.g., `0.20` for 20%).
17. **underlyingPrice**: The price of the underlying asset to use for the calculation.
18. **customerAccount**: Optional account for institutional customers. Typically empty.
19. **professionalCustomer**: Optional flag for professional customers. Typically `0` (False).
20. **optPrcOptions**: A list of tag-value pairs (for internal use, typically empty).

#### Server Response (tickOptionComputation)

The server sends a `TICK_OPTION_COMPUTATION` message in response.

-   **Direction**: Server -> Client
-   **Format (`TICK_OPTION_COMPUTATION`)**: A length-prefixed message.
    1.  **Message ID**: `21`
    2.  **Version**: The message version (unused by modern clients).
    3.  **Request ID**: The ID from the original request.
    4.  **Tick Type**: An integer indicating the data type (e.g., `13` for `MODEL_OPTION`).
    5.  **Tick Attributes**: A bitmask for price-based volatility attributes.
    6.  **Implied Volatility**: The implied volatility of the option. `-1.0` if not available.
    7.  **Delta**: The option's delta. `-2.0` if not available.
    8.  **Option Price**: The calculated price of the option. `-1.0` if not available.
    9.  **PV Dividend**: The present value of dividends expected on the option's underlying. `-1.0` if not available.
    10. **Gamma**: The option's gamma. `-2.0` if not available.
    11. **Vega**: The option's vega. `-2.0` if not available.
    12. **Theta**: The option's theta. `-2.0` if not available.
    13. **Underlying Price**: The underlying price used in the calculation. `-1.0` if not available.

### Request Real-Time Bars

-   **Request Message ID**: `50` (`REQ_REAL_TIME_BARS`)
-   **Cancel Message ID**: `51` (`CANCEL_REAL_TIME_BARS`)
-   **Response Message ID**: `50` (`REAL_TIME_BARS`)

This is a subscription that requests a stream of 5-second real-time bars for a contract.

#### Client Request (reqRealTimeBars)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `50`
2.  **Version**: `3`
3.  **Request ID**: A unique integer for this request.
4.  **Contract ID**: The unique contract identifier (`conId`).
5.  **Symbol**: The ticker symbol (can be empty if `conId` is specified).
6.  **Security Type**: The security type (e.g., "STK").
7.  **Last Trade Date/Contract Month**: For derivatives.
8.  **Strike**: The option strike price.
9.  **Right**: The option right ("C" or "P").
10. **Multiplier**: The contract multiplier.
11. **Exchange**: The destination exchange.
12. **Primary Exchange**: The primary listing exchange.
13. **Currency**: The contract's currency.
14. **Local Symbol**: The local trading symbol.
15. **Trading Class**: The trading class.
16. **Bar Size**: The bar size. Currently, only `5` (seconds) is supported.
17. **What To Show**: The type of data (e.g., "TRADES", "MIDPOINT", "BID", "ASK").
18. **Use RTH**: `1` for regular trading hours only, `0` for all data.
19. **Real-Time Bars Options**: A list of tag-value pairs (for internal use, typically empty).

#### Client Request (cancelRealTimeBars)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message to cancel the subscription.
    1.  **Message ID**: `51`
    2.  **Version**: `1`
    3.  **Request ID**: The ID of the subscription to cancel.

#### Server Response (REAL_TIME_BARS)

The server sends a continuous stream of `REAL_TIME_BARS` messages, typically one every 5 seconds.

-   **Direction**: Server -> Client
-   **Format (`REAL_TIME_BARS`)**: A length-prefixed message containing fields for a single bar.
    1.  **Message ID**: `50`
    2.  **Version**: The version of the message format (e.g., `1`).
    3.  **Request ID**: The ID from the original request.
    4.  **Time**: The bar's timestamp (Unix epoch time).
    5.  **Open**: The opening price for the bar.
    6.  **High**: The high price for the bar.
    7.  **Low**: The low price for the bar.
    8.  **Close**: The closing price for the bar.
    9.  **Volume**: The volume for the bar.
    10. **WAP**: The weighted average price for the bar.
    11. **Count**: The number of trades in the bar.

### Request Tick-by-Tick Data

-   **Request Message ID**: `97` (`REQ_TICK_BY_TICK_DATA`)
-   **Cancel Message ID**: `98` (`CANCEL_TICK_BY_TICK_DATA`)
-   **Response Message ID**: `99` (`TICK_BY_TICK`)

This is a subscription that requests a stream of real-time tick-by-tick data. This provides a higher resolution of data than real-time bars or aggregated market data.

#### Client Request (reqTickByTickData)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The `Version` field is not sent. The payload consists of the following null-terminated fields.

1.  **Message ID**: `97`
2.  **Request ID**: A unique integer for this request.
3.  **conId**: The unique contract identifier.
4.  **symbol**: The ticker symbol.
5.  **secType**: The security type.
6.  **lastTradeDateOrContractMonth**: For derivatives.
7.  **strike**: The option strike price.
8.  **right**: The option right ("C" or "P").
9.  **multiplier**: The contract multiplier.
10. **exchange**: The destination exchange.
11. **primaryExchange**: The primary listing exchange.
12. **currency**: The contract's currency.
13. **localSymbol**: The local trading symbol.
14. **tradingClass**: The trading class.
15. **tickType**: A string specifying the type of ticks to receive. Can be `"Last"`, `"AllLast"`, `"BidAsk"`, or `"MidPoint"`.
16. **numberOfTicks**: The number of ticks to return. `0` will stream ticks indefinitely. If a non-zero value is used, the stream will stop after that many ticks have been received. (Requires server version >= 140).
17. **ignoreSize**: A boolean (`1` or `0`) indicating whether to ignore ticks with a size of zero. (Requires server version >= 140).

#### Server Response (TICK_BY_TICK)

The server sends a stream of `TICK_BY_TICK` messages. The format of the message depends on the `tickType` requested.

-   **Direction**: Server -> Client
-   **Common Format**: All `TICK_BY_TICK` messages start with the same header.
    1.  **Message ID**: `99`
    2.  **Request ID**: The ID from the original request.
    3.  **Tick Type**: An integer corresponding to the requested `tickType` string (`1` for Last, `2` for AllLast, `3` for BidAsk, `4` for MidPoint).
    4.  **Time**: The time of the tick as a Unix timestamp.

-   **Format for `tickType` = "Last" or "AllLast" (Integer `1` or `2`)**
    5.  **Price**: The last trade price.
    6.  **Size**: The last trade size.
    7.  **TickAttribLast**: A bitmask with trade attributes. `1` for `pastLimit`, `2` for `unreported`.
    8.  **Exchange**: The exchange where the trade occurred.
    9.  **Special Conditions**: Any special conditions on the trade.

-   **Format for `tickType` = "BidAsk" (Integer `3`)**
    5.  **Bid Price**: The current bid price.
    6.  **Ask Price**: The current ask price.
    7.  **Bid Size**: The size of the current bid.
    8.  **Ask Size**: The size of the current ask.
    9.  **TickAttribBidAsk**: A bitmask with attributes. `1` for `bidPastLow`, `2` for `askPastHigh`.

-   **Format for `tickType` = "MidPoint" (Integer `4`)**
    5.  **MidPoint**: The midpoint price between the bid and ask.

### Request Security Definition Option Parameters

-   **Request Message ID**: `78` (`REQ_SEC_DEF_OPT_PARAMS`)
-   **Response Message ID**: `78` (`SECURITY_DEFINITION_OPTION_PARAMETER`)
-   **End Message ID**: `79` (`SECURITY_DEFINITION_OPTION_PARAMETER_END`)

This request retrieves the option chain for a given underlying security. It provides the available exchanges, trading classes, multipliers, expirations, and strikes.

#### Client Request (reqSecDefOptParams)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `78`
2.  **Request ID**: A unique integer for this request.
3.  **Underlying Symbol**: The symbol of the underlying security (e.g., "AAPL").
4.  **Exchange**: The exchange on which the options are trading. Can be an empty string `""` to receive results for all exchanges.
5.  **Security Type**: The type of the underlying security (e.g., "STK").
6.  **Underlying conId**: The unique contract ID of the underlying security.

#### Server Response (securityDefinitionOptionParameter)

The server sends a stream of `SECURITY_DEFINITION_OPTION_PARAMETER` messages, one for each exchange that has options for the underlying. The stream is concluded with a `SECURITY_DEFINITION_OPTION_PARAMETER_END` message.

-   **Direction**: Server -> Client
-   **Format (`SECURITY_DEFINITION_OPTION_PARAMETER`)**: A length-prefixed message containing the parameters for one exchange. The payload is complex and contains a variable number of expirations and strikes.
    1.  **Message ID**: `78`
    2.  **Request ID**: The ID from the original request.
    3.  **Exchange**: The exchange these parameters apply to.
    4.  **Underlying conId**: The conId of the underlying security for which these parameters are valid.
    5.  **Trading Class**: The option trading class.
    6.  **Multiplier**: The contract multiplier.
    7.  **Expirations Count**: An integer specifying how many expiration strings follow.
    8.  **Expirations**: A list of null-terminated strings for each available expiration date (e.g., "20240119").
    9.  **Strikes Count**: An integer specifying how many strike values follow.
    10. **Strikes**: A list of null-terminated float values for each available strike price.

#### Server Response (securityDefinitionOptionParameterEnd)

After all option parameter data has been sent, the server sends a final message to indicate the end of the stream for the request.

-   **Direction**: Server -> Client
--   **Format (`SECURITY_DEFINITION_OPTION_PARAMETER_END`)**: A length-prefixed message.
    1.  **Message ID**: `79`
    2.  **Request ID**: The ID from the original request.

### Request Fundamental Data

-   **Request Message ID**: `52` (`REQ_FUNDAMENTAL_DATA`)
-   **Cancel Message ID**: `53` (`CANCEL_FUNDAMENTAL_DATA`)
-   **Response Message ID**: `51` (`FUNDAMENTAL_DATA`)

This request retrieves fundamental data for a stock, which is returned as an XML string.

#### Client Request (reqFundamentalData)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `52`
2.  **Version**: `2`
3.  **Request ID**: A unique integer for this request.
4.  **Contract ID**: The unique contract identifier. Can be 0 if using symbol.
5.  **Symbol**: The ticker symbol.
6.  **Security Type**: "STK".
7.  **Exchange**: The destination exchange.
8.  **Primary Exchange**: The primary listing exchange.
9.  **Currency**: The contract's currency.
10. **Local Symbol**: The local trading symbol.
11. **Report Type**: The type of report (e.g., "ReportSnapshot", "ReportsFinSummary").
12. **Fundamental Data Options**: A list of tag-value pairs (for internal use, typically empty).

#### Server Response (fundamentalData)

The server sends a single `FUNDAMENTAL_DATA` message containing the requested report as an XML string.

-   **Direction**: Server -> Client
-   **Format (`FUNDAMENTAL_DATA`)**: A length-prefixed message.
    1.  **Message ID**: `51`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.
    4.  **Data**: An XML string containing the fundamental data report.

### Request Matching Symbols

-   **Request Message ID**: `81` (`REQ_MATCHING_SYMBOLS`)
-   **Response Message ID**: `79` (`SYMBOL_SAMPLES`)

This request retrieves a list of contracts that match a given symbol pattern.

#### Client Request (reqMatchingSymbols)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message with a payload containing the following null-terminated fields.

1.  **Message ID**: `81`
2.  **Request ID**: A unique integer for this request.
3.  **Pattern**: The symbol pattern to match (e.g., "IB").

#### Server Response (symbolSamples)

The server sends a single `SYMBOL_SAMPLES` message containing a list of matching contract descriptions.

-   **Direction**: Server -> Client
-   **Format (`SYMBOL_SAMPLES`)**: A length-prefixed message. The payload is complex and contains a variable number of contracts.
    1.  **Message ID**: `79`
    2.  **Request ID**: The ID from the original request.
    3.  **Contract Descriptions Count**: An integer specifying how many `ContractDescription` blocks follow.
    4.  **Contract Description Blocks**: For each contract, the following fields appear in order:
        -   **conId**: The contract's unique identifier.
        -   **symbol**: The contract's symbol.
        -   **secType**: The security type (e.g., "STK").
        -   **primaryExchange**: The primary exchange.
        -   **currency**: The contract's currency.
        -   **Derivative Sec Types Count**: An integer specifying the number of derivative security types that follow.
        -   **Derivative Sec Types**: A list of strings for each derivative type.
        -   **description**: The contract's description (for server versions >= 171).
        -   **issuerId**: The issuer ID (for server versions >= 171).

### Request PnL

-   **Request Message ID**: `95` (`REQ_PNL`)
-   **Cancel Message ID**: `96` (`CANCEL_PNL`)
-   **Response Message ID**: `93` (`PNL`)

This is a subscription that requests the server to start sending Profit and Loss (PnL) updates for a specific account.

#### Client Request (reqPnL)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. For modern servers, the `Version` field is omitted. The payload consists of the following null-terminated fields.

1.  **Message ID**: `95`
2.  **Request ID**: A unique integer for this request.
3.  **Account**: The account number to receive PnL updates for.
4.  **Model Code**: The model code for portfolio models. Can be an empty string if not used.

#### Client Request (cancelPnL)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message to cancel the subscription. For modern servers, the `Version` field is omitted.

1.  **Message ID**: `96`
2.  **Request ID**: The ID of the subscription to cancel.

#### Server Response (PNL)

The server sends a stream of `PNL` messages with the account's profit and loss information.

-   **Direction**: Server -> Client
-   **Format (`PNL`)**: A length-prefixed message with the following fields:
    1.  **Message ID**: `93`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.
    4.  **Daily PnL**: The daily profit and loss.
    5.  **Unrealized PnL**: The unrealized profit and loss.
    6.  **Realized PnL**: The realized profit and loss.

### Request Positions

-   **Request Message ID**: `61` (`REQ_POSITIONS`)
-   **Cancel Message ID**: `64` (`CANCEL_POSITIONS`)
-   **Response Message ID**: `61` (`POSITION_DATA`)
-   **End Message ID**: `62` (`POSITION_END`)

This is a subscription that requests the server to send all account positions. The server will send a stream of `POSITION_DATA` messages followed by a single `POSITION_END` message.

#### Client Request (reqPositions)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. For modern servers, the payload consists of the following null-terminated fields.

1.  **Message ID**: `61`
2.  **Version**: `1`

#### Client Request (cancelPositions)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message to cancel the subscription.

1.  **Message ID**: `64`
2.  **Version**: `1`

#### Server Response (position)

The server sends a stream of `POSITION_DATA` messages, one for each position held across all accounts.

-   **Direction**: Server -> Client
-   **Format (`POSITION_DATA`)**: A length-prefixed message with the following fields:
    1.  **Message ID**: `61`
    2.  **Version**: The version of the message format (e.g., `3`).
    3.  **Account**: The account number this position belongs to.
    4.  **conId**: The unique contract identifier.
    5.  **symbol**: The ticker symbol.
    6.  **secType**: The security type.
    7.  **lastTradeDateOrContractMonth**: The contract's maturity.
    8.  **strike**: The option strike price.
    9.  **right**: The option right ("C" or "P").
    10. **multiplier**: The contract multiplier.
    11. **exchange**: The destination exchange.
    12. **currency**: The contract's currency.
    13. **localSymbol**: The local trading symbol.
    14. **tradingClass**: The trading class (if version >= 2).
    15. **Position**: A `Decimal` indicating the number of units held.
    16. **Average Cost**: A `float` representing the average cost of the position (if version >= 3).

#### Server Response (positionEnd)

After all position data has been sent, the server sends a final message to indicate the end of the stream.

-   **Direction**: Server -> Client
-   **Format (`POSITION_END`)**: A length-prefixed message.
    1.  **Message ID**: `62`
    2.  **Version**: `1`

### Request Account Summary

-   **Request Message ID**: `62` (`REQ_ACCOUNT_SUMMARY`)
-   **Cancel Message ID**: `63` (`CANCEL_ACCOUNT_SUMMARY`)
-   **Response Message IDs**: `63` (`ACCOUNT_SUMMARY`), `64` (`ACCOUNT_SUMMARY_END`)

This is a subscription that requests a summary of account data for a specified group and set of tags. The server will send a stream of `ACCOUNT_SUMMARY` messages followed by a single `ACCOUNT_SUMMARY_END` message.

#### Client Request (reqAccountSummary)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message. The payload consists of the following null-terminated fields.

1.  **Message ID**: `62`
2.  **Version**: `1`
3.  **Request ID**: A unique integer for this request.
4.  **Group**: The account group to query. "All" for all accounts.
5.  **Tags**: A comma-separated list of tags to retrieve (e.g., `NetLiquidation,TotalCashValue`).

#### Server Response (accountSummary)

The server sends a stream of `ACCOUNT_SUMMARY` messages, one for each tag for each account in the requested group.

-   **Direction**: Server -> Client
-   **Format (`ACCOUNT_SUMMARY`)**: A length-prefixed message with the following fields:
    1.  **Message ID**: `63`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.
    4.  **Account**: The account number this summary line belongs to.
    5.  **Tag**: The name of the value (e.g., "NetLiquidation").
    6.  **Value**: The string representation of the value.
    7.  **Currency**: The currency of the value.

#### Server Response (accountSummaryEnd)

After all summary data has been sent, the server sends a final message to indicate the end of the stream for the request.

-   **Direction**: Server -> Client
-   **Format (`ACCOUNT_SUMMARY_END`)**: A length-prefixed message.
    1.  **Message ID**: `64`
    2.  **Version**: `(unused)`
    3.  **Request ID**: The ID from the original request.

### Request Account Updates

-   **Request Message ID**: `6` (`REQ_ACCT_DATA`)
-   **Response Message IDs**: `6` (`ACCT_VALUE`), `7` (`PORTFOLIO_VALUE`)

This is a subscription request. The client asks the server to start (`subscribe=1`) or stop (`subscribe=0`) sending updates about account values and portfolio positions.

#### Client Request (reqAccountUpdates)

-   **Direction**: Client -> Server
-   **Format**: A length-prefixed message.
    1.  **Message ID**: `6`
    2.  **Version**: `2`
    3.  **Subscribe**: `1` (to subscribe) or `0` (to unsubscribe).
    4.  **Account Code**: The account number to receive updates for.

#### Server Response (updateAccountValue)

The server sends a stream of messages with updates. `ACCT_VALUE` messages contain key-value pairs for general account metrics.

-   **Direction**: Server -> Client
-   **Format (`ACCT_VALUE`)**: A length-prefixed message with the following fields:
    1.  **Message ID**: `6`
    2.  **Version**: `2`
    3.  **Key**: The name of the account value (e.g., "TotalCashValue", "AccountReady").
    4.  **Value**: The value associated with the key.
    5.  **Currency**: The currency of the value (e.g., "USD", "BASE").
    6.  **Account Name**: The account this value belongs to.

#### Server Response (updatePortfolio)

`PORTFOLIO_VALUE` messages contain detailed information about each position held.

-   **Direction**: Server -> Client
-   **Format (`PORTFOLIO_VALUE`)**: A length-prefixed message containing a large number of fields describing the contract (symbol, secType, currency, etc.) and the position (position size, market price, average cost, P&L, etc.). The full, complex structure is defined by the `processPortfolioValueMsg` function in the official client's `decoder.py`.

## Common Server-side Messages

This section describes messages that the server may send that are not direct responses to a specific client request.

### Error and Informational Messages

The server uses a generic message format to send both errors related to a request and general status updates to the client.

-   **Message ID**: `4` (`ERR_MSG`)
-   **Direction**: Server -> Client
-   **Format**: A length-prefixed message with the following null-terminated fields:
    1.  **Message ID**: `4`
    2.  **Version**: `2` (typically)
    3.  **Request ID**: The ID of the request that caused the error. If the message is a general status update not tied to a specific request, this will be `-1`.
    4.  **Error Code**: An integer code identifying the error or message type.
    5.  **Error Message**: A string describing the error or status.

-   **Example (Error)**: `['4', '2', '-1', '320', "Error reading request. Unable to parse field..."]`
-   **Example (Informational)**: `['4', '2', '-1', '2104', 'Market data farm connection is OK:usfarm.nj']`

### Managed Accounts List

-   **Message ID**: `15` (`MANAGED_ACCTS`)
-   **Direction**: Server -> Client
-   **Format**: A length-prefixed message with the following null-terminated fields:
    1.  **Message ID**: `15`
    2.  **Version**: `1` (unused)
    3.  **Accounts List**: A comma-separated list of account numbers the client has access to.
-   **Context**: This message is sent unsolicited upon connection to a Financial Advisor (FA) account, and is also sent as a direct response to a client's `reqManagedAccts` (ID 17) request.


## Implementation Notes & Learnings

This section summarizes key takeaways and potential pitfalls discovered while implementing a client based on this protocol.

**1. The Handshake is a Multi-Step, Special-Case Process**

The initial connection handshake does not follow the standard message framing for all its parts and requires careful, ordered implementation. A failure to follow this sequence precisely will result in the server closing the connection without an explicit error message.

*   **Step 1: Client Hello:** The very first bytes sent by the client **must** be the literal ASCII string `API\0`. This is immediately followed by the length-prefixed API version string (e.g., `v100..187`). This is sent as a single, continuous message to the socket.

*   **Step 2: Server Hello:** The server's response is a **single** standard length-prefixed message. The payload of this single message contains two null-terminated fields: the server version and the connection timestamp. It is crucial *not* to attempt to read two separate messages or raw bytes, but to parse it as one standard message frame.

*   **Step 3: Start API:** The client sends its first standard message, `START_API` (ID 71), to finalize the connection. No other API requests will be accepted before this message is sent.

**2. Debugging Disconnections**

*   If the server closes the connection immediately after the client sends a message (e.g., a `read` or `recv` call returns 0 bytes), it is almost certainly due to a malformed message from the client.
*   During the handshake, this is most often caused by an incorrect "Client Hello" message format.
*   For standard API messages after the handshake, this can be caused by an incorrect message ID, a wrong version number for that message, or an incorrect number or type of fields for that specific request.

**3. Message Field Encoding**

*   All fields in a standard message payload, even those representing numbers (like Message IDs, versions, or Client IDs), should be converted to their string representation before being joined with null terminators and encoded in ASCII. 

**4. Conditional Version Fields**

*   A critical learning is that the `Version` field is **not** always present in client requests, even if the protocol documentation for a given message includes it. Its inclusion is often conditional on the server version negotiated during the handshake.
*   For example, in a `reqHistoricalData` (ID 20) message, the client only sends the `Version` field if the server version is less than 101. For modern servers that do not expect it, sending the version field will cause a field misalignment and result in a parsing error on the server side (e.g., `Error reading request. Unable to parse field...`).
*   Always consult the reference client implementation for the specific server version checks surrounding the inclusion of the `Version` field for any given message. 

**5. Handling Informational Messages**

*   The server will proactively send status messages (e.g., about data farm connections) using the same message format as errors (ID `4`). These messages are not failures and can typically be logged for informational purposes. They are often identifiable by a `Request ID` of `-1`. 

**6. Silent Failures and Modern Server Requirements**

*   **Server Behavior on Weekends**: Some requests, particularly those involving calculations like `calculateOptionPrice` and `calculateImpliedVolatility`, may not receive a response from the server during weekends or when markets are closed. The server does not send an error message in this case; it simply ignores the request. The client must rely on a timeout to handle this situation.
*   **Empty `conId` for Symbol Lookups**: When requesting data for a contract without knowing its `conId`, the `conId` field should be sent as an **empty string**. Sending an integer `0` may cause the request to fail silently.
*   **"Optional" Fields are Not Always Optional**: Modern server versions may require fields that are marked as "optional" in older documentation. For example, `calculateOptionPrice` requires `customerAccount`, `professionalCustomer`, and `optPrcOptions` fields to be sent (even if empty) to a modern server. Omitting these fields can cause the server to ignore the request without sending an error. Cross-referencing with a working client implementation is crucial.