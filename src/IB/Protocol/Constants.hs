module IB.Protocol.Constants
  ( -- Request Message IDs
    reqMktData
  , cancelMktData
  , placeOrder
  , reqAcctData
  , reqExecutions
  , reqIds
  , reqContractData
  , reqMktDepth
  , cancelMktDepth
  , reqManagedAccts
  , reqHistoricalData
  , reqScannerSubscription
  , cancelScannerSubscription
  , cancelHistoricalData
  , reqCurrentTime
  , reqRealTimeBars
  , cancelRealTimeBars
  , reqFundamentalData
  , cancelFundamentalData
  , reqCalcImpliedVolat
  , reqCalcOptionPrice
  , cancelCalcImpliedVolat
  , cancelCalcOptionPrice
  , reqPositions
  , reqAccountSummary
  , cancelAccountSummary
  , cancelPositions
  , startApi
  , reqSecDefOptParams
  , reqMatchingSymbols
  , reqHeadTimestamp
  , reqPnl
  , cancelPnl
  , reqTickByTickData
  , cancelTickByTickData
    -- Response Message IDs
  , tickPrice
  , tickSize
  , orderStatus
  , errMsg
  , openOrder
  , acctValue
  , portfolioValue
  , acctUpdateTime
  , nextValidId
  , contractData
  , execDetails
  , updateMktDepth
  , updateMktDepthL2
  , updateNewsBulletin
  , managedAccts
  , historicalData
  , scannerData
  , tickOptionComputation
  , currentTime
  , realTimeBars
  , fundamentalData
  , contractDataEnd
  , openOrderEnd
  , acctDownloadEnd
  , execDetailsEnd
  , positionData
  , positionEnd
  , accountSummary
  , accountSummaryEnd
  , securityDefinitionOptionParameter
  , securityDefinitionOptionParameterEnd
  , symbolSamples
  , headTimestamp
  , pnl
  , tickByTick
  , tickEFP
  , marketDataType
  ) where

import IB.Protocol.Types (MessageId(..))

-- Request Message IDs
reqMktData :: MessageId
reqMktData = MessageId 1

cancelMktData :: MessageId
cancelMktData = MessageId 2

placeOrder :: MessageId
placeOrder = MessageId 3

reqAcctData :: MessageId
reqAcctData = MessageId 6

reqExecutions :: MessageId
reqExecutions = MessageId 7

reqIds :: MessageId
reqIds = MessageId 8

reqContractData :: MessageId
reqContractData = MessageId 9

reqMktDepth :: MessageId
reqMktDepth = MessageId 10

cancelMktDepth :: MessageId
cancelMktDepth = MessageId 11

reqManagedAccts :: MessageId
reqManagedAccts = MessageId 17

reqHistoricalData :: MessageId
reqHistoricalData = MessageId 20

reqScannerSubscription :: MessageId
reqScannerSubscription = MessageId 22

cancelScannerSubscription :: MessageId
cancelScannerSubscription = MessageId 23

cancelHistoricalData :: MessageId
cancelHistoricalData = MessageId 25

reqCurrentTime :: MessageId
reqCurrentTime = MessageId 49

reqRealTimeBars :: MessageId
reqRealTimeBars = MessageId 50

cancelRealTimeBars :: MessageId
cancelRealTimeBars = MessageId 51

reqFundamentalData :: MessageId
reqFundamentalData = MessageId 52

cancelFundamentalData :: MessageId
cancelFundamentalData = MessageId 53

reqCalcImpliedVolat :: MessageId
reqCalcImpliedVolat = MessageId 54

reqCalcOptionPrice :: MessageId
reqCalcOptionPrice = MessageId 55

cancelCalcImpliedVolat :: MessageId
cancelCalcImpliedVolat = MessageId 56

cancelCalcOptionPrice :: MessageId
cancelCalcOptionPrice = MessageId 57

reqPositions :: MessageId
reqPositions = MessageId 61

reqAccountSummary :: MessageId
reqAccountSummary = MessageId 62

cancelAccountSummary :: MessageId
cancelAccountSummary = MessageId 63

cancelPositions :: MessageId
cancelPositions = MessageId 64

startApi :: MessageId
startApi = MessageId 71

reqSecDefOptParams :: MessageId
reqSecDefOptParams = MessageId 78

reqMatchingSymbols :: MessageId
reqMatchingSymbols = MessageId 81

reqHeadTimestamp :: MessageId
reqHeadTimestamp = MessageId 87

reqPnl :: MessageId
reqPnl = MessageId 95

cancelPnl :: MessageId
cancelPnl = MessageId 96

reqTickByTickData :: MessageId
reqTickByTickData = MessageId 97

cancelTickByTickData :: MessageId
cancelTickByTickData = MessageId 98


-- Response Message IDs
tickPrice :: MessageId
tickPrice = MessageId 1

tickSize :: MessageId
tickSize = MessageId 2

orderStatus :: MessageId
orderStatus = MessageId 3

errMsg :: MessageId
errMsg = MessageId 4

openOrder :: MessageId
openOrder = MessageId 5

acctValue :: MessageId
acctValue = MessageId 6

portfolioValue :: MessageId
portfolioValue = MessageId 7

acctUpdateTime :: MessageId
acctUpdateTime = MessageId 8

nextValidId :: MessageId
nextValidId = MessageId 9

contractData :: MessageId
contractData = MessageId 10

execDetails :: MessageId
execDetails = MessageId 11

updateMktDepth :: MessageId
updateMktDepth = MessageId 12

updateMktDepthL2 :: MessageId
updateMktDepthL2 = MessageId 13

updateNewsBulletin :: MessageId
updateNewsBulletin = MessageId 14

managedAccts :: MessageId
managedAccts = MessageId 15

historicalData :: MessageId
historicalData = MessageId 17

scannerData :: MessageId
scannerData = MessageId 20

tickOptionComputation :: MessageId
tickOptionComputation = MessageId 21

currentTime :: MessageId
currentTime = MessageId 49

realTimeBars :: MessageId
realTimeBars = MessageId 50

fundamentalData :: MessageId
fundamentalData = MessageId 51

contractDataEnd :: MessageId
contractDataEnd = MessageId 52

openOrderEnd :: MessageId
openOrderEnd = MessageId 53

acctDownloadEnd :: MessageId
acctDownloadEnd = MessageId 54

execDetailsEnd :: MessageId
execDetailsEnd = MessageId 55

positionData :: MessageId
positionData = MessageId 61

positionEnd :: MessageId
positionEnd = MessageId 62

accountSummary :: MessageId
accountSummary = MessageId 63

accountSummaryEnd :: MessageId
accountSummaryEnd = MessageId 64

securityDefinitionOptionParameter :: MessageId
securityDefinitionOptionParameter = MessageId 79

securityDefinitionOptionParameterEnd :: MessageId
securityDefinitionOptionParameterEnd = MessageId 80

symbolSamples :: MessageId
symbolSamples = MessageId 81

headTimestamp :: MessageId
headTimestamp = MessageId 87

pnl :: MessageId
pnl = MessageId 95

tickByTick :: MessageId
tickByTick = MessageId 97

tickEFP :: MessageId
tickEFP = MessageId 47

marketDataType :: MessageId
marketDataType = MessageId 58
