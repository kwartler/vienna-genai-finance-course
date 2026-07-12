#' Author: Ted Kwartler
#' Date: Oct 21, 2022
#' Updated: added an EMA based MACD alongside the original SMA based MACD, for direct comparison
#' Purpose: MACD Example As Indicator
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)
library(PerformanceAnalytics)
library(dygraphs)
library(htmltools)

# Get Chipotle
getSymbols("CMG") #"CMG_1_TTR_D.rds"
#CMG <- CMG['2018-01-01/2019-01-01']
CMG <- CMG['2022-01-01/']


# Manual MACD, SMA version
# FAST MA
CMGsma12 <- SMA(CMG$CMG.Close, 12)
tail(CMGsma12, 5) 

# SLOW MA
CMGsma26 <- SMA(CMG$CMG.Close, 26)
tail(CMGsma26, 5) 

# MA Difference
SMAdiff <- CMGsma12 - CMGsma26
tail(SMAdiff, 5) 
tail(CMGsma12, 1) - tail(CMGsma26, 1) #same as above

# 3rd Moving Avg of the difference between the two
manualSigSMA <- SMA(SMAdiff, 9)

# Manual MACD, EMA version
# MACD conventionally uses an EMA, not an SMA, for the fast, slow, and signal lines.
# An EMA weights recent prices more heavily, so MACD reacts faster to a change
# in momentum than an SMA based version would. We calculate both here so the
# difference is visible side by side, rather than just described.

# FAST EMA
CMGema12 <- EMA(CMG$CMG.Close, 12)
tail(CMGema12, 5) 

# SLOW EMA
CMGema26 <- EMA(CMG$CMG.Close, 26)
tail(CMGema26, 5) 

# EMA Difference, this is the MACD line
EMAdiff <- CMGema12 - CMGema26
tail(EMAdiff, 5) 
tail(CMGema12, 1) - tail(CMGema26, 1) #same as above

# 3rd moving average of the difference between the two, this is the signal line
manualSigEMA <- EMA(EMAdiff, 9)

# Calculate both versions with the TTR function, so the manual math above can be checked
CMGmacdSMA <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="SMA",
                percent = F) # Values or Percents

CMGmacdEMA <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="EMA", # standard practice, matches most trading platforms
                percent = F)

# Examine to ensure the underlying math is understood, for both versions
data.frame(manualSignalSMA = as.vector(tail(manualSigSMA, 5)),
           ttrSignalSMA    = tail(CMGmacdSMA$signal, 5),
           manualSignalEMA = as.vector(tail(manualSigEMA, 5)),
           ttrSignalEMA    = tail(CMGmacdEMA$signal, 5))

# Compare the two MACD lines directly. Watch for days where the SMA and
# EMA versions would have you take opposite positions.
data.frame(macdSMA = as.vector(tail(CMGmacdSMA$macd, 10)),
           macdEMA = as.vector(tail(CMGmacdEMA$macd, 10)))

# Same comparison, but as a chart rather than a table
plot(CMGmacdSMA$macd, main = "MACD line: SMA versus EMA")
lines(CMGmacdEMA$macd, col = 'blue')
legend("topleft", legend = c("MACD (SMA)", "MACD (EMA)"), col = c("black", "blue"), lty = 1)

# For some it's easier to interpret as a percent of share price
# We continue with EMA from here, since that is the version most trading
# platforms compute by default.
CMGmacdPer <- MACD(CMG$CMG.Close,
                nFast = 12, 
                nSlow = 26, 
                nSig = 9, 
                maType="EMA", 
                percent = T)
tail(CMGmacdPer)

# As a trading indicator
signal <- Lag(ifelse(CMGmacdPer$macd > CMGmacdPer$signal, 1, 0))

# Quick Check
table(signal)

CMG <- merge(CMG$CMG.Close, CMGmacdPer)
CMG$MACDindicator <- CMG$macd - CMG$signal

# Now let's visualize in a stacked dynamic plot
browsable(
  tagList(
    dygraph(CMG$CMG.Close, 
            group = "Price", 
            height = 200, 
            width = "100%"),
    dygraph(CMGmacdPer,
            group = "Price", 
            height = 200, 
            width = "100%") %>%
      dySeries('macd',label='MACD') %>%
      dySeries('signal',label='SIGNAL') %>%
      dyRangeSelector()
  )
)

# Now let's visualize in a stacked dynamic plot; anytime signal value is positive buy
browsable(
  tagList(
    dygraph(CMG$CMG.Close, 
            group = "Price", 
            height = 200, 
            width = "100%"),
    dygraph(CMG$MACDindicator,
            group = "Price", 
            height = 200, 
            width = "100%") %>%
      dyRangeSelector()
  )
)

# End