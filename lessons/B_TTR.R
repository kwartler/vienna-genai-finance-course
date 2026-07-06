#' Author: Ted Kwartler
#' Date: Oct 21, 2022
#' Updated: added an EMA comparison section
#' Purpose: Simple Moving Avg Example, then a comparison against an Exponential Moving Avg
#'

# Opts
options(scipen=999)

# Libs
library(TTR)
library(quantmod)

# Get Chipotle
getSymbols("CMG") 
CMG <- CMG['2018-03-01/']

# Plot
plot(CMG$CMG.Close)

# Calculate the moving Avgs
CMGma3  <- SMA(CMG$CMG.Close, 3)
CMGma10 <- SMA(CMG$CMG.Close, 10)
CMGma30 <- SMA(CMG$CMG.Close, 30)
CMGma100 <- SMA(CMG$CMG.Close, 100)
CMGma250 <- SMA(CMG$CMG.Close, 250)

# Plot different MA windows
plot(CMG$CMG.Close)
lines(CMGma3, col='red')

plot(CMG$CMG.Close)
lines(CMGma10, col='red')

plot(CMG$CMG.Close)
lines(CMGma30, col='red')

plot(CMG$CMG.Close)
lines(CMGma100, col='red')

plot(CMG$CMG.Close)
lines(CMGma250, col='red')

## Now compare to an Exponential Moving Average (EMA)
# An SMA treats every day in the window equally.
# An EMA gives more weight to recent prices and less weight to older prices,
# so it reacts faster when the price changes direction.

CMGema3   <- EMA(CMG$CMG.Close, 3)
CMGema10  <- EMA(CMG$CMG.Close, 10)
CMGema30  <- EMA(CMG$CMG.Close, 30)
CMGema100 <- EMA(CMG$CMG.Close, 100)
CMGema250 <- EMA(CMG$CMG.Close, 250)

# Overlay the SMA and the EMA on the same window, so the responsiveness
# difference is visible on one chart rather than described in words.
plot(CMG$CMG.Close)
lines(CMGma10, col='red')
lines(CMGema10, col='blue')
legend("topleft", legend = c("Close", "SMA 10", "EMA 10"), col = c("black", "red", "blue"), lty = 1)

plot(CMG$CMG.Close)
lines(CMGma30, col='red')
lines(CMGema30, col='blue')
legend("topleft", legend = c("Close", "SMA 30", "EMA 30"), col = c("black", "red", "blue"), lty = 1)

# Compare the two directly on the last 15 trading days.
# Watch which one moves first when the price changes direction.
compare <- data.frame(close = as.vector(tail(CMG$CMG.Close, 15)),
                       sma10 = as.vector(tail(CMGma10, 15)),
                       ema10 = as.vector(tail(CMGema10, 15)))
compare

# End

