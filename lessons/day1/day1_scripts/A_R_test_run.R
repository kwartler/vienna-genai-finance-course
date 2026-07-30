#' A_R_test_run.R
#' TK
#' July 28, 2026


# Libraries
library(ggplot2)
library(echarts4r)

# Set seed for reproducibility
set.seed(42)

# Generate some sample OHLC data
n     <- 250
dates <- seq.Date(from = as.Date("2026-01-01"), by = "day", length.out = n) 

# Creating a data frame with fake OHLC data
fakeOHLC <- data.frame(Date = dates,
                       Open = runif(n, min = 95, max = 105),
                       Close = runif(n, min = 98, max = 102),
                       Low = runif(n, min = 90, max = 100),
                       High = runif(n, min = 100, max = 110))

# View the first few rows of the data frame
head(fakeOHLC)

# Summary statistics
summary(fakeOHLC)

# Simple static plot
simplePlot <- ggplot(fakeOHLC, aes(x = Date, y = Close)) +
  geom_line() +
  theme_minimal() + 
  ggtitle("Fake Closing Data")

# Show in R Studio
print(simplePlot)

# Simple interactive plot
dygraph(ts(fakeOHLC$Close)) %>% dyRangeSelector()

# End