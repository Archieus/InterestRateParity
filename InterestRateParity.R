library(Quandl)
library(quantmod)
Sys.setenv(TZ = "EST5EDT")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

SP500PE <- Quandl("MULTPL/SP500_PE_RATIO_MONTH", type = "xts", start_date = "2007-01-01")
SCHILLER <- Quandl("MULTPL/SHILLER_PE_RATIO_MONTH", type = "xts", start_date = "2007-01-01")
SP500EY <- Quandl("MULTPL/SP500_EARNINGS_YIELD_MONTH", type = "xts", start_date = "2007-01-01")
getSymbols(c("DGS10","SP500"), src = "FRED", from = "2007-01-01")

SP500.mo <- na.locf(to.monthly(SP500), na.rm = FALSE, fromLast = TRUE)

#### Interest Rate using 10-year Treasury ####
UST10.mo <- to.monthly(DGS10)

USTPE <- na.locf((1/DGS10)*100,na.rm = FALSE, fromLast = TRUE)
USTPE.mo <- to.monthly(USTPE)

#### Combining Data ####
RPData <- na.omit(cbind(SP500PE, SCHILLER, USTPE.mo[,4], SP500.mo[,4], SP500EY, UST10.mo[,4]))
names(RPData) <- c("S&P500PE", "SchillerPE", "IntRateParity", "SP500", "SP500EY", "UST10Yld")

#### P/E Relative to Risk Parity ####
PE.Parity <- as.data.frame(lapply(RPData, function(x) {x - RPData[,3]}))

#### REDUCE EQUTIY EXPOSURE WHEN RELATIONSHIP IS GREATER THAN 15% ####

#### Bond Equity Earnings Yield Ratio (BEER) ####
#### BEER < 1 may indicate Equity Market is undervalued or bond market not adequately pricing risk ####
#### BEER > 1 may indicate Equity Market is overvalued ####
#### BEER = 1 may indicate equal levels of risk in bond market and equity market ####

BEER <- RPData[,6]/RPData[,5]

#### PLOT CHARTS ####
layout(1:4)
plot(RPData[,4], main = "S&P 500", type = 'l')
plot(RPData[,2], main = "S&P 500 Rel Int Rate Parity", type = 'l')
plot(RPData[,3], main = "Schiller Rel Int Rate Parity", type = 'l')
plot(BEER, main = "Bond Equity Earnings Yield Ratio (BEER)", type = "l")



