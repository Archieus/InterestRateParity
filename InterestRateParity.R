library(Quandl)
library(quantmod)
Sys.setenv(TZ = "EST5EDT")
Quandl.api_key("LDvEJuPkQWrPGpHwokVx")

SP500PE <- Quandl("MULTPL/SP500_PE_RATIO_MONTH", type = "xts")
SCHILLER <- Quandl("MULTPL/SHILLER_PE_RATIO_MONTH", type = "xts")
SP500EY <- Quandl("MULTPL/SP500_EARNINGS_YIELD_MONTH", type = "xts")
SP500mo <- Quandl("MULTPL/SP500_REAL_PRICE_MONTH", type = "xts")

getSymbols("DGS10", src = "FRED")

#### Interest Rate using 10-year Treasury ####
UST10.mo <- to.monthly(DGS10)

USTPE <- na.locf((1/DGS10)*100,na.rm = FALSE, fromLast = TRUE)
USTPE.mo <- to.monthly(USTPE)

#### Schiller E/Y (Inverse of Schiller P/E) ####
SCH.EY <- (1/SCHILLER)*100

#### Combining Data ####
RPData <- na.omit(cbind(SP500PE, SCHILLER, USTPE.mo[,4], SP500mo, SP500EY, UST10.mo[,4]))
RPData$Sprd <- RPData[,5]- RPData[,6]
names(RPData) <- c("S&P500PE", "SchillerPE", "IntRateParity", "SP500", "SP500EY", "UST10Yld", "EY-10Yr")

#### Earnings Yield Data ####
EYData <- na.omit(cbind(SP500EY, SCH.EY, UST10.mo[,4]))
EYData$SP5Sprd <- EYData[,1]-EYData[,3]
EYData$SchSprd <- round(EYData[,2]- EYData[,3],2)
names(EYData) <- c("SP500EY", "SchillerEY", "UST10Yld", "SP5EY-US10Y", "SchEY-US10Y")

#### P/E Relative to Risk Parity ####
#### REDUCE EQUTIY EXPOSURE WHEN RELATIONSHIP IS GREATER THAN 15% ####
PE.Parity <- as.data.frame(lapply(RPData[,c(1:2)], function(x) {x - RPData[,3]}))

#### Bond Equity Earnings Yield Ratio (BEER) ####
#### BEER < 1 may indicate Equity Market is undervalued or bond market not adequately pricing risk ####
#### BEER > 1 may indicate Equity Market is overvalued ####
#### BEER = 1 may indicate equal levels of risk in bond market and equity market ####

BEER <- RPData[,6]/RPData[,5]

#### PLOT CHARTS ####
layout(1:2)
plot(RPData[,4], main = "S&P 500", type = 'l')
plot(BEER, main = "Bond Equity Earnings Yield Ratio (BEER)", type = "l")



