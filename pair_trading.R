library(quantstrat)
library(PerformanceAnalytics)
library(tseries)

source("pair_formation.R")
source("check_blotter_update.R")

Sys.setenv(TZ = "UTC")
currency("USD")

#single pair for now, withmaximum correlation
pair <- trading_pairs[[1]]
quantmod::getSymbols(Symbols = pair, src = "yahoo", index.class = "POSIXct", from = start_date, to = end_date)

FinancialInstrument::stock("spread", currency = "USD", multiplier = 1)

portfolio.st <- "PairTrading"

rm.strat(portfolio.st)
rm.strat(portfolio.st)

spread<-OHLC(.GlobalEnv[[pair[1]]]) - OHLC(.GlobalEnv[[pair[2]]])
colnames(spread)<- c("open", "high", "low", "close")

chart_Series(spread)
blotter.initPortf(name = portfolio.st, symbols = "spread", initDate = init_date)

blotter::initAcct(name = portfolio.st, portfolios = portfolio.st, initDate = init_date, initEq = init_equity)

quantstrat::initOrders(portfolio = portfolio.st, symbols = 'spread', initDate = init_date)

quantstrat::strategy(portfolio.st, store=TRUE)

pair_info<-log10(.GlobalEnv[[pair[1]]][,6] / .GlobalEnv[[pair[2]]][,6])

pair_info <- cbind(pair_info, rollapply(pair_info[,1], 5, mean))
pair_info <- cbind(pair_info, rollapply(pair_info[,1],5,sd))

ZScore <- function(x){
  z <- (x$price_ratio - x$smoothed_price_ratio)/ x$price_ratio_sd
  return(z)
}

pair_info <- cbind(pair_info, (pair_info[,1] - pair_info[,2]) / pair_info[,3])
colnames(pair_info) <- c("price_ratio", "smoothed_price_ratio", "price_ratio_sd", "z_score")

plot(main = "Z-Score Time Series", xlab = "Date", ylab = "Z-Score", pair_info, type = "I")

quantstrat::add.indicator(strategy = portfolio.st, name = "ZScore", arguments =
                            list(x=pair_info), label = "ZScore")

#zscore threshold for detecting divergence
buy_threshold <- -1.5
sell_threshold <- -buy_threshold
exit_long <- 0.5
exit_short<- 0.5

quantstrat::add.signal(strategy = portfolio.st, name="sigThreshold",arguments=list(column="price_ratio.ZScore", threshold=buy_threshold,
                                                                                   relationship="lt", cross=FALSE),label="long_Z")

quantstrat::add.signal(strategy = portfolio.st, name="sigThreshold",arguments=list(column="price_ratio.ZScore", threshold= exit_long,
                                                                                   relationship="gt", cross=FALSE),label="long_Z_exit")

quantstrat::add.signal(strategy = portfolio.st, name="sigThreshold",arguments=list(column="price_ratio.ZScore", threshold=sell_threshold,
                                                                                   relationship="gt", cross=FALSE),label="short_Z")

quantstrat::add.signal(strategy = portfolio.st, name="sigThreshold",arguments=list(column="price_ratio.ZScore", threshold= exit_short,
                                                                                   relationship="lt", cross=FALSE),label="short_Z_exit")

quantstrat::addPosLimit( portfolio = portfolio.st,
                         symbol = 'spread',
                         timestamp = initDate,
                         maxpos = 100,
                         longlevels = 1,
                         minpos = -100)

quantstrat::add.rule(strategy = portfolio.st, name='ruleSignal',arguments = list(sigcol="long_Z",
                                                                                 sigval=TRUE, orderqty=100,  osFUN = osMaxPos, replace = FALSE, ordertype='market',
                                                                                 orderside='long', prefer = "open"), type='enter', label = 'enter' )

quantstrat::add.rule(strategy = portfolio.st, name='ruleSignal', arguments = list(sigcol="short_Z",
                                                                                  sigval=TRUE, orderqty=-100,  osFUN = osMaxPos, replace = FALSE,ordertype='market',
                                                                                  orderside='short', prefer = "open"), type='enter', label = "enter")

quantstrat::add.rule(strategy = portfolio.st, name='ruleSignal', arguments = list(sigcol="long_Z_exit",
                                                                                  sigval=TRUE, orderqty= 'all', ordertype='market', orderside='short', prefer = "open"), type='exit', label = 'exit')

quantstrat::add.rule(strategy = portfolio.st, name='ruleSignal', arguments = list(sigcol="short_Z_exit",
                                                                                  sigval=TRUE, orderqty= 'all' , ordertype='market', orderside='long', prefer = "open"), type='exit')

summary(get.strategy(portfolio.st))
results <- applyStrategy(portfolio.st, portfolios = portfolio.st, mktdata = spread)
updatePortf(portfolio.st)
updateAcct(portfolio.st)
updateEndEq(portfolio.st)

if(checkBlotterUpdate(portfolio.st, portfolio.st, verbose = TRUE)){
  save.strategy(portfolio.st)
}

tstats = tradeStats(portfolio.st)
ptstats = perTradeStats(portfolio.st, Symbol = "spread")
txns = getTxns(portfolio.st, "spread")

print(t(tstats))

rets <- PortfReturns(Account = portfolio.st)
charts.PerformanceSummary(rets, colorset = bluefocus)















