library(tidyverse)
library(tidyquant)
library(FFdownload)


#get asset prices
getSymbols(Symbols = "AMZN", from="2019-09-01", to="2020-09-01", periodicity="monthly")
getSymbols(Symbols = "AAPL", from="2019-09-01", to="2020-09-01", periodicity="monthly")
getSymbols(Symbols = "MSFT", from="2019-09-01", to="2020-09-01", periodicity="monthly")
getSymbols(Symbols = "GOOG", from="2019-09-01", to="2020-09-01", periodicity="monthly")


#all data together to a xts
stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], AMZN = AMZN[, "AMZN.Close"],
                            MSFT = MSFT[, "MSFT.Close"], GOOG = GOOG[, "GOOG.Close"]))
head(stocks)
plotbox <- vecst<-data.matrix(as.data.frame(stocks)) #vecst=matrix

#plotting the data - histogram
h<-hist(vecst, breaks=10, col="red", xlab="ClosingPrice",
        main="Histogram with Normal Curve")
xfit<-seq(min(vecst),max(vecst),length=40)
yfit<-dnorm(xfit,mean=mean(vecst),sd=sd(vecst))
yfit <- yfit*diff(h$mids[1:2])*length(vecst)
lines(xfit, yfit, col="blue", lwd=2)


#plotting the data - boxplot
boxplot(vecst)

#plotting the data - qqplot
qqnorm(stocks) #stocks=xtsfile
