library(sqldf) # SQL
library(astsa)
setwd("~/Documents/Rossman sales/Rossman")
Train<- read.csv("train.csv", header=T)


try1=sqldf(
  'select Sales, Date
  from Train a
  where Store=1'
)

ts1=ts(try1$Sales, start=2013, frequency=365)
# scatter plot
par(mfrow=c(1,1))
plot.ts(ts1, ylab="", main="store 1")

# ACF & PACF
par(mfrow=c(2,1))
acf(ts1, 100)
pacf(ts1,100)

