
#### STEP 0 : Data Import
### import
sales <- read.csv("/cloud/project/data/UsedCarSales.txt")
attach(sales)
Time <- as.numeric(1:nrow(sales))
fMonth <- as.factor(Month)
sales <- data.frame(sales,Time, fMonth)

#### STEP 1: Part 1:
### three periods of economic contraction
sales.ts<- ts(Sales, start = c(1992,1), freq = 12)
plot(sales.ts, xlab = "time", ylab = "Sales", main = "Sales vs. time")
logsales.ts<- ts(log(Sales), start = c(1992,1), freq = 12)
plot(logsales.ts, xlab = "time", ylab = "Log Sales", main = "Sales vs. time", col = "red")
sales_vector <- sales$Sales
## contraction
max(sales$Sales)
salescontraction<-
  c(rep(NA,111),sales_vector[112:119], rep(NA,73) ,sales_vector[193:210],
    rep(NA,128),sales_vector[339:340],rep(NA,20))
salescontraction
sales_regular<-
  c(sales_vector[1:111],rep(NA,8), sales_vector[120:192] ,rep(NA,18),
    sales_vector[211:338],rep(NA,2),sales_vector[341:360])
sales_regular
test<- cbind(salescontraction, sales_regular)
plot(ts(salescontraction,start = c(1992,1), freq = 12), 
     col = "red", lwd = 2, ylab = "Sales", 
     main = "Sales vs. time", ylim = c(1000,18000))
lines(ts(sales_regular, start = c(1992,1), freq = 12), col = "black")
## log
plot(ts(log(salescontraction),start = c(1992,1), freq = 12), 
     col = "red", lwd = 2, ylab = "Log Sales", 
     main = "Sales vs. time",  ylim = c(7,10))
lines(ts(log(sales_regular), start = c(1992,1), freq = 12), col = "black")


### STEP 2: Part 2: Form a spectral plot using just years

small_sales <- sales[1:336,]
