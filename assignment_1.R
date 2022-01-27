
### import
sales <- read.csv("/cloud/project/data/GroceryStoreSales.txt")
attach(sales)
Time <- as.numeric(Time)
fMonth <- as.factor(Month)
sales <- data.frame(sales,fMonth)

### question 1
sales_vector<- sales$Sales
sales.ts<- ts(sales_vector, start = c(1992,1), freq = 12)
plot(sales.ts, xlab = "time", ylab = "Sales", main = "Sales vs. time")
lsales<- log(sales_vector)
sales.ts<- ts(lsales, start = c(1992,1), freq = 12)
plot(sales.ts, xlab = "time", ylab = "Log Sales", main = "Sales vs. time")
## contraction
salescontraction<-
  c(rep(NA,110),sales_vector[111:119], rep(NA,73) ,sales_vector[193:210],
    rep(NA,128),sales_vector[339:340],rep(NA,20))
salescontraction
sales_regular<-
  c(sales_vector[1:111],rep(NA,8), sales_vector[120:192] ,rep(NA,18),
   sales_vector[211:338],rep(NA,1),sales_vector[340:360])
sales_regular
plot(ts(salescontraction,start = c(1992,1), freq = 12), col = "red", lwd = 2)
lines(ts(sales_regular, start = c(1992,1), freq = 12), col = "black")

### question 2
model1 <- lm(logSales~poly(Time,4) + fMonth + 
               c348+s348+c432+s432)
summary(model1)
## part a
b1 <- coef(model1)[1]
b2 <- coef(model1)[6:16]+b1
b3<- c(b1,b2)
seas <- exp(b3-mean(b3))
seas
seas.ts <- ts(seas)
plot(seas.ts, ylab = "seasonal indices", xlab = "month")

## part b
qqnorm(resid(model1))
qqline(resid(model1))
plot(ts(resid(model1), start = c(1992,1),
        freq = 12), ylab = "Model 1 residuals")
acf(ts(resid(model1)))


### question 3
sales.ts <- ts(sales, freq=12)
sales.decmps <- decompose(sales.ts)
seasd <- sales.decmps$seasonal
seasd[1:12]
# compare to index estimates from model 1
options(digits =5)
cbind(seas, seasd[1:12])


### question 4
model <- model1
lresid<-c(rep(NA,360))
lag1resid<-lresid
lag2resid<-lresid
lag3resid<-lresid
lag1resid[2]<-resid(model)[1]
lag1resid[3]<-resid(model)[2]
lag2resid[3]<-resid(model)[1]
for(i in 4:360){
  i1<-i-1;i2<-i-2;i3<-i-3
  lag1resid[i]<-resid(model)[i1];lag2resid[i]<-resid(model)[i2]
  lag3resid[i]<-resid(model)[i3]
}

## now do residuals on this new model 
qqnorm(resid(model))
qqline(resid(model))
plot(ts(resid(model), start = c(1992,1),
        freq = 12), ylab = "Model 1 residuals")

### question 5

model<-
  lm(logSales~poly(Time,4)+fMonth+c348+s348+c432+s432,data=sales[1:192,])


