
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
plot(sales.ts, xlab = "time", ylab = "Log Sales", main = "Sales vs. time", col = "red")
plot(ts(lsales, start(c(1992,1), freq = 12)), xlab = "time", ylab = "Log Sales", main = "Sales vs. time", col = "black")

## contraction
salescontraction<-
  c(rep(NA,111),sales_vector[112:119], rep(NA,73) ,sales_vector[193:210],
    rep(NA,128),sales_vector[339:340],rep(NA,20))
salescontraction
sales_regular<-
  c(sales_vector[1:111],rep(NA,8), sales_vector[120:192] ,rep(NA,18),
   sales_vector[211:338],rep(NA,2),sales_vector[341:360])
sales_regular
test<- cbind(salescontraction, sales_regular)
## if you plot the black part first, wouldn't happen, plot most vertical spread first
# then add others to that range
plot(ts(salescontraction,start = c(1992,1), freq = 12), 
     col = "red", lwd = 2, ylab = "Sales", 
     main = "Sales vs. time", ylim = c(20000,80000))
lines(ts(sales_regular, start = c(1992,1), freq = 12), col = "black")
## log
plot(ts(log(salescontraction),start = c(1992,1), freq = 12), 
     col = "red", lwd = 2, ylab = "Log Sales", 
     main = "Sales vs. time", ylim = c(10,12))
lines(ts(log(sales_regular), start = c(1992,1), freq = 12), col = "black")


### question 2
model1 <- lm(lsales~poly(Time,4) + fMonth + 
               c348
               # + s348 + c432 
             + s432)
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
plot(model1, which = 2)
shapiro.test(resid(model1))
acf(ts(resid(model1)))
plot(ts(resid(model1), start = c(1992,1), freq = 12),
        main = "Model 1")


## add a dummy
sales$obs339 <- ifelse(Sales == 73901, 1, 0)
sales$obs340 <- ifelse(Sales == 64383, 1, 0)
sales$obs341 <- ifelse(Sales == 68436, 1, 0)
model2 <- lm(sales$logSales~poly(Time,4) + fMonth + 
               c348
             # + s348 + c432 
             + s432 +
               sales$obs339+ sales$obs340 + sales$obs341)
summary(model2)
## part a
b1 <- coef(model2)[1]
b2 <- coef(model2)[6:16]+b1
b3<- c(b1,b2)
seas <- exp(b3-mean(b3))
seas
seas.ts <- ts(seas)
plot(seas.ts, ylab = "seasonal indices", xlab = "month")

## part b
qqnorm(resid(model2))
qqline(resid(model2))
plot(model2, which = 2)
shapiro.test(resid(model2))
acf(ts(resid(model2)))
plot(ts(resid(model2), start = c(1992,1), freq = 12),
        main = "Model 2")


### question 3
sales.ts <- ts(logSales, freq=12)
sales.decmps <- decompose(sales.ts, type = "mult")
seasdmult1 <- sales.decmps$seasonal
seasdmult <- seasdmult1[1:12]/prod(seasdmult1[1:12]) ^(1/12)
prod(seasdmult)
seasdmult
# compare to index estimates from model 1
options(digits =5)
cbind(seas, seasdmult[1:12])
## since the estimation uses a 
plot(ts(sales.decmps$random[7:360]))
acf(ts(sales.decmps$random[7:300]),36)
plot(ts(seas), lty = 1, lwd=2, col= "red", main = "seas")
plot(ts(seasdmult[1:12]), lty = 2, lwd = 2, col = "green", main = "decomposed")


### question 4
model2 <- lm(sales$logSales~poly(Time,4) + fMonth + 
               c348
             + s348 + c432 
             + s432 +
               sales$obs339+ sales$obs340 + sales$obs341)
summary(model2)
model<- model2
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

model3 <- lm(sales$logSales~poly(Time,4) + fMonth + 
               c348+
             + s432 +
               sales$obs339+ sales$obs340 + sales$obs341 +
               lag1resid + lag2resid +lag3resid)
summary(model3)

## now do residuals on this new model 
qqnorm(resid(model3))
qqline(resid(model3))
plot(ts(resid(model3), start = c(1992,1),
        freq = 12), ylab = "Model 3 residuals", main = "Model 3")
acf(ts(resid(model3)))

### question 5

##### part A
modelA<-
  lm(logSales~poly(Time,4)+fMonth+c348+
       # s348+c432+
       s432,data=sales[1:192,])
summary(modelA)
qqnorm(resid(modelA), main = "Normal QQ plot - A")
qqline(resid(modelA))
plot(modelA, which = 2)
shapiro.test(resid(modelA))
acf(ts(resid(modelA)))

s432+sales$obs96+ sales$obs74+ sales$obs75
## add a dummy
sales$obs96 <- ifelse(Sales == 37046, 1, 0)
sales$obs74 <- ifelse(Sales == 27947, 1, 0)
sales$obs75 <- ifelse(Sales == 30501, 1, 0)
modelA<-
  lm(logSales~poly(Time,4)+fMonth+c348 + sales[1:192,]$obs96 +
       sales[1:192,]$obs74 + sales[1:192,]$obs75,
       # s348+c432+
       data=sales[1:192,])
summary(modelA)
## part a
b1 <- coef(modelA)[1]
b2 <- coef(modelA)[6:16]+b1
b3<- c(b1,b2)
seas <- exp(b3-mean(b3))
seas
seas.ts <- ts(seas)
plot(seas.ts, ylab = "seasonal indices", xlab = "month", main = "Model A : 1992-2007")
plot(ts(resid(modelA), start = c(1992,1), freq = 12),
     main = "Model A")


##### part B
modelB<-
  lm(logSales~poly(Time,4)+fMonth+c348+
       # s348+c432+
       s432,data=sales[193:336,])
summary(modelB)
qqnorm(resid(modelB), main = "Normal QQ plot B")
qqline(resid(modelB))
plot(modelB, which = 2)
shapiro.test(resid(modelB))
acf(ts(resid(modelB)))

## add a dummy
sales$obs242 <- ifelse(Sales == 44174, 1, 0)
sales$obs232 <- ifelse(Sales == 45586, 1, 0)
sales$obs326 <- ifelse(Sales == 51284, 1, 0)
modelB<-
  lm(logSales~poly(Time,4)+fMonth+c348 + sales[193:336,]$obs242 +
       sales[193:336,]$obs232 + sales[193:336,]$obs326,
     # s348+c432+
     data=sales[193:336,])
summary(modelB)
## part a
b1 <- coef(modelB)[1]
b2 <- coef(modelB)[6:16]+b1
b3<- c(b1,b2)
seas_B <- exp(b3-mean(b3))
seas_B
seas.ts <- ts(seas_B)
plot(seas.ts, ylab = "seasonal indices", xlab = "month", main = "Model 8: 2008-2021")
plot(ts(resid(modelB), start = c(2008,1), freq = 12),
     main = "Model B")

cbind(seas, seas_B)
