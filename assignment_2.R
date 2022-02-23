
#### STEP 0 : Data Import
### import
sales <- read.csv("/cloud/project/data/UsedCarSales.txt")
attach(sales)
Time <- as.numeric(1:nrow(sales))
fMonth <- as.factor(Month)
sales <- data.frame(sales,Time, fMonth)

#### QUESTION 1: Part 1:
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
     main = "Log Sales vs. time",  ylim = c(7,10))
lines(ts(log(sales_regular), start = c(1992,1), freq = 12), col = "black")


### QUESTION 2: Part 2: Form a spectral plot using just years

small_sales <- sales[1:336,]
attach(small_sales)
Time <- as.numeric(1:nrow(small_sales))
fMonth <- as.factor(Month)
sqrt(336)/2
logsales <- log(small_sales$Sales)
spectrum(logsales,span = 9)
abline(v=c(1/12,2/12,3/12,4/12,5/12,6/12),col="red",lty=2) 
abline(v=c(0.220,0.348,0.432),col="blue",lty=2)


### QUESTION 3

### model 1: multiplicative
model1 <- lm(logsales ~ Time +I(Time^2) + I(Time^3) + I(Time^4) +
               I(Time^5) + I(Time^6) + fMonth)
summary(model1)
qqnorm(resid(model1))
qqline(resid(model1))
plot(model1, which = 2)
## normality test, null is normality, can't reject, normal
shapiro.test(resid(model1))
#acf
acf(ts(resid(model1)))
plot(ts(resid(model1), start = c(1992,1), freq = 12),
     main = "Model 1")
### point 4 is an outlier

### model 2: remove outlier
## add a dummy
small_sales$obs4 <- ifelse(Sales == 2601, 1, 0)
model2 <- lm(logsales ~ Time +I(Time^2) + I(Time^3) + I(Time^4) +
               I(Time^5) + I(Time^6) + fMonth +
               small_sales$obs4)
summary(model2)
# more normal
qqnorm(resid(model2))
qqline(resid(model2))
## normality test, null is normality, reject, not normal
shapiro.test(resid(model2))
#acf
acf(ts(resid(model2)))
plot(ts(resid(model2), start = c(1992,1), freq = 12),
     main = "Model 2")

### model 3: spectral indicates calendar, so 
## include trigonometric pairs

### spectral plot does suggest calendare structure
## so we include relevant trigonometric pairs

### two calendar trigonometric pairs
c348<-cos(pi*0.696*Time);s348<-sin(pi*0.696*Time)
c432<-cos(pi*0.864*Time);s432<-sin(pi*0.864*Time)
c220<-cos(pi*0.440*Time);s220<-sin(pi*0.440*Time)

model3<-
  lm(logsales ~ Time +I(Time^2) + I(Time^3) + I(Time^4) +
       I(Time^5) + I(Time^6) + fMonth+ small_sales$obs4 +
       c348 + s348 + c432 + s432 + c220 + s220)
summary(model3)

### model 4

model4<-
  lm(logsales ~ Time +I(Time^2) + I(Time^3) + I(Time^4) +
       I(Time^5) + I(Time^6) + fMonth+ small_sales$obs4)
summary(model4)

### QUESTION 3.a


b1<- coef(model4)[1]
b2 <- coef(model4)[8:18] + b1
b3 <- c(b1,b2)
seas <- exp(b3-mean(b3))
cbind(seas)
seas.ts <- ts(seas)
plot(seas.ts, xlab = "Month", ylab = "Estimated seasonal index")

### QUESTION 3.b
resids <- resid(model4)
qqnorm(resids)
qqline(resids)
# some aspects of the trend structure have not
# been captured by the model, and 
# there is evidence of mild heteroscdasticity
plot(ts(resids))
acf(ts(resids))

## residual spectural plots
spectrum(resids, span = 9)
abline(v=c(1/12,2/12,3/12,4/12,5/12,6/12),col="red",lty=2)
abline(v=c(0.220,0.348,0.432),col="blue",lty=2)


### QUESTION 4
model5<-
  lm(logsales ~ Time +I(Time^2) + I(Time^3) + I(Time^4) +
       I(Time^5) + I(Time^6) + fMonth+ small_sales$obs4 +
       Dynamic + Dynamic*fMonth)
summary(model5)

### 4a
### yes, the addition of the new is sig, p <.001
anova(model5, model4)
summary(model5)
### 4b
b1<-coef(model5)[1]
b2<-coef(model5)[8:18]+b1
b3<-c(b1,b2)
seas1<-exp(b3-mean(b3))
b1<-coef(model5)[1]+coef(model5)[22]
b2<-coef(model5)[8:18]+coef(model5)[21:31]+b1
b3<-c(b1,b2)
seas2<-exp(b3-mean(b3))

## seas 1 is 1992:2010 and seas 2 is 2011:2019
## sales estimates increase later on in beginning of year
## then lower in summer, and more in winter.
## people started buying more in the winter after recession

## 4c
resids <- resid(model5)
qqnorm(resids)
qqline(resids)
plot(ts(resids))
acf(ts(resids))


cbind(seas, seas1, seas2)

### QUESTION 5

lresid<-c(rep(NA,359))
lag1resid<-lresid;lag2resid<-lresid
lag1resid[2]<-resid(model5)[1]
for(i in 3:336){
  i1<-i-1;i2<-i-2
  lag1resid[i]<-resid(model5)[i1];lag2resid[i]<-resid(model5)[i2]
}
lag1resid
model6<-
  lm(logsales ~ Time +I(Time^2) + I(Time^3) + I(Time^4) +
       I(Time^5) + I(Time^6) + fMonth+ small_sales$obs4 +
       Dynamic + Dynamic*fMonth + lag1resid[1:336] + lag2resid[1:336])
summary(model6)
resids <- resid(model6)
qqnorm(resids)
qqline(resids)
plot(ts(resids))
acf(ts(resids))
