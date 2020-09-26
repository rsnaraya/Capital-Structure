## Clean up and read the source data
rm(list=ls())
dat=readxl::read_xls("D://OneDrive - Flutura Business Solutions Pvt Ltd//Personal//Paper//Logistics regression//across sectors ratios 26-Sep-2018.xls",sheet="Automobile")

# have a look at the data
head(dat)
str(dat)
class(dat)
summary(dat)

## replace NA char in business risk to na so that we can do missing value treatement
library(dplyr)
dat <- na_if(dat,"NA")

## which colums have null values and if missing values have to be treated
colSums(is.na(dat))
which(is.na(dat$`Debt service capacity`))

## make Business risk revised as Numeric
dat$`BusinessRisk-revised`<- as.numeric(dat$`BusinessRisk-revised`)

## remove col Tobin's Q - no value - hence being treated as logical
dat$`Tobin's Q ratio` <- NULL
summary(dat)

##### TREATING MISSING VALUES
# impute the missing values with mean/mode/median
library(Hmisc)
datMean <- dat
datMean$"Growth Opportunity"[is.na(datMean$"Growth Opportunity")] <- mean(dat$"Growth Opportunity", na.rm = T)
which(is.na(datMean$"Growth Opportunity"))
datMean$"Debt service capacity"[is.na(datMean$"Debt service capacity")] <- mean(dat$"Debt service capacity", na.rm = T)
which(is.na(datMean$"Debt service capacity"))
datMean$`Business Risk`[is.na(datMean$`Business Risk`)] <- mean(dat$`Business Risk`, na.rm = T)
which(is.na(datMean$`Business Risk`))
str(datMean)
summary(datMean)


# install.packages("TTR")
# install.packages("quantmod")
library(TTR)
library(quantmod)

# read the Equity excel file that has the name of stock and stock symbol
equity=read.csv("D://OneDrive - Flutura Business Solutions Pvt Ltd//Personal//Paper//Logistics regression//Equity.csv")
head(equity$Security.Name)
head(datMean$Company)
match(toupper(datMean$Company),equity$Security.Name) # convert to uppercase in Company since equity table has in Upper case

equity$Security.Id[match(toupper(datMean$Company),equity$Security.Name)] #Subset by square brackets and get the symbol from Equity file
datMean$Symbol = equity$Security.Id[match(toupper(datMean$Company),equity$Security.Name)]
## check how many null values in Symbol column - where the names have not directly matched
colSums(is.na(datMean)) # 281 rows have Nulls

missingSymbol_rownum = which(is.na(datMean$Symbol),arr.ind=FALSE) # corresponding row number for missing Symbols
# data.frame(unique(datMean$Company[which(is.na(datMean$Symbol),arr.ind=FALSE)]))


# missingSymbol$CompanyName = data.frame(missingSymbol) # gives the 17 mis-matching company names
# missingSymbol$CompanyName = unique(datMean$Company[which(is.na(datMean$Symbol),arr.ind=FALSE)])
# missingSymbol1 = pmatch(missingSymbol,equity$Security.Name,nomatch=NA,duplicates.ok=FALSE)
# equity$Security.Id[missingSymbol1]
# missingSymbol$Symbol=equity$Security.Id[missingSymbol1]
# missingSymbol

i=1
for (i in missingSymbol_rownum) {
  if (datMean$Company[i] == "Balkrishna Industries Ltd.") {
    datMean$Company[i] = "BALKRISHNA INDUSTRIES LTD.-$"
  } else if (datMean$Company[i] == "Force Motors Ltd.") {
    datMean$Company[i] = "FORCE MOTORS LTD.-$"
  } else if (datMean$Company[i] == "J K Tyre & Inds. Ltd.") {
    datMean$Company[i] = "JK TYRE & INDUSTRIES LTD."
  } else if (datMean$Company[i] == "M R F Ltd.") {
    datMean$Company[i] = "MRF LTD."
  } else if (datMean$Company[i] == "S M L Isuzu Ltd.") {
    datMean$Company[i] = "SML ISUZU LIMITED"
  } else if (datMean$Company[i] == "T V S Motor Co. Ltd.") {
    datMean$Company[i] = "TVS MOTOR COMPANY LTD."
  } else if (datMean$Company[i] == "T V S Srichakra Ltd.") {
    datMean$Company[i] = "TVS SRICHAKRA LTD.-$"
  } else if (datMean$Company[i] == "Tube Investments Of India Ltd.") {
    datMean$Company[i] = "Tube Investments of India Ltd"
  } else if (datMean$Company[i] == "Minda Industries Ltd.") {
    datMean$Company[i] = "MINDA INDUSTRIES LTD.-$"
  } else if (datMean$Company[i] == "Endurance Technologies Ltd.") {
    datMean$Company[i] = "Endurance Technologies Ltd"
  } else print("No missing values")
  datMean$Symbol[i] = equity$Security.Id[match(datMean$Company[i],equity$Security.Name)]
}

## check if any null values exists
colSums(is.na(datMean)) # no missing values
summary(datMean)
library(moments)
range(datMean$`Busines risk`)
var(datMean[5:20])
var(datMean$`log of sales`)

skewness(datMean[5:20])
kurtosis(datMean[5:20])

## rename the col names so that its easier for analysis
FinancialLev = datMean$`Financial leverage`
Sales = datMean$`log of sales`
Earningrate = datMean$`Earning rate`
Capitalemployed = datMean$`Capital employed`
NDTS = datMean$`Non debt tax shield`
Divpayout = datMean$`Div payout`
DebtServiceCapacity = datMean$`Debt service capacity`
AssetValue = datMean$`Collateral value of assets`
ProfitabilityRatio = datMean$`Profitability Ratio (EBIT/Total Assets)`
BusinessRisk = datMean$`Busines risk`
GrowthOpportunity = datMean$`Growth Opportunity`
Taxation = datMean$Taxation
Uniqueness = datMean$Uniqueness
Exports = datMean$Exports
Tangibility = datMean$Tangibility
Age = datMean$Age


# do a logistic regression on target variable (Financial Leverage)
library(Hmisc)
length(datMean)
cor(datMean[5:20])
#install.packages("PerformanceAnalytics")
#library(PerformanceAnalytics)
#chart.Correlation(datMean[5:20])

#plot(Sales, FinancialLev)
#splom(datMean[5:20])

# logistic rehression for all variables
library(forecast)
Logisticregressionfit=glm(FinancialLev~Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datMean)
summary(Logisticregressionfit)

#install.packages("ggplot2")
#require(ggplot2)
#ggplot(data=datMean,mapping = aes(x=x,y=FinancialLev))+geom_point()

#x <- c(Sales,Earningrate,Taxation,Uniqueness,Capitalemployed,NDTS,Exports,Divpayout,DebtServiceCapacity,AssetValue,Tangibility,ProfitabilityRatio,Age,BusinessRisk,GrowthOpportunity)
#data <- data.frame(x,FinancialLev)

#p <- plotlyM(data,x = ~x)%>%

# logistic regresion for variables that are important in above log regression
Logisticregressionfit2=glm(FinancialLev~Taxation+Exports+Divpayout+AssetValue+Tangibility,data=datMean)
summary(Logisticregressionfit2)

#============================
# Residual analysis
#============================
pred1=predict(Logisticregressionfit) # gives log-odds of hat(p_i)
pred2=predict(Logisticregressionfit,type="response") # gives hat(p_i)
##predict(fit1,interval="confidence")

res1=residuals(Logisticregressionfit,type="deviance")
res2=residuals(Logisticregressionfit,type="pearson")

# Multicollinearity
car::vif(Logisticregressionfit)
car::vif(Logisticregressionfit2)


# diagnostics
# x- outliers
barplot(hatvalues(Logisticregressionfit))

# linearity
plot(FinancialLev,pred1)


# concordance & discordance
cor(FinancialLev,pred2,method="kendall")

#=============================
### Linearity of IVs
#============================

#fit1=glm(Size~Admin+Mark+Admin*Mark+states,family="binomial")

# automatic variable selection
null = glm(FinancialLev~1)
full = Logisticregressionfit

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = glm(aicmodel$terms)
summary(finalaic)


#=============================
### Confusion matrix
#============================

pred_prob = pred2
yhat = numeric(length(pred_prob))
yhat[pred_prob>=0.2935]=1
#yhat[pred_prob>=0.75]=1


table(FinancialLev, yhat)


library(funModeling)
dat1$FinancialLev1 = FinancialLev
dat1$pred_prob1 = pred_prob
ff=gain_lift(data=dat1,score="pred_prob1",target="FinancialLev1")

#==================================
# ROC Curve
library(pROC)
#install.packages("TTR")
library(TTR)
library(ROCR)
summary(dat1$pred_prob1)
describe(dat1$pred_prob1)
summary(dat1$FinancialLev1)
describe(dat1$FinancialLev1)
rr= ROC(dat1$FinancialLev1,dat1$pred_prob1)
plot(rr)
auc(rr)

#==================================
### Hosmer-Lemeshow Goodness of Fit
### How well our model fits depends on the difference between the model and the observed data.  One approach for binary data is to implement a Hosmer Lemeshow goodness of fit test.
#==================================

#install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(FinancialLev, fitted(Logisticregressionfit))
hoslem.test(FinancialLev, fitted(Logisticregressionfit2))
##Our model appears to fit well because we have no significant difference between the model and the observed data (i.e. the p-value is above 0.05)

#==================================
### Hausman test
#==================================

#install.packages("plm")
library(plm)
#install.packages("stargazer")
library(stargazer)

# declare our data to be a panel data
datMean.p <- pdata.frame(datMean,index=c("Company","Year"))

# run a panel model
# fixed effects - within each company
fixedeffects1 <- plm(FinancialLev~Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datMean.p,model="within")
summary(fixedeffects1)
stargazer(fixedeffects1,type='text')

fixedeffects2 <- plm(FinancialLev~Sales+Taxation+Uniqueness+NDTS+Tangibility+ProfitabilityRatio+BusinessRisk+GrowthOpportunity,data=datMean.p,model="within")
stargazer(fixedeffects2,type='text')
summary(fixedeffects2)

# random effects - across each company
# randomeffects1 <- plm(FinancialLev~Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+BusinessRisk+GrowthOpportunity+Age,data=datMean.p,model="random")
randomeffects1 <- plm(FinancialLev~Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility,data=datMean.p,model="random")
summary(randomeffects1)
stargazer(randomeffects1,type='text')

randomeffects2 <- plm(FinancialLev~Sales+Taxation+Uniqueness+NDTS+Tangibility+ProfitabilityRatio+BusinessRisk+GrowthOpportunity,data=datMean.p,model="random")
stargazer(randomeffects2,type='text')
summary(randomeffects2)

# run the Hausman tests 
phtest(fixedeffects1,randomeffects1)
phtest(fixedeffects2,randomeffects2) # p = 0.5906; Null Hypothesis is that preferred model is random effect vs alternate is fixed effect. Since p>0.05, hence random effect model recommended

# regular OLS (Pooled) using PLM
pool <- plm(FinancialLev~Sales+Taxation+Uniqueness+NDTS+Tangibility+ProfitabilityRatio+BusinessRisk+GrowthOpportunity,data=datMean,index=c("Company","Year"),model="pooling")
summary(pool)

#against all variables
pool1 <- plm(FinancialLev~Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datMean,index=c("Company","Year"),model="pooling")
summary(pool1)

# Bruesch - Pagan Lagrange Multiplier for random effects. NUll Hypothesis is no panel effect (ie: OLS is better)
plmtest(pool, type=c("bp"))
plmtest(pool1,type=c("bp"))
bptest(Logisticregressionfit)

# test for cross-sectional dependence
pcdtest(fixedeffects2,test = c("lm"))
pcdtest(fixedeffects2,test = c("cd"))

# test for serial correlation
pbgtest(fixedeffects2)
pbgtest(fixedeffects1)

library(lmtest)
bgtest(fixedeffects2)
bgtest(fixedeffects1)

##perform Durbin Watson test
library(car)
durbinWatsonTest(Logisticregressionfit2)
durbinWatsonTest(Logisticregressionfit)

# test for stationary/unit roots - Dickey-Fuller tests. Null hypothesis is that series has unit root (non-stationary)
library(tseries)
Panel.set <- plm.data(datMean,index=c("Company","Year"))
adf.test(Panel.set$Year, k=2)

# do a time series on the above variables - based on Financial Leverage being the dependent variable
# TSA for mean
#
#training Sample
T=nrow(datMean)
stock.test=tail(datMean,86) # 43 companies * 2 years
stock.train=ts(datMean[1:(T-86),],start = c(1995,1), frequency = 43) 
# there is a problem here as the rows are not repeating across companies - also we need company as part of this
head(stock.train)
head(stock.test)


install.packages("glmtree")
library(glmtree)
Logisticregressionfittree=glmtree(FinancialLev~Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datMean)
print(Logisticregressionfittree)

# get the closing stock price based on the symbol/stock ticker
# Stock price for the year - take this as Mar-ending
#install.packages("stochvol")
#install.packages("qrmtools")
library(stochvol)
library(qrmtools)

#install.packages("TTR")
#install.packages("quantmod")
library(TTR)
library(quantmod)

yearend = paste(datMean$Year,"03","31",sep="-")
datMean$SymbolBO = paste(datMean$Symbol,"BO",sep=".")


if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
library(BatchGetSymbols)

i=1
startdate=paste(datMean$Year[i],"03","31",sep="-")
lastdate=paste(datMean$Year[nrow(datMean)],"04","01",sep="-")
freqdata = 'yearly'
print(startdate)
print(lastdate)
print(freqdata)

tickers = unique(datMean$SymbolBO)

l.out <- BatchGetSymbols(tickers = tickers,
                         first.date = startdate,
                         last.date = lastdate,
                         thresh.bad.data = 0.1,
                         freq.data = freqdata,
                         do.fill.missing.prices = TRUE,
                         do.complete.data = TRUE,
                         cache.folder = file.path(tempdir(),'BGS_Cache'))
print(l.out$df.tickers$price.close)

format(as.Date(l.out$df.tickers$ref.date[1], format="%Y/%m/%d"),"%Y")
l.out$df.tickers$ticker[1]

a <- data.frame("SymbolBO" = l.out$df.tickers$ticker,"Year" = format(as.Date(l.out$df.tickers$ref.date, format="%Y/%m/%d"),"%Y"),"Close.price" = l.out$df.tickers$price.close, stringsAsFactors=FALSE)

datstock = merge(datMean,a, by=c("SymbolBO","Year"),all.x = TRUE)

colSums(is.na(datstock))
colSums(is.na(l.out$df.tickers))
summary(l.out$df.tickers$price.close)
summary(datstock)

datstock$`Closing Stock price` <- NULL
datstock$Symbol <- NULL
summary(datstock)

# drop rows with Null values since stock prices are not available for all stocks
datstock <- na.omit(datstock)
summary(datstock)


## rename the col names so that its easier for analysis
FinancialLev = datstock$`Financial leverage`
Sales = datstock$`log of sales`
Earningrate = datstock$`Earning rate`
Capitalemployed = datstock$`Capital employed`
NDTS = datstock$`Non debt tax shield`
Divpayout = datstock$`Div payout`
DebtServiceCapacity = datstock$`Debt service capacity`
AssetValue = datstock$`Collateral value of assets`
ProfitabilityRatio = datstock$`Profitability Ratio (EBIT/Total Assets)`
BusinessRisk = datstock$`Busines risk`
GrowthOpportunity = datstock$`Growth Opportunity`
Taxation = datstock$Taxation
Uniqueness = datstock$Uniqueness
Exports = datstock$Exports
Tangibility = datstock$Tangibility
Age = datstock$Age
ClosingPrice = datstock$Close.price


# logistic regression for all variables
library(forecast)
Logisticregressionfitstock=glm(ClosingPrice~FinancialLev+Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk,data=datstock)
summary(Logisticregressionfitstock)

# logistic regresion for variables that are important in above log regression
Logisticregressionfit2stock=glm(ClosingPrice~Sales+Earningrate+DebtServiceCapacity+BusinessRisk,data=datstock)
summary(Logisticregressionfit2stock)

#============================
# Residual analysis
#============================
pred1=predict(Logisticregressionfitstock) # gives log-odds of hat(p_i)
pred2=predict(Logisticregressionfitstock,type="response") # gives hat(p_i)
##predict(fit1,interval="confidence")

res1=residuals(Logisticregressionfitstock,type="deviance")
res2=residuals(Logisticregressionfitstock,type="pearson")

# Multicollinearity
car::vif(Logisticregressionfitstock)
car::vif(Logisticregressionfit2stock)


# diagnostics
# x- outliers
barplot(hatvalues(Logisticregressionfitstock))

# linearity
plot(ClosingPrice,pred1)


# concordance & discordance
cor(ClosingPrice,pred2,method="kendall")

#=============================
### Linearity of IVs
#============================

# automatic variable selection
null = glm(ClosingPrice~1)
full = Logisticregressionfitstock

aicmodel = step(null,scope=list(lower=null,upper=full),direction="forward",k=2)
finalaic = glm(aicmodel$terms)
summary(finalaic)


#=============================
### Confusion matrix
#============================

pred_prob = pred2
yhat = numeric(length(pred_prob))
yhat[pred_prob>=0.2935]=1
#yhat[pred_prob>=0.75]=1


table(ClosingPrice, yhat)


library(funModeling)
dat1$ClosingPrice1 = FinancialLev
dat1$pred_prob1 = pred_prob
ff=gain_lift(data=dat1,score="pred_prob1",target="FinancialLev1")

#==================================
# ROC Curve
library(pROC)
#install.packages("TTR")
library(TTR)
library(ROCR)
summary(dat1$pred_prob1)
describe(dat1$pred_prob1)
summary(dat1$FinancialLev1)
describe(dat1$FinancialLev1)
rr= ROC(dat1$FinancialLev1,dat1$pred_prob1)
plot(rr)
auc(rr)

#==================================
### Hosmer-Lemeshow Goodness of Fit
### How well our model fits depends on the difference between the model and the observed data.  One approach for binary data is to implement a Hosmer Lemeshow goodness of fit test.
#==================================

install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(ClosingPrice, fitted(Logisticregressionfitstock))
hoslem.test(ClosingPrice, fitted(Logisticregressionfit2stock))
##Our model appears to fit well because we have no significant difference between the model and the observed data (i.e. the p-value is above 0.05)

#==================================
### Hausman test
#==================================

#install.packages("plm")
library(plm)
#install.packages("stargazer")
library(stargazer)

# declare our data to be a panel data
datstock.p <- pdata.frame(datstock,index=c("Company","Year"))

# run a panel model
# fixed effects - within each company
fixedeffects1 <- plm(ClosingPrice~FinancialLev+Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datstock.p,model="within")
summary(fixedeffects1)
stargazer(fixedeffects1,type='text')

# random effects - across each company
#randomeffects1 <- plm(ClosingPrice~FinancialLev+Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+BusinessRisk+GrowthOpportunity+Age,data=datstock.p,model="random",random.method = "walhus")
randomeffects1 <- plm(ClosingPrice~Sales+Uniqueness+Capitalemployed+NDTS+Exports+DebtServiceCapacity+AssetValue+Tangibility+BusinessRisk,data=datstock.p,model="random")
summary(randomeffects1)
stargazer(randomeffects1,type='text')

# run the Hausman tests 
phtest(fixedeffects1,randomeffects1) # p = 0.5906; Null Hypothesis is that preferred model is random effect vs alternate is fixed effect. Since p>0.05, hence random effect model recommended

# regular OLS (Pooled) using PLM
pool <- plm(ClosingPrice~FinancialLev+Sales+Taxation+Uniqueness+NDTS+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datstock,index=c("Company","Year"),model="pooling")
summary(pool)

#against all variables
pool1 <- plm(ClosingPrice~FinancialLev+Sales+Earningrate+Taxation+Uniqueness+Capitalemployed+NDTS+Exports+Divpayout+DebtServiceCapacity+AssetValue+Tangibility+ProfitabilityRatio+Age+BusinessRisk+GrowthOpportunity,data=datstock,index=c("Company","Year"),model="pooling")
summary(pool1)

# Bruesch - Pagan Lagrange Multiplier for random effects. NUll Hypothesis is no panel effect (ie: OLS is better)
library(lmtest)
plmtest(pool, type=c("bp"))
plmtest(pool1,type=c("bp"))
bptest(Logisticregressionfitstock)

# test for cross-sectional dependence
pcdtest(fixedeffects1,test = c("lm"))
pcdtest(fixedeffects1,test = c("cd"))

# test for serial correlation
pbgtest(fixedeffects1)

bgtest(fixedeffects1)

##perform Durbin Watson test
library(car)
durbinWatsonTest(Logisticregressionfit2stock)
durbinWatsonTest(Logisticregressionfitstock)

# test for stationary/unit roots - Dickey-Fuller tests. Null hypothesis is that series has unit root (non-stationary)
library(tseries)
Panel.set <- plm.data(datstock,index=c("Company","Year"))
adf.test(Panel.set$Year, k=2)

## ======
# try k-means clustering
## ======
install.packages("tidyverse")
library(tidyverse)
library(cluster)
install.packages("factoextra")
library(factoextra)
install.packages("gridExtra")
library(gridExtra)
kmeans2 <- kmeans(datstock.p, centers = 2, nstart = 25)
str(kmeans2)
fviz_cluster(kmeans2, data = datstock)
fviz_nbclust(kmeans2,data=datstock)


#now lets compute the accuracy
#install.packages("DMwR")
library(DMwR)
actuals <- dat$'BusinessRisk-revised'[is.na(dat$'BusinessRisk-revised')]
predictmean <- rep(mean(datMean$'BusinessRisk-revised', na.rm=T), length(actuals))
regr.eval(actuals, predictmean)
## could not get this - need to check

# kNN imputation
library(dplyr)
?knnImputation
str(dat)
datnumeric <- select_if(dat,is.numeric)
str(datnumeric)
summary(datnumeric)
datknnnumeric <- knnImputation (datnumeric)  # perform knn imputation
anyNA(datknnnumeric)
#now lets compute the accuracy
predictknn <- datknnnumeric[is.na(dat$Taxation),"knn"]
regr.eval(actuals, predictknn)

## rpart
library(rpart)
datrpart <- rpart(Taxation ~ ., data=datnumeric[!is.na(datnumeric$Taxation),],method="anova",na.action=na.omit)
summary(datrpart)
anyNA(datrpart)

predictrpart<- predict(datrpart, datnumeric[is.na(datnumeric$Taxation),])

## check the accuracy
install.packages("Metrics")
library(Metrics)
fn<-ifelse (is.na (datnumeric$Taxation) ==TRUE, datnumeric$Taxation, 0)
original<-datnumeric$Taxation [is.na (fn)]

mae_rpart<-mae(original,predictrpart)

rmse_rpart<-rmse(original,predictrpart)
mape_rpart<-mape(original,predictrpart)

Accuracy_rpart<-cbind(mae=mae_rpart,rmse=rmse_rpart,mape=mape_rpart)

## mice
install.packages("mice")
library(mice)
?mice
miceMod <- mice(datnumeric)
datmice <-complete(miceMod)
anyNA(datmice)
