# Econ 310 Homework 5
# James Sanders


# Problem 1

library(pdfetch)
library(xts)
library(ggplot2)
library(ggthemes)
library(forecast)
library(gridExtra)
library(readxl)
library(vars)
library(tidyr)
library(stargazer)
library(knitr)
library(texreg)
library(pander)
library(plm)
theme_set(theme_tufte())
setwd("C:/Users/jmssa/OneDrive/Documents/Econ 310/Assignments")

fedfunds <- pdfetch_FRED(c('FEDFUNDS'))
fedfunds_mothly <- to.monthly(fedfunds)
fedfunds_close <- fedfunds_mothly$fedfunds.Close

dsg10 <-pdfetch_FRED(c('DGS10'))
dsg10_monthly <- to.monthly(dsg10)
dsg10_close <- dsg10_monthly$dsg10.Close

fred <- merge(fedfunds_close, dsg10_close)
fred <-fred["1963/2020-11"]
fred <- na.omit(fred)
colnames(fred) <- c("FEDFUNDS", "DSG10")
fred$FEDFUNDS_diff <- diff(fred$FEDFUNDS)
fred$DSG10_diff <- diff(fred$DSG10)


g1 <- ggplot(fred,aes(x=Index,y=FEDFUNDS))+
  geom_line(col="darkblue")+xlab("")+ylab("Federal Funds Rate")
g2 <- ggplot(fred,aes(x=Index,y=DSG10))+
  geom_line(col="darkred")+xlab("")+ylab("10 year treasury interest rate")
g3 <- ggplot(fred,aes(x=Index,y=FEDFUNDS_diff))+
  geom_line(col="darkblue")+xlab("")+ylab("Monthly change in Federal Funds Rate")
g4 <- ggplot(fred,aes(x=Index,y=DSG10_diff))+
  geom_line(col="darkred")+xlab("")+ylab("Monthly change in 10 year treasury interest rate")
grid.arrange(g1, g2, g3, g4, ncol=2)

ggtsdisplay(as.vector(fred$FEDFUNDS_diff), main="Monthly change in Federal Funds Rate")
ggtsdisplay(as.vector(fred$DSG10_diff), main="Monthly change in 10 year treasury interest rate")

arma_fedfunds <- auto.arima(fred$FEDFUNDS_diff,max.p = 24, max.q = 24, ic="aic")
arma_dsg10 <- auto.arima(fred$DSG10_diff,max.p = 24, max.q = 24, ic="aic")
summary(arma_fedfunds)
summary(arma_dsg10)


# Problem 2

six_month_rate <- pdfetch_FRED(c('DTB6'))
six_month_rate_mothly <- to.monthly(six_month_rate)
six_month_rate_close <- six_month_rate_mothly$six_month_rate.Close
six_month_rate_close <- na.omit(six_month_rate_close)
six_month_rate_close <- six_month_rate_close["1963/2020-11"]
colnames(six_month_rate_close) <- c("CTB6")

fred$CTB6 <- six_month_rate_close
fred$CTB6_diff <- diff(fred$CTB6)

varmodel <- VAR(na.omit(merge(fred$FEDFUNDS_diff, fred$DSG10_diff, fred$CTB6_diff)),type="const",lag.max = 3, ic = "AIC")
stargazer(varmodel$varresult$FEDFUNDS_diff,varmodel$varresult$DSG10_diff,varmodel$varresult$CTB6_diff,
          title="Vector Autoregression",out.header=FALSE,df=FALSE,header=FALSE,
          dep.var.caption = "",dep.var.labels = "",font.size = "footnotesize",
          column.labels = c("FEDFUNDS", "DSG10","CTB6"), type="text")

imp_12 <- irf(varmodel, impulse = "FEDFUNDS_diff", response = "DSG10_diff", boot =TRUE,n.ahead = 10)
imp_13 <- irf(varmodel, impulse = "FEDFUNDS_diff", response = "CTB6_diff", boot =TRUE,n.ahead = 10)
imp_21 <- irf(varmodel, impulse = "DSG10_diff", response = "FEDFUNDS_diff", boot =TRUE,n.ahead = 10)
imp_23 <- irf(varmodel, impulse = "DSG10_diff", response = "CTB6_diff", boot =TRUE,n.ahead = 10)
imp_31 <- irf(varmodel, impulse = "CTB6_diff", response = "FEDFUNDS_diff", boot =TRUE,n.ahead = 10)
imp_32 <- irf(varmodel, impulse = "CTB6_diff", response = "DSG10_diff", boot =TRUE,n.ahead = 10)
plot(imp_12)
plot(imp_13)
plot(imp_21)
plot(imp_23)
plot(imp_31)
plot(imp_32)

# An increase in the federal funds rate causes the DGS10 and CTB6 rates to immediately increase significantly for a few months.
# This is exactly the behavior we would expect from increasing the federal funds rate. 
# An increase in the DSG10 or CTB6 causes a delayed upward spike in the federal funds rate roughly two monts afterwards, which can be seen as a policy response. 
# An increase in the DSG10 causes the CTB6 to immediately increases for a few months, while an increase in the CTB6 does not have a significant effect on the DSG10, but a small downward dip can be seen four months later. 



# Problem 3

fred$logDSG10 <- log(fred$DSG10)
fred$logCTB6 <- log(fred$CTB6)

adf0_logdsg10 <-summary(ur.df(fred$logDSG10, selectlags = "AIC"))
adf0_logctb6 <-summary(ur.df(fred$logCTB6, selectlags = "AIC"))

adf1_logdsg10 <-summary(ur.df(na.omit(diff(fred$logDSG10)), selectlags = "AIC"))
adf1_logctb6 <-summary(ur.df(na.omit(diff(fred$logCTB6)), selectlags = "AIC"))

testtable1<-t(c(adf0_logdsg10@teststat, adf0_logctb6@teststat, adf1_logdsg10@teststat, adf1_logctb6@teststat))
colnames(testtable1)<-c("log(DSG10)", "log(CTB6)", "First Difference log(DSG10)", "First Difference log(CTB6)")
kable(testtable1,caption="Test Statistics for ADF Unit Root Tests for log data")
kable(adf1_logdsg10@cval,caption="ADF test critical values")
# Both are integrated of order I(0)

coint_data1 <- merge(fred$logDSG10, fred$logCTB6)
summary(ca.po(coint_data1,demean = "trend"))
# Fail to reject Null. Therefore, these are not cointegrated



adf0_dsg10 <-summary(ur.df(fred$DSG10, selectlags = "AIC"))
adf0_ctb6 <-summary(ur.df(fred$CTB6, selectlags = "AIC"))

adf1_dsg10 <-summary(ur.df(na.omit(diff(fred$DSG10)), selectlags = "AIC"))
adf1_ctb6 <-summary(ur.df(na.omit(diff(fred$CTB6)), selectlags = "AIC"))

testtable2 <- t(c(adf0_dsg10@teststat, adf0_ctb6@teststat, adf1_dsg10@teststat, adf1_ctb6@teststat))
colnames(testtable2) <- c("DSG10", "CTB6", "First Difference DSG10", "First Difference CTB6")
kable(testtable2,caption="Test Statistics for ADF Unit Root Tests for level data")
kable(adf1_dsg10@cval,caption="ADF test critical values")
# Both are integrated of order I(0)

coint_data2 <- merge(fred$DSG10, fred$CTB6)
summary(ca.po(coint_data2,demean = "trend"))
# Fail to reject Null. Therefore, these are not cointegrated



# Problem 6

AB1991 <- read_excel("AB1991.xlsx")
AB1991$year <- as.factor(AB1991$year)
AB1991$id <- as.factor(AB1991$id)
AB1991 <- pdata.frame(AB1991, index=c("id", "year"))

model1<-pgmm(k ~ lag(k) | lag(k, 2), AB1991, effect="twoways",transformation="d",model="onestep",robust=TRUE)
model1r <- coeftest(model1, vcov=vcovHC(model1,type="HC0",cluster="group"))
model2<-pgmm(k ~ lag(k) | lag(k, 2), AB1991, effect="twoways",transformation="ld",model="twosteps",robust=TRUE)
model2r <- coeftest(model2, vcov=vcovHC(model2,type="HC0",cluster="group"))

stargazer(model1r,model2r,header=FALSE,digits=4,
          column.labels = c("Arello-Bond One-Step", "Bundell-Bond One-Step"),
          title="Exercise 17.15", type="text")
