# Econ 310 Homework 2
# James Sanders


setwd("C:/Users/jmssa/OneDrive/Documents/Econ 310/Assignments")
getwd()

library(readxl)
library(dplyr)
library(sandwich)
library(miceadds)
library(lmtest)
library(stargazer)


# Problem 2, Exercise 4.26
ddk<-read_excel("DDK2011.xlsx")

ddk$scaledtotalscore<-scale(ddk$totalscore)
ddk$age <- as.numeric(ddk$agetest)
ddk$gender <- as.numeric(ddk$girl)
ddk$init_percentile <- as.numeric(ddk$percentile)
ddk$contact_teacher <- ddk$etpteacher
model1<-lm(scaledtotalscore~tracking + age + gender + contact_teacher + init_percentile,data=ddk)
model2<-miceadds::lm.cluster(data=ddk,formula=scaledtotalscore~tracking + age + gender + contact_teacher + init_percentile,cluster="schoolid")
test1<-coeftest(model1, vcov = vcov(model1))
test2<-coeftest(model2, vcov = model2$vcov)
stargazer(test1,test2,type="text",column.labels = c("OLS SEs", "Clustered SEs"),header=FALSE,dep.var.caption="")

# a) The standard error for tracking increases the most (a 3.2x increase to 0.076 from 0.024). The SE for gender increases the least (a 1.2x increase to 0.028 from 0.024).
# b) The coefficient on tracking increases to 0.173 with the inclusion of control variables. This is up from a coefficient of 0.138 in 4.54








# Problem 5, Exercise 7.28
cps09<-read_excel("cps09mar.xlsx")

# Calculate annual wages; total annual earnings divided by number of hours in year
cps09$wage<-cps09$earnings/(cps09$hours*cps09$week)
cps09$lwage<-log(cps09$wage)

cps09$experience <- (cps09$age - cps09$education - 6)
cps09$experience2 <- ((cps09$experience)^2)/100
cps09$intercept <- rep(1, length(cps09$age))

# Filters to get a dataset of white Hispanic men
white_male_hisp <- filter(cps09, female==0, race==1, hisp==1)


model1 <- lm(lwage ~ education + experience + experience2 + intercept -1, cps09)
model2 <- lm(lwage ~ education + experience + experience2 + intercept -1, white_male_hisp)
test1<-coeftest(model1, vcov = vcov(model1))
test2<-coeftest(model2, vcov = vcov(model2))

# a)
stargazer(test1,test2,type="text",column.labels = c("Everybody", "White Hispanic Men"),header=FALSE,dep.var.caption="")


# b)
generate_theta = function(beta,experience){
  # data = years of experience
  # beta = vector of B = [B1, B2, B3, B4]
  return_edu <- beta[1]
  return_exp <- beta[2] + beta[3]*(2*experience - 1)/100
  theta = return_edu/return_exp
  return(theta)
}

beta_hat <- unname(model1$coefficients)
theta_hat <- generate_theta(beta_hat, 10)
print(theta_hat)

# c)
generate_sd = function(covariance, beta, experience){
  c <- ((experience+1)^2 - experience^2)/100
  r1 <- 1/(beta[2] + beta[3]*c)
  r2 <- beta[1]/(beta[2] + beta[3]*c)^2
  r3 <- c*beta[1]/(beta[2] + beta[3]*c)^2
  r4 <- 0
  R <- t(t(c(r1, r2, r3, r4)))
  sd <- sqrt(t(R)%*%covariance%*%R)
  return(sd)
}
covariance <- vcov(model1)
se <- generate_sd(covariance, beta_hat, 10)
print(se)

# d)
generate_theta_CI=function(beta,covariance,data,alpha){
  theta_hat <- generate_theta(beta, data)
  se <- generate_sd(covariance, beta, data)
  lower_bnd <- theta_hat - qnorm(mean(alpha, 1))*se
  upper_bnd <- theta_hat + qnorm(mean(alpha, 1))*se
  CI <- c(lower_bnd, upper_bnd)
  return(CI)
}
theta_CI <- generate_theta_CI(beta_hat, covariance, 10, 0.9)
print(theta_CI)


# e)
generate_CI=function(model,data,alpha){
  point <- data.frame(education = data[1], experience = data[2], experience2 = (data[2]^2)/100, intercept = 1)
  CI <- predict(model, point, interval = "confidence", level = alpha)
  return(CI)
}
CI <- generate_CI(model1, c(12,20), 0.95)
print(CI)


# f)
generate_PI=function(model,data,alpha){
  point <- data.frame(education = data[1], experience = data[2], experience2 = (data[2]^2)/100, intercept = 1)
  PI <- predict(model, point, interval = "prediction", level = alpha)
  return(PI)
}
PI_logwage <- generate_PI(model1, c(16,5), 0.8)
PI_wage = exp(PI_logwage)
print(PI_logwage)
print(PI_wage)



# 6c)

library(tidyr)
library(latex2exp)
library(ggplot2)
set.seed(456)


# Function to simulate a Normal linear regression model
simreg2 <- function(beta,sig2,n) {
  k <- length(beta)
  sig=sqrt(sig2)
  X=matrix(c(rep(1,n),rnorm((k-1)*n)),nrow=n,byrow = TRUE)
  y <- X%*%beta + sig*(rchisq(n,1)-1)
  # forced the first x to be 1, regress without intercept
  model <- lm(y~X-1)
  # We will only return estimated beta1 and its standard error
  betahat1<-model$coefficients[2]
  betahat2<-model$coefficients[3]
  sehat1<- sqrt(diag(vcov(model))[2])
  sehat2<- sqrt(diag(vcov(model))[3])
  covar12 <- vcov(model)[2,3]
  return(c(betahat1,betahat2,sehat1,sehat2,covar12))
}


# Same parameters as before
beta <- c(.5,.6,.7,0,0)
# True sigma^2
sig2=0.64
# Run nsim regressions
nsim=1000
# Start with same sample size as before, and see if normality assumption makes sense
n=100
results2<-replicate(nsim,simreg2(beta,sig2,n))
results2<-as.data.frame(t(results2))

colnames(results2)<-c("betahat1","betahat2","sehat1","sehat2","covar12")
# t-stat for beta1 - beta2 = 0.1
standardized2<-as.data.frame((results2$betahat2-results2$betahat1-0.1)/(((results2$sehat1)^2+(results2$sehat2)^2-2*results2$covar12)^.5))
colnames(standardized2)<-"tstat"

ggplot(standardized2,aes(x=tstat))+
  geom_histogram(aes(y=..density..,color="Histogram"),bins=30,alpha=0.2)+
  geom_density(aes(y=..density..,col="Empirical Density"),lwd=1)+
  stat_function(fun=dnorm,aes(col="Normal Density"), 
                args=list(mean = mean(standardized2$tstat),sd=sd(standardized2$tstat)),lwd=1) +
  ylab("Density")+
  labs(color="Densities")+
  scale_color_brewer(palette = "Set1")+
  theme(legend.position="bottom")






