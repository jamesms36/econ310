# Econ 310 Homework 4
# James Sanders


# Problem 4

library(readxl)
library(AER)
library(stargazer)
library(kableExtra)
library(matlib)
library(MASS)

# Part a
setwd("C:/Users/jmssa/OneDrive/Documents/Econ 310/Assignments")

ajr_data <- read_excel("AJR2001.xlsx")

ols_model <- lm(loggdp ~ risk, ajr_data)
stargazer(ols_model, title = "OLS Regression", type = "text")

reduced_model <- lm(risk ~ logmort0, ajr_data)
stargazer(reduced_model, title = "Reduced form Regression", type = "text")

twoSLS_model <- ivreg(loggdp ~ risk | . -risk +logmort0, data = ajr_data)
stargazer(twoSLS_model, title = "2SLS Regression", type = "text")



# The 2SLS estimate is different than the reported value by 0.01

# Part b

ols_se_homo <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "const"))[2,2]
ols_se_hetero <- coeftest(ols_model, vcov = vcovHC(ols_model, type = "HC0"))[2,2]

red_se_homo <- coeftest(reduced_model, vcov = vcovHC(reduced_model, type = "const"))[2,2]
red_se_hetero <- coeftest(reduced_model, vcov = vcovHC(reduced_model, type = "HC0"))[2,2]

twoSLS_se_homo <- coeftest(twoSLS_model, vcov = vcovHC(twoSLS_model, type = "const"))[2,2]
twoSLS_se_hetero <- coeftest(twoSLS_model, vcov = vcovHC(twoSLS_model, type = "HC0"))[2,2]

se_homo_results <- c(ols_se_homo, red_se_homo, twoSLS_se_homo)
se_hetero_results <- c(ols_se_hetero, red_se_hetero, twoSLS_se_hetero)
se_results <- cbind(se_homo_results, se_hetero_results)
colnames(se_results) <- c("Homoskedastic", "Heteroskedastic HC0")
rownames(se_results) <- c("OLS", "Reduced", "2SLS")

se_results

# They used the homoskedastic errors estimate


# Part f

ols_model2 <- lm(loggdp ~ risk + latitude + africa, data = ajr_data)
stargazer(ols_model2, type = "text")
# Yes. In this model, both "latitude" and "africa" are significant predictors. 

# Part g

twoSLS_model2 <- ivreg(loggdp ~ risk + latitude + africa | . -risk + logmort0, data = ajr_data)
stargazer(twoSLS_model2, type = "text")
# Under the instrumental model, "latitude" and "africa" are no longer significant predictors. 

# Part i

ajr_data$logmort2 <- (ajr_data$logmort0)^2
reduced_model3 <- lm(risk ~ logmort0 + logmort2, data = ajr_data)
stargazer(reduced_model3, title = "Modified Reduced Regression", type = "text")

twoSLS_model3 <- ivreg(loggdp ~ risk + latitude + africa| . -risk +logmort0 + logmort2, data = ajr_data)
stargazer(twoSLS_model3, type = "text")

# Part j

summary(twoSLS_model3, diagnostics = TRUE)$diagnostics

# The instruments are strong using the Stock-Yogo test




# Problem 6

# Part a

n <- length(ajr_data$loggdp)
Y <- ajr_data$loggdp
X <- cbind(ajr_data$risk, ajr_data$latitude, ajr_data$africa, rep(1,n))
Z <- cbind(ajr_data$logmort0, ajr_data$logmort2, ajr_data$latitude, ajr_data$africa, rep(1,n))
omega <- (1/n)*sum(reduced_model3$residuals^2)*(t(Z)%*%Z)
W <- ginv(omega)
Q <- 1/n*t(Z)%*%X

beta_gmm <- ginv(t(X)%*%Z%*%W%*%t(Z)%*%X) %*% (t(X)%*%Z%*%W%*%t(Z)%*%Y)
var_bgmm <- ginv(t(Q)%*%W%*%Q)
se_bgmm <- sqrt(diag(var_bgmm))
bgmm_results <- cbind(beta_gmm, se_bgmm)
colnames(bgmm_results) <- c("Beta", "SE")
bgmm_results

# Part b

J <- t(Y-X%*%beta_gmm) %*% Z %*% W %*% t(Z) %*% (Y - X%*%beta_gmm)

# Part c





