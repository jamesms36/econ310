# Econ 310 Homework 3
# James Sanders


setwd("C:/Users/jmssa/OneDrive/Documents/Econ 310/Assignments")

library(readxl)
library(dplyr)
library(sandwich)
library(miceadds)
library(lmtest)
library(stargazer)
library(kableExtra)
library(boot)

set.seed(864)


cps09<-read_excel("cps09mar.xlsx")

cps09$wage<-cps09$earnings/(cps09$hours*cps09$week)
cps09$lwage<-log(cps09$wage)
cps09$experience <- (cps09$age - cps09$education - 6)
cps09$experience2 <- ((cps09$experience)^2)/100
cps09$married1 <- as.numeric(cps09$marital == 1)
cps09$married2 <- as.numeric(cps09$marital == 2)
cps09$married3 <- as.numeric(cps09$marital == 3)
cps09$widowed <- as.numeric(cps09$marital == 4)
cps09$divorced <- as.numeric(cps09$marital == 5)
cps09$separated <- as.numeric(cps09$marital == 6)
cps09$intercept <- rep(1, length(cps09$age))

white_male_hisp <- filter(cps09, female==0, race==1, hisp==1)


# a)
model1 <- lm(lwage ~ education + experience + experience2 + married1 +
               married2 + married3 + widowed + divorced + separated + intercept -1, white_male_hisp)

stargazer(model1,type="text",header=FALSE)


# b)
cps09$combined_mar1_wid <- cps09$married1 + cps09$widowed
cps09$combined_div_sep <- cps09$divorced + cps09$separated

white_male_hisp <- filter(cps09, female==0, race==1, hisp==1)

model2 <- lm(lwage ~ education + experience + experience2 + combined_mar1_wid +
               married2 + married3 + combined_div_sep + intercept -1, white_male_hisp)
stargazer(model2,type="text",header=FALSE)

# c)
#??




# 4)

set.seed(864)

simreg <- function(n, alpha, beta) {
  theta = exp(beta)
  x = runif(n, 0, 1)
  e = rnorm(n, 0, 1)
  y = rep(alpha, n) + beta*x + e
  
  model <- lm(y ~ x)
  beta_hat <- as.numeric(model$coefficients)[2]
  theta_hat <- exp(beta_hat)
  
  V_beta <- vcovHC(model, type = "HC0")[2,2]
  se_beta <- as.numeric(sqrt(V_beta))
  R <- exp(beta_hat)
  se_theta <- sqrt(R*V_beta*R)
  
  t_beta <- (beta_hat - beta)/se_beta
  t_theta <- (theta_hat - theta)/se_theta
  
  return(c(beta_hat, theta_hat, t_beta, t_theta))
}

results <- replicate(1000, simreg(50, 0, 1))
results<-as.data.frame(t(results))
colnames(results)<-c("beta_hat","theta_hat","t_beta","t_theta")

e_beta_hat <- mean(results$beta_hat)
e_theta_hat <- mean(results$theta_hat)

p_t_beta <- mean(as.numeric(results$t_beta > 1.645))
p_t_theta <- mean(as.numeric(results$t_theta > 1.645))























# Problem 6

mrw <-read_excel("MRW1992.xlsx")
mrw <- as.data.frame(mrw)
countries <- mrw$country
data <- mrw[,2:10]
mrw <- data.frame(countries, lapply(data, function(x)as.numeric(as.character((x)))))

# These are the estimators that were outlined in section 8.12 of the textbook
mrw$lndY <- log(mrw$Y85) - log(mrw$Y60)
mrw$lnY60 <- log(mrw$Y60)
mrw$lnI <- log(mrw$invest/100)
mrw$lnG <- log(mrw$pop_growth/100+0.05)
mrw$lnS <- log(mrw$school/100)

# a)

# Unrestricted regression
model_ols <- lm(lndY ~ lnY60 + lnI + lnG + lnS, mrw)
beta_ols <- model_ols$coefficients

V_ols <- vcovHC(model_ols, type = "HC2")
se_ols <- sqrt(diag(V_ols))


# Jacknife
n <- dim(mrw)[1]

jack_calc <- function(i){
  model <- lm(lndY ~ lnY60 + lnI + lnG + lnS, data = mrw[-i,])
  theta <- model$coefficients[3] + model$coefficients[4] + model$coefficients[5]
  return(c(as.numeric(model$coefficients), theta))
}

jack_data <- sapply(1:n, jack_calc)
jack_data <- as.data.frame(t(jack_data))
colnames(jack_data) <- c("intercept", "ln60", "lnI", "lnG", "lnS", "theta")
jack_se <- sapply(1:6, function(i)sqrt((n-1)*var(jack_data[,i])*(n-1)/n))

# Bootstrap
set.seed(864)

boot_calc <- function(data, indices){
  model <- lm(lndY ~ lnY60 + lnI + lnG + lnS, data = data[indices,])
  theta <- model$coefficients[3] + model$coefficients[4] + model$coefficients[5]
  return(c(as.numeric(model$coefficients), theta))
}


boot_data <- boot(mrw, boot_calc, R=10000)
boot_se <- sapply(1:6, function(i)sd(boot_data$t[,i]))

# Comparison

compare <- t(rbind(se_ols, jack_se[1:5], boot_se[1:5]))

rownames(compare) <-  c("intercept SE", "ln60 SE", "lnI SE", "lnG SE", "lnS SE")
colnames(compare) <- c("Asymptotic", "Jacknife", "Bootstrap")

kable(compare,digits=3)%>%
  column_spec(2:4,width = "1in")



# b)


theta_ols <- beta_ols[3] + beta_ols[4] + beta_ols[5]
var_ols_theta <- V_ols[3,3] + V_ols[4,4] + V_ols[5,5] + 2*V_ols[3,4] + 2*V_ols[3,5] + 2*V_ols[4,5]
se_ols_theta <- sqrt(var_ols_theta)

theta_jack <- mean(jack_data[,6])
se_theta_jack <- jack_se[6]

theta_boot <- mean(boot_data$t[,6])
se_theta_boot <- boot_se[6]

theta_estimates <- c(theta_ols, theta_jack, theta_boot)
theta_se <- c(se_ols_theta, se_theta_jack, se_theta_boot)
compare_theta <- rbind(theta_estimates, theta_se)
rownames(compare_theta) <- c("Theta", "SE(Theta)")
colnames(compare_theta) <- c("Asymptotic", "Jacknife", "Bootstrap")
kable(compare_theta,digits=3, caption="Theta estimates and standard errors")%>%
  column_spec(2:4,width = "1in")


# c)
conf <- boot.ci(boot_data, conf = 0.95, type = c("perc", "bca"), index = c(6,6))
citable<-matrix(rep(0,4),nrow=2)
citable[1,] <- c(conf$percent[4:5])
citable[2,] <- c(conf$percent[4:5])

rownames(citable) <- c("Percentile", "BC")
colnames(citable) <- c("low", "high")
kable(citable,digits=3,caption="Bootstrap Confidence Intervals")%>%
  column_spec(2:3,width = ".6in")%>%
  add_header_above(c("","$\\\\theta$"=2),escape=F)



