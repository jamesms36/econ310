# Econ 310 Homework 1
# James Sanders


setwd("C:/Users/jmssa/OneDrive/Documents/Econ 310/Assignments")

library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stargazer)


cps09<-read_excel("cps09mar.xlsx")

# Calculate annual wages; total annual earnings divided by number of hours in year
cps09$wage<-cps09$earnings/(cps09$hours*cps09$week)
cps09$lwage<-log(cps09$wage)


cps09$experience <- (cps09$age - cps09$education - 6)
cps09$experience2 <- (cps09$experience)^2

# Adds dummy variables for region and marital status
cps09$northeast <- as.integer(cps09$region==1)
cps09$south <- as.integer(cps09$region==3)
cps09$west <- as.integer(cps09$region==4)
cps09$married <- as.integer(cps09$marital==1 | cps09$marital==2 | cps09$marital==3)
cps09$widdiv <- as.integer(cps09$marital==4 | cps09$marital==5)
cps09$separated <- as.integer(cps09$marital==6)

# Filters to get a dataset of while hispanic man and while men
white_male_hisp <- filter(cps09, female==0, race==1, hisp==1)
white_male <- filter(cps09, female==0, race==1)

# Runs a linear regression
model1 <- lm(lwage ~ education + experience + experience2 + northeast + south + west + married + widdiv + separated, white_male_hisp)
stargazer(model1, type="text", header=FALSE)


white_male$ethnicity = "All ethnicities"
white_male_hisp$ethnicity = "Hispanic"
# Creates a dataset that includes white hispanic men twice, so that this data set can be partitioned into a subset of all white men, and a subset of white hispanic men
combined_white_male <- rbind(white_male, white_male_hisp)

# Plots conditional mean log wage for White male Hispanics and the larger White male subsamples as functions of experience
groupmeansbyed <- combined_white_male %>%
  group_by(ethnicity,experience) %>%
  summarize(meanlogincome = mean(lwage, na.rm = TRUE))
ggplot(groupmeansbyed,aes(x=experience,y=meanlogincome,col=ethnicity))+
  geom_point()+
  stat_smooth(se=FALSE,aes(col=ethnicity))+
  ylab("Log Dollars per Hour")+xlab("Years of Experience")+
  labs(col="Ethnicity:")+
  scale_colour_brewer(palette = "Set1")

# Plots the densities of log wages for White males and White male Hispanics
groupmeans <- combined_white_male %>%
  group_by(ethnicity) %>%
  summarize(meanlogincome = mean(lwage, na.rm = TRUE))
theme_set(theme_tufte())
ggplot(combined_white_male,aes(x=lwage,col=ethnicity))+
  geom_rug(alpha=0.5)+
  geom_density(adjust=1.5)+
  xlim(0,6)+
  geom_vline(data=groupmeans, aes(xintercept = meanlogincome,color = ethnicity),lty=2) +
  scale_colour_brewer(palette = "Set1")
