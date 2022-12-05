ARTofR::xxx_title2('4330: Assignment 4')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                             4330: Assignment 4                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Libraries ---------------------------------------------------------------
library(magrittr)
library(dplyr)
library(nnet)
library(reactable)
library(reactablefmtr)

# Data set ----------------------------------------------------------------
#Reform Data set
reform = read.csv(file.choose(), header = T)
reform = reform %>% mutate(period = 3)

#Adult Education Data Set
ad_ed = read.csv(file.choose(), header = T)
ad_ed$education = factor(ad_ed$education)
ad_ed$education = relevel(ad_ed$education, ref ="HS-grad")


# Question 1 --------------------------------------------------------------
# Part A

fit <- glm(numvisit ~ reform + badh + age + educ + loginc + offset(log(period)), 
              data= reform, family = poisson)
summary(fit)

RR = exp(fit$coefficients[2])
RR

# The rate of number of doctor visits after the health care reform that took 
#place in 1997 is only 79% as high as the rate when the reform was not in 
#effect.

# Part B
RR = exp(fit$coefficients[4])
RR

# The expected number of doctor visits, as age increases by 1 year, would 
#increase by 0.4%. 

# Part C
# Estimating the 1-month rate of visits for a 30year old before the reform, 
#with 12 years of education, in bad health, and loginc = 7.6989. 
ndat <- data.frame(age = 30, reform = 0, badh = 1, educ = 12, loginc = 7.6989, 
                   period = 1)

# Predict with type=response will give the estimated rate.
pred.rate <- predict(fit, newdata = ndat, type="response")
pred.rate

#The estimated number of doctor visits for that specific individual would be 
# between 2-3 visits per month. 

# Part D
ndat <- data.frame(age = 30, reform = 0, badh = 1, educ = 12, loginc = 7.6989, 
                   period = 12)
pred.rate1 <- predict(fit, newdata = ndat, type="response")
pred.rate1

#The estimated number of doctor visits for that specific individual would be 
# between 29-30 visits per year. 

# Part E



# Question 2 --------------------------------------------------------------

#Handwritten notes

# Question 3 --------------------------------------------------------------

fit = glm(numvisit ~ age, data = reform, family = poisson)
summary(fit)

yi = reform$numvisit
xi = reform$age

log.lik.pr = function(par){
  b0 = par[1]
  b1 = par[2]
  
  lam = exp(b0 + b1*xi)
  
  -sum(dpois(yi, lambda = lam, log = TRUE))
}

opt.pr = optim(par = list(b0 = 1, b1 = 1), fn = log.lik.pr)
opt.pr$par

# Question 4 --------------------------------------------------------------
#Part A
fit =  multinom(education~ age + sex, data = ad_ed)
fit

#Part B
coef = summary(fit)$coefficients
stderrs = summary(fit)$standard.errors
stderrs


Bachodds = data.frame(Coefficients = c("Constant", "Age", "Sex (Male)"),
                      Estimates = round(c(coef[1,1], coef[1,2], coef[1,3]),
                                        digits = 4),
                      StandardErrors = round(c(stderrs[1,1], stderrs[1,2], 
                                               stderrs[1,3]), digits = 4))

add_title(table = reactable(Bachodds), title = "Bachelors vs. HS-Grads")

Docodds = data.frame(Coefficients = c("Constant", "Age", "Sex (Male)"),
                      Estimates = round(c(coef[2,1], coef[2,2], coef[2,3]),
                                        digits = 4),
                      StandardErrors = round(c(stderrs[2,1], stderrs[2,2], 
                                               stderrs[2,3]), digits = 4))

add_title(table = reactable(Docodds, bordered = TRUE), 
          title = "Doctrate vs. HS-Grads")

Masodds = data.frame(Coefficients = c("Constant", "Age", "Sex (Male)"),
                     Estimates = round(c(coef[3,1], coef[3,2], coef[3,3]),
                                       digits = 4),
                     StandardErrors = round(c(stderrs[3,1], stderrs[3,2], 
                                              stderrs[3,3]), digits = 4))

add_title(table = reactable(Masodds, bordered = TRUE), 
          title = "Masters vs. HS-Grads")



