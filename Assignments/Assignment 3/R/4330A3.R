#### 4330 Assignment 2 ####
#### Question 1 ####
CO2df = read.table('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 4330/3. Assessments/Assignments/Assignment 3/Data/CO2.txt'
                   , header = TRUE)

View(CO2df)

# Part A
fit = lm(CO ~ Traffic + Wind, data = CO2df)
summary(fit)

# Beta 0 is 1.274461
# Beta 1 is 0.018290
# Beta 2 is 0.174747

confint(fit) 

# Beta 0 C.I. = (0.8624, 1.6865)
# Beta 1 C.I. = (0.0155, 0.0211)
# Beta 2 C.I. = (0.0567, 0.2928)

# Part B
par(mfrow = c(2,2))
#Residual vs. Fitted
plot(fit$fitted.values, fit$residuals, pch=19, col="darkgreen", cex=0.5, xlab = 'Fitted Values',
     ylab = 'Residuals')
abline(h=0, lty=2, lwd=2, col="red")

#Residual vs. Predictors
plot(CO2df$Traffic, fit$residuals, pch=19, col="darkgreen", cex=0.5, xlab = 'Traffic',
     ylab = 'Residuals')
abline(h=0, lty=2, lwd=2, col="red")
plot(CO2df$Wind, fit$residuals, pch=19, col="darkgreen", cex=0.5, xlab = 'Wind', 
     ylab = 'Residuals')
abline(h=0, lty=2, lwd=2, col="red")

#Normality Plot
par(mfrow = c(1,2))
library("car")
hist(fit$residuals, xlab = 'Residuals')
qqPlot(scale(fit$residuals), main = 'QQ-Plot', ylab = 'Residuals'); abline(0,1)



# Part C
res.fit <- lm(abs(fit$residuals) ~ fit$fitted.values)
w <- 1/(res.fit$fitted.values^2)
wls.fit <- lm(CO ~ Traffic + Wind, data=CO2df, weights=w)
summary(wls.fit)

confint(wls.fit)

#### Question 2 ####
df = read.csv('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 4330/3. Assessments/Assignments/Assignment 3/Data/acath2.csv'
              , header = TRUE)
View(df)

#Part A
fit = glm(sigdz ~ cholest, data=df, family=binomial)
summary(fit)
beta1_OR = exp(fit$coefficients[2])
beta1_OR

#Since our odds ratio is 1.006246, the odds of having significant coronary disease by 
#cardiac catheterization increases by a factor of 1.006246 as your cholesterol increases.
CI = confint(fit, level = 0.95)
new_CI = c(exp(CI[2,1]), exp(CI[2,2]))
new_CI

# Since the confidence interval does not overlap 1, then we can say there is an association between
# significant coronary disease and cholesterol. 


#Part B
n.dat <- data.frame(cholest = 400)
predict(fit, newdata = n.dat, type="response")

#Subsetting patients that have greater than 400mg cholesterol.
indices = which(df$cholest > 400)
df_sub_geq400 = df[indices,] 


#Part C
#I would say that the expression in (b) cannot be used to accurately predict 
#significant coronary disease for a general population because the data set used,
# were for patients that already had some prior chest pain. These are two different populations of interest,
#hence, it is not a good predictive model.
  
  
  
#Part D
fit2 = glm(sigdz ~ cholest + age + sex, data = df, family = binomial)
summary(fit2)

betas = as.vector(fit2$coefficients)
exp(betas)

# vec = as.matrix(confint(fit2))
# vec = vec[3:4,]
# exp(vec)

#Beta 1 OR would be 1.0090463
#Beta 2 OR would be 1.0724942
#Beta 3 OR would be 0.1231459

# To identify whether Age and Sex are confounders for significant coronary diseases and cholesterol level,
# we should see a difference in our estimated coefficient for cholesterol. When comparing the two models,
# fit and fit2, we can see that the $\beta_1$ coefficient for cholesterol has changed, though not by much,
# shows that Age and Sex are a potential confounder to the model.


#Part E
library("pROC")
pred.val1 = predict(fit)
pred.val2 = predict(fit2)
par(mfrow = c(1,2))

auc(df$sigdz, pred.val1, plot=TRUE, auc.polygon=TRUE, 
    auc.polygon.col="lightblue", asp=FALSE, main = 'Model with Cholesterol')

auc(df$sigdz, pred.val2, plot=TRUE, auc.polygon=TRUE, 
    auc.polygon.col="lightblue", asp=FALSE, main = 'Model with Cholest, Age and Sex')

#I would say that the model with the added predictors, age and sex, would be the better model.
# If we see based of the ROC curves, the 2nd model to  be closer to the top left corner of the plot. 
#This signifies that we are getting higher value for sensitivity as well as specificity. Furthermore,
#the AUC for the second model is 0.7887 which is better than the previous model with only 0.5887.