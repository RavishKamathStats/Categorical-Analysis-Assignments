######### 43330 Assignment 2 ###########
#### Question 1 ####
A2df = read.csv('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 4330/3. Assessments/Categorical Analysis Assignments/Assignments/Data/acath2.csv',
                header = TRUE)
View(A2df)
# Part A
install.packages('sjmisc')
library(sjmisc)
summary(A2df)
#As we can see off the summary of our data set, we can see that the median of the variable, cholest, would be 224.5

A2df$cholest = dicho(A2df$cholest, dich.by = 'median')
summary(A2df$cholest)


#Since we are splitting it based of its median, as we can see based of the summary there are 1129 in both levels. 
# 0 represents when the cholesterol is below 224.5 and 1 represents when the cholesterol is above 224.5

odds_tb = table(A2df$cholest, A2df$sigdz)
colnames(odds_tb) = c("non-signficant coronary", "significant coronary")
rownames(odds_tb) = c('chol < 224.5', 'chol > 224.5')
odds_tb
n = sum(odds_tb)
chol_great= sum(odds_tb[2,])
chol_low = sum(odds_tb[1,])
#P(significant coronary disease | high cholesterol)
sigcoron_high_prob = odds_tb[2,2]/ chol_great

#P(significant coronary disease | low cholesterol)
sigcoron_low_prob = odds_tb[1,2]/ chol_low


odds_ratio = (sigcoron_high_prob/(1 - sigcoron_high_prob))/
             (sigcoron_low_prob/(1 - sigcoron_low_prob))
odds_ratio

#With the odds ratio being greater than one, the individuals with higher cholesterol
# will have a higher odds of having significant coronary disease.

# Part B
odds_tb = table(A2df$cholest, A2df$tvdlm)
colnames(odds_tb) = c("non-severe coronary", "severe coronary")
rownames(odds_tb) = c('chol < 224.5', 'chol > 224.5')
odds_tb
chol_great= sum(odds_tb[2,])
chol_low = sum(odds_tb[1,])

#P(severe coronary disease | high cholesterol)
sevcoron_high_prob = odds_tb[2,2]/ chol_great

#P(severe coronary disease | low cholesterol)
sevcoron_low_prob = odds_tb[1,2]/ chol_low


odds_ratio = (sevcoron_high_prob/(1 - sevcoron_high_prob))/
  (sevcoron_low_prob/(1 - sevcoron_low_prob))
odds_ratio

# With the odds ratio being greater than one, the individuals with higher cholesterol
# will have a higher odds of having severe coronary disease. 

# Part C
#We cannot estimate the risk ratio for either significant nor sever coronary disease.
#It is clear that this is a retrospective study/case control since we are already 
#sampling relative to the outcome, which would be coronary heart disease. Furthermore, a quick Google 
#search shows us that coronary heart disease is quite common, and hence the probability of coronary heart
#disease is greater than 5%. Hence we cannot use the odds ratio to estimate the risk ratio. 




#### Question 2 ####
A2df = read.csv('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 4330/3. Assessments/Categorical Analysis Assignments/Assignments/Data/acath2.csv')
View(A2df)

#Part A
fit <- lm(A2df$cholest~ age + sex, data=A2df)
s.fit <- summary(fit)
s.fit

# Our model will be: cholesterol = 224.96743 + 0.03886 Age + 9.78549 Sex. 

# Beta 0 interpretation: Given that the age of the individual is 0 year old and male, 
#their cholesterol level will be 224.96743

# Beta 1 interpretation: Given that sex is a constant, for every increase in unit of Age, the cholesterol level would
#increase by 0.03886mg.

#Beta 2 interpretation: Given that age is constant, then cholesterol level would increase by 9.78549 if they are female

#Part B
confint(fit, level=0.95)
#Age C.I.
#Our confidence interval for alpha = 0.05 would be (-0.1828807, 0.2605961)


#Sex C.I.
#Our confidence interval for alpha = 0.05 would be (5.258151, 14.3181654)
#Part C
new.x <- data.frame(age= 50, sex= 1)
predict(fit, newdata = new.x)

#Based of the model we would predict that their cholesterol level would be 236.6958mg for a 50 year old female.

#Part D
new.x <- data.frame(age= 10, sex= 0)
predict(fit, newdata = new.x)

#Based of the model we would predict that their cholesterol level would be 225.356 for a 10 year old male. 
# Yes I would be less confident in this prediction. Firstly, most of these patients are experiencing chest pain,
#which would be common within older people, rather than someone who is 10 year's old. Younger people tend to have a much healthier 
#heart do you to younger age. To have a 225mg cholesterol level, would seem too unrealistic for that individual,
#unless they have an obesity issue.




