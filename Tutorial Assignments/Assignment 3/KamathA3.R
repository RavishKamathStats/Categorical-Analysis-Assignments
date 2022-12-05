df = read.table('/Users/ravishkamath/Desktop/University/2. York Math/1 MATH/1. Statistics /MATH 4330/3. Assessments/Tutorial Assignments/Assignment 3/icudat.txt', header =  TRUE)

# Part A
fit <- glm(sta ~ age + sex + pco, data=df, family=binomial)
summary(fit)

# Part B
betas = as.vector(fit$coefficients)
exp(betas)


#Based of the output above:
#Beta 1: Since our odds ratio is 1.02855677, the odds of death increases by a factor of 1.0286 for every increase in age,
#given that sex and pH initial blood gasses are held constant.

#Beta 2: Since our odds ratio is 1.00609558, the odds of death increases by a factor of 1.0061 for a female individual, 
#given that age and pH initial blood gasses are held constant.

#Beta 3: Since our odds ratio is 0.77186667, the odds ratio of death decreases by a factor of 1 - 0.7719 = 0.2281 for every 
#increase in pH from initial blood gasses, given that age and sex are constant. 
