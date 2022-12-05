lungcancer = read.csv(file.choose(), header = T)
head(lungcancer)

#Part 1
lungcancer$pop = lungcancer$pop*6

lungcancer.fit = glm(dead~age+offset(log(pop)), 
                     data=lungcancer, family = poisson)
summary(lungcancer.fit)

#Part 2
lungcancer.fit_coef = lungcancer.fit$coefficients

RR = exp(lungcancer.fit_coef)

RR[6]
confint(lungcancer.fit)
#The incident of death from lung cancer for 65-69 year-old is 7.0791 times higher than the rate for 40-44 year-old. Furthermore, 
#Since the confidence interval for 65-69 year-old is not overlapping 1, we can say that there is an association between being 40-44 year old
#death from lung cancer.

#Part 3
ndat <- data.frame( age="65-69", pop=1000)

pred.rate <- predict(lungcancer.fit, newdata = ndat, type="response")
pred.rate

#The prediction for death from lung cancer would be 39.54421 for 65-69 year-old per 1000 person-years.
