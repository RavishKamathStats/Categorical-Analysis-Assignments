#Name: Ravish Kamath  
#student number: 213893664

library(MASS)
data('birthwt')
View(birthwt)
cont_tb = table(birthwt$smoke, birthwt$low)
rownames(cont_tb) = c("non-smoker", "smoker")
colnames(cont_tb) = c('birth Weight > 2.5', 'birth Weight < 2.5')
cont_tb
#Based off the contingency table, we can definitely see that there is a higher count for non-smokers that are associated
#with greater 2.5kg birth weight. However these are just counts and does not truly reflect an association with non-smokers 
#nd births greater than 2.5kg

chisq.test(cont_tb)
#If we are using our alpha to be 0.05, then based off the p-value we have it to be 0.03958, which means
#we would reject our hypothesis that smoking status during pregnancy is associated with low birth weight
