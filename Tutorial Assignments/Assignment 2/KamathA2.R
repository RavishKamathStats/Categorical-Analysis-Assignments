dat = read.csv('/Users/ravishkamath/Downloads/fev.csv', header = TRUE)
View(dat)

#Part 1
fit <- lm(FEV ~ Age + Sex + Height + Smoker, data = dat)
s.fit <- summary(fit)
s.fit

#Part 2
# 1 for Male, 0 for Female

#Beta 0 interpretation: Holding Age, Sex to be female, Smoking status to non smoker and Height as a constant, the expected value for FEV 
#would be -4.544220.

# Beta 1 interpretation: Holding Sex, Smoking Status and Height as a constant, A one unit increase in Age would be 
#0.065509 increase in forced expiatory volume in liters.

#Beta 2 interpretation: Holding Age, Smoking status and Height constant, FEV would increase by 0.157103 given that 
#their sex is male. 

#Beta 3 interpretation: Holding Age, Smoking status, and Sex constant, A one unit increase in Height would be 
#0.104199 increase in forced expiatory volume in liters. 

#Beta 4 interpretation: Holding Age and Height constant and Sex is Female, FEV would increase by 0.087246 given that 
#their non smoking status.

#Part 3
t.test (dat$FEV , dat$Age)
t.test (dat$FEV ~ dat$Sex)
t.test(dat$FEV, dat$Height)
t.test(dat$FEV ~ dat$Smoker)

#Using 0.05 as our alpha

#Age Variable
#H_0: Beta_1 = 0 and H_a Beta_1 not equal to 0 
#We would reject H_0 and say that there is evidence to show that there is an association between age and expiatory volume
#in litres (FEV)

#Sex Variable
#H_0: Beta_2 = 0 and H_a Beta_2 not equal to 0 
#We would reject H_0 and say that there is evidence to show that there is an association between sex and expiatory volume
#in litres (FEV)

#Height Variable
#H_0: Beta_3 = 0 and H_a Beta_3 not equal to 0 
#We would reject H_0 and say that there is evidence to show that there is an association between height and expiatory volume
#in litres (FEV)

#Smoking Status Variable
#H_0: Beta_4 = 0 and H_a Beta_4 not equal to 0 
#We would not reject H_0 and say that there is no evidence to show that there is an association between smoking status and expiatory volume
#in litres (FEV)