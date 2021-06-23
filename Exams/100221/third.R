# Problem n.3
# The file landslides.txt collects data on slow moving landslides. The dataset reports, for 80 monitored landslides,
# the average downslope displacement rate [mm/year], the annual precipitations [mm], the hardness of subsurface
# rocks [in Mohs' scale], the quantity of coarse debris [g/cm3] and the quantity of fine debris [g/cm3].
# a) Formulate a linear regression model for the average downslope displacement rate, as a function of all the other
# variables. Report the model and its parametrization, together with the estimates of all its parameters. Verify
# the model assumptions.

landslides <- read.table('landslides.txt', header=TRUE)
head(landslides)
fit <- lm(rate ~ ., data=landslides)
summary(fit)
x11()
par(mfrow=c(2,2))
plot(fit)
dev.off()
shapiro.test(fit$residuals)
summary(fit)
sum(fit$residuals^2)/fit$df.residual

# b) Based on appropriate test(s), reduce the model and update the model parameters.

summary(fit)
fit2 <- lm(rate ~ rain + coarse + fine, data=landslides)
x11()
par(mfrow=c(2,2))
plot(fit2)
dev.off()
shapiro.test(fit2$residuals)
summary(fit2)

# c) Using model (b), test the hypothesis according to which the effect on the displacement rate of the increase of 1
# g/cm3 of coarse debris is two times the effect of the increase of 1 g/cm3 of fine debris. Report the hypotheses
# and the p-value of the test performed. Possibly propose a new constrained model and estimate its parameters.

# test if beta_2 = 2*beta_3
C = rbind(c(0,0,1,-2))
b = 0
linearHypothesis(fit2, C, b)

fit3 <- lm(rate ~ rain + I(2*coarse + fine), data=landslides)
x11()
par(mfrow=c(2,2))
plot(fit3)
dev.off()
shapiro.test(fit3$residuals)
summary(fit3)

# d) A new landslide has been identified on a site with annual precipitations 700 mm, hardness of subsurface rocks
# 5, quantity of coarse debris 10 g/cm3 and quantity of fine debris 8 g/cm3.
# Using the last model, compute a
# pointwise estimate and a confidence interval of level 99% for the mean displacement rate of the new landslide.

x.new = data.frame(rain=700,hardness=5,coarse=10,fine=8)
predict(fit3, x.new, interval = 'confidence',level=0.99)
beta=fit3$coefficients

beta[1] + beta[2]*x.new$rain + (2*x.new$coarse+x.new$fine)*beta[3]



































































