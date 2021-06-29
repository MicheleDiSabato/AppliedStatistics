# Problem n.3
# Different chemicals are tested to predict aquatic toxicity towards Daphnia Magna, a small planktonic crustacean.
# The file toxicity.txt contains the values of 6 molecular descriptors (C1,. . .,C6) of the 100 tested chemicals and
# a measure of toxicity (tox), which is the concentration that causes death in 50% of test population of Daphnia
# Magna over a test duration of 48 hours.
# a) Formulate a linear regression model for the toxicity, as a function of all the other variables. Report the estimates
# of the parameters and verify the assumptions of the model.
# b) Predict the toxicity of a new chemical characterized by the following molecular descriptors: C1=100, C2=0.7,
# C3=2, C4=4, C5=1.4, C6=3. Provide a pointwise estimate and an interval of level 95%.
# c) Perform a variable selection through a Lasso method, by optimizing via cross-validation the parameter controlling the penalization (??) within the range [0.01; 1]. Report the optimal ?? and the significant coefficients.
# d) Answer point (b) using the reduced model obtained at point (c).

data <- read.table('toxicity.txt', header=TRUE)
head(data)
dim(data)

# a) Formulate a linear regression model for the toxicity, as a function of all the other variables. Report the estimates
# of the parameters and verify the assumptions of the model.

fit = lm(tox ~., data = data)
summary(fit)

par(mfrow=c(2,2))
plot(fit)
dev.off()

shapiro.test(fit$residuals)


# fit = lm(tox ~., data = data[-64,])
# summary(fit)
# 
# par(mfrow=c(2,2))
# plot(fit)
# dev.off()
# 
# shapiro.test(fit$residuals)


fit$coefficients
(sum(fit$residuals)^2)/fit$df.residual


# b) Predict the toxicity of a new chemical characterized by the following molecular descriptors: C1=100, C2=0.7,
# C3=2, C4=4, C5=1.4, C6=3. Provide a pointwise estimate and an interval of level 95%.

xnew = data.frame(C1=100, C2=0.7, C3=2, C4=4, C5=1.4, C6=3)
predict(fit, xnew, level=0.99, interval="prediction")

# c) Perform a variable selection through a Lasso method, by optimizing via cross-validation the parameter controlling the penalization (??) within the range [0.01; 1]. Report the optimal ?? and the significant coefficients.

x=model.matrix(tox ~., data = data)[,-1]
y=data$tox
grid=seq(0.01,1,by=0.001)
set.seed(1)
cv.lasso <- cv.glmnet(x,y,alpha=1,lambda=grid)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso

fit.lasso <- glmnet(x,y, lambda = grid)
coef.lasso <- predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
coef.lasso 

# d) Answer point (b) using the reduced model obtained at point (c).

xnew = data.frame(C1=100, C2=0.7, C3=2, C4=4, C5=1.4, C6=3)

attach(data)
yhat <- cbind(rep(1,100), C1, C2, C3, C4, C5, C6)%*%coef.lasso
detach(data)

yhat_previsto = cbind(1, xnew$C1,xnew$C2, xnew$C3, xnew$C4, xnew$C5, xnew$C6)%*%coef.lasso


















































































































