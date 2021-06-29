# Problem n.3
# The file bikes.txt reports the data on public bikes rented in 50 days, the corresponding weather data (temperature
#                                                                                                        [
#                                                                                                          ???C] and wind [m/s]) and holiday information. The dataset reports, for each day, the number of bikes rented (b),
# the mean temperature (t), the mean wind speed (w) and the holiday information (Holiday/No holiday). Consider
# the following model
# bi,g = ??0,g + ??1,gti + ??2,gwi + eps;
# where g ??? {1, 2} indicates the group according to the holiday information (g = 1 for holiday, g = 2 for no holiday)
# and eps ??? N(0; ??
#           2
# ).
# a) Estimate the parameters of the model and report the estimated values.
# b) Verify the needed model assumptions and perform two statistical tests at level 5% to verify if
# - there is statistical evidence of a dependence of the mean number of bikes rented on weather information;
# - there is statistical evidence of a dependence of the mean number of bikes rented on holiday information.
# Report the hypothesis and the p-values of the test performed.
# c) Comment on possible model weaknesses and, if needed, reduce the model, and update the parameter estimates.
# d) Based on the model obtained at point (c), provide a pointwise estimate and a prediction interval (probability
#                                                                                                      95%) for the number of public bikes rented on a holiday with mean temperature 2???C and mean wind speed 3
# m/s.


# a) Estimate the parameters of the model and report the estimated values.

data <- read.table('bikes.txt', header=TRUE)
head(data)
D = ifelse(data$day=="No Holiday",1,0)
D
n = dim(data)[1]
n
data = cbind(data[,1:3], d = D)
head(data)
data

fit <- lm(bike_count  ~ mean_temp + mean_wind + d + d:mean_temp + d:mean_wind, data=data)
summary(fit)

sigma2 = sum(fit$residuals^2)/fit$df.residual
sigma2



# b) Verify the needed model assumptions and perform two statistical tests at level 5% to verify if
# - there is statistical evidence of a dependence of the mean number of bikes rented on weather information;
# - there is statistical evidence of a dependence of the mean number of bikes rented on holiday information.
# Report the hypothesis and the p-values of the test performed.

par(mfrow=c(2,2))
plot(fit)
dev.off()
shapiro.test(fit$residuals)

C1 <- rbind(c(0,1,0,0,0,0),
            c(0,0,1,0,0,0),
            c(0,0,0,1,0,0),
            c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
b1 = c(0,0,0,0,0)
linearHypothesis(fit, C1, b1)
summary(fit)

C2 <- rbind(c(0,0,0,1,0,0),
            c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
b2 = c(0,0,0)
linearHypothesis(fit, C2, b2)



# c) Comment on possible model weaknesses and, if needed, reduce the model, and update the parameter estimates.

summary(fit)
par(mfrow=c(2,2))
plot(fit)
dev.off()

fit2 <- lm(bike_count  ~ mean_temp + mean_wind + d + d:mean_temp + 
             d:mean_wind, data=data[c(-50,-32,-10),])
summary(fit2)
par(mfrow=c(2,2))
plot(fit2)
dev.off()
shapiro.test(fit2$residuals)

summary(fit)

C2 <- rbind(c(0,0,0,0,1,0),
            c(0,0,0,0,0,1))
b2 = c(0,0)
linearHypothesis(fit, C2, b2)


fit3 <- lm(bike_count  ~ mean_temp + mean_wind + d, data=data)
summary(fit3)










































































