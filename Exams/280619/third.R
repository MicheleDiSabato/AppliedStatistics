# Problema 3
# The file airport.txt reports the data on the duration of the trips [min] made by 168 travellers moving from
# Terrassa to Barcellona airport, using the Express Shuttle Bus in weekdays of June 2019. For the duration of a trip
# consider a linear model, accounting for the distance traveled x [km] (i.e., the distance from the bus stop of origin
#                                                                       and the airport), and for the time of the day ('6-10', '11-15', '16-20'):
#   Yg = ??0,g + ??1,g · x + eps,
# with eps ??? N(0, sigma2)
#  and g the grouping structure induced by the time of the day.
# a) Estimate the parameters of the model ({beta_0,g, beta_1,g, sigma}). Verify the model assumptions.
# b) Perform two statistical tests - each at level 1% - to verify if
# - there is a significant dependence of the mean duration on the time of the day;
# - there is a significant dependence of the mean duration on the distance traveled.
# c) Based on tests (b) or any other test deemed relevant, reduce the model and update the model parameters.
# d) You have a flight from Barcelona airport at 10:30 a.m., and you want to be at the airport at least 1 hour
# before the flight departure. At the bus station in front of your hotel in Terrassa (distance of 57 km from the
#                                                                                     airport), the bus is scheduled to depart every 30 minutes from 6 a.m. to 20:30 p.m.. What time would you
# take the bus to be on time with probability 99%?
#   


# a) Estimate the parameters of the model ({??0,g, ??1,g, ??}). Verify the model assumptions

data <- read.table('airport.txt', header=TRUE)
head(data)
table(data$time.of.the.day)
D1 = c(rep(0,53),rep(1,48),rep(0,67))
D2 = c(rep(0,53),rep(0,48),rep(1,67))
D1 
D2
airport = cbind(data[,1:2], D1=D1, D2=D2)
head(airport)
airport
fit <- lm(duration ~      distance    + 
                     D1 + D1:distance + 
                     D2 + D2:distance, data=airport)
summary(fit)

x11()
par(mfrow=c(2,2))
plot(fit)
dev.off()

shapiro.test(fit$residuals)

fit2 <- lm(duration ~      distance    + 
            D1 + D1:distance + 
            D2 + D2:distance, data=airport[c(-14,-18, -49),])
summary(fit2)

x11()
par(mfrow=c(2,2))
plot(fit2)
dev.off()

shapiro.test(fit2$residuals)

# why are those points relevant?
x11()
plot(data[,1:2],pch=19,col=c(D1+2+2*D2))
points(data[14,1:2], col="black",pch=4,lwd=4)
points(data[18,1:2], col="black",pch=4,lwd=4)
points(data[49,1:2], col="black",pch=4,lwd=4)

data[14,]
data[18,]
data[49,]

# they are all in the time zone 6-10. Probably the issue is that they are 
# far from the other points in group 6-10


# b) Perform two statistical tests - each at level 1% - to verify if
# - there is a significant dependence of the mean duration on the time of the day;
# - there is a significant dependence of the mean duration on the distance traveled.

alpha = 0.01
summary(fit)
C1 = rbind(c(0,0,1,0,0,0),
           c(0,0,0,1,0,0),
           c(0,0,0,0,1,0),
           c(0,0,0,0,0,1))
b1 = c(0,0,0,0)
C2 = rbind(c(0,1,0,0,0,0),
           c(0,1,0,0,1,0),
           c(0,1,0,0,0,1))
b2 = c(0,0,0)
# C1 = rbind(c(0,0,1,0,0,0),
#            c(0,0,0,1,0,0))
# b1 = c(0,0)
linearHypothesis(fit, C1, b1)
linearHypothesis(fit, C2, b2)

# w reject both the HP

# c) Based on tests (b) or any other test deemed relevant, reduce the model and update the model parameters.

summary(fit)
C = rbind(c(0,0,1,0,0,0),
          c(0,0,0,1,0,0))
b = c(0,0)
linearHypothesis(fit, C, b)
# remove the dependence of the intercept on the groups

fit_bis <- lm(duration ~  distance    + 
                          D1:distance + 
                          D2:distance, data=airport)
summary(fit_bis)

x11()
plot(data[,2:1],pch=19,col=c(D1+2+2*D2))
x = seq(30, 70, length=400)
beta = fit_bis$coefficients
intercept = beta[1]
points(x,intercept + (beta[2])*x, type="l", col=2)
points(x,intercept + (beta[2]+beta[3])*x  ,type="l", col=3)
points(x,intercept + (beta[2]+beta[4])*x  ,type="l", col=4)


# d) You have a flight from Barcelona airport at 10:30 a.m., and you want to be at the airport at least 1 hour
# before the flight departure. At the bus station in front of your hotel in Terrassa (distance of 57 km from the
# airport), the bus is scheduled to depart every 30 minutes from 6 a.m. to 20:30 p.m.. What time would you
# take the bus to be on time with probability 99%?
#

x_new = data.frame(D1 = 0, D2 = 0, distance = 57) # devo partire nella fascia 6-10 per arrivare in tempo
predict(fit_bis, x_new, interval = "prediction", level = 0.99)
# la previsione della durata del viaggio è di 112.4174 minuti (circa 1 ora e un 50, 2 ore)
# voglio arrivare alle 9:30 o prima ---> prendo al più la navetta che parte alle 7:30






