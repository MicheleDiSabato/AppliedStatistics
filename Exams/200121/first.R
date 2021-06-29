# Problem n.1
# The file wine.txt reports the alcohol content of 150 Italian wines. The dataset also reports the color of the wine
# (red or white) and the region of production (Piemonte, Toscana or Veneto).
# a) Build a complete ANOVA model for the alcohol content as a function of the factors color (red or white) and
#   region (Piemonte, Toscana or Veneto). Report and verify the assumptions of the model.
# b) Perform tests for the significance of the factors and of their interaction and comment the results. If needed,
#   propose a reduced model and report the estimates of the parameters of the model.
# c) Build Bonferroni confidence intervals (global level 99%) for the means and variances of the groups identified at
#   point (b). Comment the results.

wine <- read.table('wine.txt', header=TRUE)
head(wine)

# a) Build a complete ANOVA model for the alcohol content as a function of the factors color (red or white) and
# region (Piemonte, Toscana or Veneto). Report and verify the assumptions of the model.

wine$region

alc = wine$alcohol
region = factor(wine$region)
color = factor(wine$color)

g = length(levels(region))
b = length(levels(color))
n = length(alc)/(g*b)
n
g
b

M = mean(alc)
Mregion = tapply(alc, region, mean)
Mcolor = tapply(alc, color, mean)

x11()
par(mfrow=c(1,2))
boxplot(alc ~ region)
boxplot(alc ~ color)
# region seems similar, color seeems different

# fit the model:
fit <- aov(alc ~ region + color + region:color)
summary(fit)

# verify assumptions:
i1 = which(region == "Piemonte" & color == "red")
i2 = which(region == "Piemonte" & color == "white")
i3 = which(region == "Toscana" & color == "red")
i4 = which(region == "Piemonte" & color == "white")
i5 = which(region == "Veneto" & color == "red")
i6 = which(region == "Veneto" & color == "white")

P = NULL
for(j in 1:6)
 P = cbind(P,shapiro.test(wine[eval(parse(text=paste("i",j,sep=""))),1])$p)


bartlett.test(alc, factor(region):factor(color))



# b) Perform tests for the significance of the factors and of their interaction and comment the results. If needed,
#   propose a reduced model and report the estimates of the parameters of the model.

summary(fit)

# from the summary it can be seen that the interaction is not statistically relevant:
fit2 <- aov(alc ~ region + color)
summary(fit2)

# from the summary it can be seen that factor region is not statistically relevant:
fit3 <- aov(alc ~ color)
summary(fit3)

# estimate of the parameters of the model:
mi = M
mi
tau_Rosso = mean(alc[which(color=="red")])
tau_Bianco = mean(alc[which(color=="white")])
tau_Rosso
tau_Bianco
sigma2 = sum(fit3$residuals^2)/fit3$df.residual
sigma2



# c) Build Bonferroni confidence intervals (global level 99%) for the means and variances of the groups identified at
#   point (b). Comment the results.

S = sigma2
alpha = 0.01
k = 2
chi_1 = qchisq(alpha/(2*k), fit3$df.residual)
chi_2 = qchisq(1-alpha/(2*k), fit3$df.residual)
qT = qt(1-alpha/(2*k), fit3$df.residual)
IC = c(Mcolor[1]-Mcolor[2] - qT * sqrt(S*(1/75 + 1/75)),
       Mcolor[1]-Mcolor[2] + qT * sqrt(S*(1/75 + 1/75)))
names(IC) = c("inf","sup")
IC
# alcohol in red wine seems to be higher that the one in the white wine

c(fit3$df.residual*S/chi_2, fit3$df.residual*S/chi_1)
S








































































