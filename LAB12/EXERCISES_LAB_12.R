# LAB 12

library(MASS)
library(car)
library(rgl)

setwd("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/LAB_12")
load("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/LAB_5/mcshapiro.test.RData")


######-----------------------------
###### Exam of 5/09/2008

### Problem 1
# Famous Dantists believe that the number of characters (NC) and the 
# number of words (NP) contained in a generic sonnet of Dante follow
# approximately a jointly normal distribution with mean mu=c(400,90) 
# and covariance matrix Sigma=cbind(c(100,20), c(20,10)). Recently, 
# two new sonnets attributed to Dante have been discovered, and have 
# been identified for the moment with the codes 2008A and 2008B. 
# Assuming the number of characters and of words of the sonnet 2008A
# independent of the number of characters and words of the sonnet 2008B:
# a) identify in the plane NC x NP an ellipsoidal region in which the
#    sonnet 2008A is contained with probability 0.9.
# b) How likely only one of the two sonnets falls in the region 
#    identified at point (a)?

# SOLUTION:

# (a)

mu=c(400,90)
sigma=cbind(c(100,20), c(20,10))
alpha = 0.1
r = sqrt(qchisq(1-alpha, 2))
x11()
plot(mu[1],mu[2], col="white",xlim=c(350,450),ann=FALSE)
ellipse(center = mu, shape = sigma, radius = r)
dev.off()

# (b)

# P(2008A in and 2008B out) +  P(2008B in and 2008A out)
0.9*0.1 + 0.1*0.9


### Problem 2
# The dataset eighteen.txt contains, for 100 Italian municipalities, 
# the percentages of underage boys and underage girls with respect to
# the entire population resident in the municipality. Assuming that 
# this is a sample from a bivariate normal distribution:
# a) perform a test to verify that in Italy the number of underage 
#    resident boys is the same as the number of underage resident girls;
# b) knowing that 60 million people reside in Italy, provide three 
#    T2-simultaneous intervals (global confidence 90%) for:
#    - The absolute number of underage boys who reside in Italy,
#    - The absolute number of underage girls who reside in Italy,
#    - The absolute number of minors who reside in Italy.

# a) perform a test to verify that in Italy the number of underage 
#    resident boys is the same as the number of underage resident girls;
eighteen <- read.table('eighteen.txt', header=T)
head(eighteen)
dim(eighteen)

girls = eighteen[,2]
boys = eighteen[,1]
shapiro.test(girls)
shapiro.test(boys)

m1 = mean(girls)
m1
m2 = mean(boys)
m2
S1 = cov(data.matrix(girls))
S1
S2 = cov(data.matrix(boys))
S2
n1 = length(girls)
n2 = length(boys)
n = n1+n2
Sp = ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
Spinv = solve(Sp)
alpha   <- .1
delta.0 <- 0
T2 <- n1*n2/(n1+n2) * (m1-m2-delta.0) %*% Spinv %*% (m1-m2-delta.0)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
T2 < cfr.fisher # reject H0, the means are different
P <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
P 

#   QUESTO è SBAGLIATO PERCHè LE DUE POP NON SONO INDIPENDENTI

D = boys - girls
shapiro.test(D)
m = mean(D)
m
S = var(D)
S
Sinv = solve(S)  
Sinv 
n = 100 
p = 1
delta.0 = 0
T2 = n*(m-delta.0)%*%Sinv%*%(m-delta.0)
f = T2*(n-p)/(p*(n-1))
P <- 1-pf(f, p, n-p)
P
t.test(D, alternative = 'two.sided')


  
# b) knowing that 60 million people reside in Italy, provide three 
#    T2-simultaneous intervals (global confidence 90%) for:
#    - The absolute number of underage boys who reside in Italy,
#    - The absolute number of underage girls who reside in Italy,
#    - The absolute number of minors who reside in Italy.

N = 60000000/100
d = eighteen*N
m = colMeans(d)
m
S = cov(d)
Sinv = solve(S)
n=100
p=2
k = 3
t = qt(1-alpha/(2*k), n-1)
IC1 = cbind(m[1] - t * sqrt(S[1,1]/n),
            m[1] + t * sqrt(S[1,1]/n))
IC2 = cbind(m[2] - t * sqrt(S[2,2]/n),
            m[2] + t * sqrt(S[2,2]/n))
IC3 = cbind(m[1]+m[2] - t * sqrt((S[2,2]+S[1,1]+2*S[1,2])/n),
            m[1]+m[2] + t * sqrt((S[2,2]+S[1,1]+2*S[1,2])/n))
IC = rbind(IC1,IC2,IC3)
IC            
            
f = sqrt(qf(1-alpha, p, n-p)*(p*(n-1))/(n-p))
IC1 = cbind(m[1] - f * sqrt(S[1,1]/n),
            m[1] + f * sqrt(S[1,1]/n))
IC2 = cbind(m[2] - f * sqrt(S[2,2]/n),
            m[2] + f * sqrt(S[2,2]/n))
IC3 = cbind(m[1]+m[2] - f * sqrt((S[2,2]+S[1,1]+2*S[1,2])/n),
            m[1]+m[2] + f * sqrt((S[2,2]+S[1,1]+2*S[1,2])/n))  
IC = rbind(IC1,IC2,IC3)
IC             
# > T2
# Inf     Mean      Sup
# [1,]  7060682  7204440  7348198
# [2,]  7721717  7852680  7983643
# [3,] 14963529 15057120 15150711
            

### Problem 3
# The West Sussex Bread Association has randomly selected 60 business
# trades in which doughnuts are commonly sold. 30 activities are based
# in the city of Brighton and 30 in the town of Worthing. For each of 
# the two cities, the price of a plain doughnut was recorded in 10 
# activities, the price of a doughnut filled with cream in other 10 
# activities and the price of a doughnut filled with jam in the 
# remaining 10 activities.
# The data are reported in doughnut.txt dataset.
# a) Describe the ANOVA model you deem appropriate for the analysis of 
#    these data.
# b) Identifying factors that significantly influence the distribution
#    of the price of doughnuts, propose a possible reduced model.
# c) Using the Bonferroni's inequality estimate through bilateral 
#    confidence intervals (with global confidence 95%) the means and the
#    variances of the subpopulations associated with the reduced model
#    identified at step (b).

doughnuts <- read.table('doughnut.txt', header=TRUE)
head(doughnuts)
dim(doughnuts)

# a) Describe the ANOVA model you deem appropriate for the analysis of 
#    these data.
# Xij_k = mu + tau_i + beta_j + gamma_ij + eps_ij_k  (k=1 --> 2wayANOVA)
# primo livello: città ; secondo livello: gusto

# b) Identifying factors that significantly influence the distribution
#    of the price of doughnuts, propose a possible reduced model.

table(doughnuts$citta)
table(doughnuts$tipo)
prezzo <- doughnuts$prezzo
citta <- factor(doughnuts$citta)
tipo <- factor(doughnuts$tipo)
g <- length(levels(citta)) 
b <- length(levels(tipo)) 
n <- length(prezzo)/(g*b)

fit.aov1 <- aov(prezzo ~ citta + tipo + citta:tipo)
summary.aov(fit.aov1)

# verify assumptions
p.val <- c(
  shapiro.test(prezzo[which(citta==levels(factor(citta))[1] & tipo==levels(factor(tipo))[1])])$p,
  shapiro.test(prezzo[which(citta==levels(factor(citta))[1] & tipo==levels(factor(tipo))[2])])$p,
  shapiro.test(prezzo[which(citta==levels(factor(citta))[1] & tipo==levels(factor(tipo))[3])])$p,
  shapiro.test(prezzo[which(citta==levels(factor(citta))[2] & tipo==levels(factor(tipo))[1])])$p,
  shapiro.test(prezzo[which(citta==levels(factor(citta))[2] & tipo==levels(factor(tipo))[2])])$p,
  shapiro.test(prezzo[which(citta==levels(factor(citta))[2] & tipo==levels(factor(tipo))[3])])$p)
p.val

bartlett.test(prezzo, factor(citta):factor(tipo))

fit.aov2 <- aov(prezzo ~ citta + tipo)
summary.aov(fit.aov2)

fit.aov3 <- aov(prezzo ~ tipo)
summary.aov(fit.aov3)

# c) Using the Bonferroni's inequality estimate through bilateral 
#    confidence intervals (with global confidence 95%) the means and the
#    variances of the subpopulations associated with the reduced model
#    identified at step (b).

M           <- mean(prezzo)
Mcitta     <- tapply(prezzo,      citta, mean)
Mtipo       <- tapply(prezzo,       tipo, mean)

SSres <- sum(residuals(fit.aov3)^2) # variance estimate

k <- g*(g-1)/2
crema_liscia = cbind(Mtipo[1]-Mtipo[2] - qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))),
      Mtipo[1]-Mtipo[2] + qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))
crema_marmellata = cbind(Mtipo[1]-Mtipo[3] - qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))),
                     Mtipo[1]-Mtipo[3] + qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))
liscia_marmellata = cbind(Mtipo[2]-Mtipo[3] - qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))),
                     Mtipo[2]-Mtipo[3] + qt(0.95, (n*g-1)*b) * sqrt(SSres/((n*g-1)*b) *(1/(n*g) + 1/(n*g))))

IC <- list(crema_liscia=crema_liscia,
           crema_marmellata=crema_marmellata,
           liscia_marmellata=liscia_marmellata)
IC

# la differenza è tra crema e liscia, la città non è influente

# ma non è quello che mi chiedeva, il testo chiedeva di utilizzare intervalli di confidenza
# per stimare la media delle singole sottopopolazioni e la loro varianza.
# OSSERVAZIONI:
#  - la media varia da gruppo a gruppo
#  - la varianza è unica ---> usare la Spooled (omoschedasticity)

# divido per tipo
marmellata <- prezzo[which(tipo == "marmellata")]
n3 <- length(marmellata)
liscia <- prezzo[which(tipo == "liscia")]
n2 <- length(liscia)
crema <- prezzo[which(tipo == "crema")]
n1 <- length(crema)
n <- n1 + n2 + n3 # numero di unità statistiche (60)
g <- 3 # numero di gruppi
m1 <- mean(crema)
m2 <- mean(liscia)
m3 <- mean(marmellata)
S1 <- var(crema)
S2 <- var(liscia)
S3 <- var(marmellata)
Sp <- ((n1-1)*S1+(n1-1)*S2+(n1-1)*S3)/(n-g)
Spinv <- solve(Sp)
alpha <- 0.05
k <- 4 # perchè voglio controllare le 3 medie E LA VARIANZA
qT <- qt(1-alpha/(2*k),n-g)
IC_mean <- list(crema = cbind(m1-qT*sqrt(Sp/n1), m1+qT*sqrt(Sp/n1)),
                liscia = cbind(m2-qT*sqrt(Sp/n2), m2+qT*sqrt(Sp/n2)),
                marmellata = cbind(m3-qT*sqrt(Sp/n3), m3+qT*sqrt(Sp/n3)))
IC_mean
chi_2 <- qchisq(1-alpha/(2*k), n-g) #  chi_2 > chi_1
chi_1 <- qchisq(alpha/(2*k), n-g)

IC_sigma <- cbind((n-g)*Sp/chi_2, (n-g)*Sp/chi_1) # chi_1 <= (n-g)*Sp/(sigma^2) <= chi_2
IC_sigma

# soluzione esatta:

# > BF
# inf         sup
# [1,] 0.4292741080 0.458725892
# [2,] 0.3482741080 0.377725892
# [3,] 0.4317741080 0.461225892
# [4,] 0.0004264347 0.001098285 # qeusta ultima riga riguarda l'IC per la varianza



### Problem 4
# Due to the increased cost of oil, the airline FlyDown is interested in
# identifying a model to estimate the weight of a passenger (typically 
# not available) from its age and the sex (available data). For this 
# reason, the FlyDown collected, via an anonymous questionnaire, data 
# about the weight of 126 clients (63 men and 63 women) aged between 18
# and 80 years (flydown.txt dataset).
# a) Introduce a regression model quadratic in age to describe the 
#    dependence of the expected weight of a passenger on their age and
#    sex.
# b) Is there statistical evidence of a dependence of the expected 
#    weight on sex?
# c) Is there statistical evidence of a dependence (linear or quadratic)
#    of the expected weight on age?
# d) Identify a reduced model of the model (a) suitable to describe the
#    collected data and estimate its parameters.
# e) On the basis of the results at point (d), is there statistical 
#    evidence to reject the hypothesis that the maximum of the expected
#    weight (for men and / or women) is reached at the age of 50?
# f) Identify a reduced model of the model (d) that takes into account
#    the statement in paragraph (e) and estimate its parameters.

# a) Introduce a regression model quadratic in age to describe the 
#    dependence of the expected weight of a passenger on their age and
#    sex.

fly <- read.table('flydown.txt', header=TRUE)
head(fly)
attach(fly)
dummy <- c(rep(1,63), rep(0,63))
dummy
eta2 = (fly$eta)^2
fit <- lm(peso ~ eta + eta2 + dummy + dummy:eta + dummy:eta2)
summary(fit)
# y = beta_0 + beta_1 * eta + beta_2 * eta^2 + beta_3 * dummy + beta_4 * dummy * eta + beta_5 * dummy * eta^2 + eps
# forma ridotta: y = BETA_0 + BETA_1 * eta + BETA_2 * eta^2 + EPS
# con :
# BETA_0 = beta_0, se F
# BETA_0 = beta_0 + beta_3, se M
# BETA_1 = beta_1, se F
# BETA_1 = beta_1 + beta_4, se M
# BETA_2 = beta_2, se F
# BETA_2 = beta_2 + beta_5, se M

x = seq(min(eta),max(eta), by = 0.1)
betas = fit$coefficients
betas
plot(x, betas[1]+betas[2]*x+betas[3]*x^2,type='l',col="deeppink",ylim=c(50,100))
points(x, betas[1]+betas[4]+(betas[2]+betas[5])*x+(betas[3]+betas[6])*x^2,type='l',col="blue")
points(eta, peso, col=c(rep("blue", 63), rep("deeppink", 63)),pch=19)

par(mfrow=c(2,2))
plot(fit)

shapiro.test(fit$residuals)


# b) Is there statistical evidence of a dependence of the expected 
#    weight on sex?

C <- rbind(c(0,0,0,1,0,0),
           c(0,0,0,0,1,0),
           c(0,0,0,0,0,1))
b = c(0,0,0)
linearHypothesis(fit, C, b)
# p-value basso ---> the expected weight depends on sex


# c) Is there statistical evidence of a dependence (linear or quadratic)
#    of the expected weight on age?

C <- rbind(c(0,1,0,0,0,0),
           c(0,0,1,0,0,0),
           c(0,0,0,0,1,0),
           c(0,0,0,0,0,1))
b = c(0,0,0,0)
linearHypothesis(fit, C, b)
# p-value basso ---> the expected weight depends on age


# d) Identify a reduced model of the model (a) suitable to describe the
#    collected data and estimate its parameters.

summary(fit)
# pvalue bassi per eta:dummy e eta2:dummy ---> rimuovo eta2:dummy
fit2 <- lm(peso ~ eta + eta2 + dummy + dummy:eta)
summary(fit2)
Q = cbind(fly[,c(1,3)], dummy)
fit3 <- lm(peso ~ eta + I(eta^2) + dummy,data=Q)
summary(fit3)
# p value bassi, non riduco oltre
# OSSERVASIONE:
# AVREI POTUTO RIMUOVERE CONTEMPORANEAMENTE eta2:dummy E eta:dummy USANDO
# IL COMANDO linearHypothesis()
par(mfrow=c(2,2))
plot(fit3)

dev.off()

shapiro.test(fit3$residuals)


# e) On the basis of the results at point (d), is there statistical 
#    evidence to reject the hypothesis that the maximum of the expected
#    weight (for men and / or women) is reached at the age of 50?

# y = beta_0 + beta_1*eta + beta_2*eta^2 + beta_3*dummy + eps
x = seq(min(eta),max(eta), by = 0.1)
betas = fit3$coefficients
betas
plot(x, betas[1]+betas[2]*x+betas[3]*x^2,type='l',col="deeppink",ylim=c(50,100))
points(x, betas[1]+betas[4]+(betas[2])*x+(betas[3])*x^2,type='l',col="blue")
points(eta, peso, col=c(rep("blue", 63), rep("deeppink", 63)),pch=19)
X_new.M <- data.frame(eta = 50, eta2=50*50, dummy = 0)
X_new.F <- data.frame(eta = 50, eta2=50*50, dummy = 1)
IC.M <-predict(fit3 ,X_new.M,interval="confidence",level=0.95)
IC.F <-predict(fit3 ,X_new.F,interval="confidence",level=0.95)
matplot(X_new.M,IC.M,add=T,type='l',lwd=2,lty=2)

# non sta chiedendo questo, serve derivare e imporre che sia zero in eta = 50:

# Deriving and imposing the derivative to be 0 we obtain that the 
# maximum is reached for (beta2<0):
# beta1+2*beta2*eta.max=0, i.e., eta.max=50 <=> beta1+2*beta2*50=0
A <- c(0,1,2*50,0)
b <- 0

linearHypothesis(fit3,A,b)


# f) Identify a reduced model of the model (d) that takes into account
#    the statement in paragraph (e) and estimate its parameters.

dummy2=-100*eta+eta2
fit4 <- lm(peso ~ dummy2 + dummy)
summary(fit4)
fit5 <- lm(peso ~ I(-100*eta+eta^2) + dummy,data=Q)
coef(fit5)
coef(fit4)


######-----------------------------

###### Exam of 21/09/2009

#_______________________________________________________________________________
### Problem 1

# The sco2009.txt file collects the duration [minutes] of different 
# talks at S.Co.2009 Conference (18 sessions of 4 talks each). Assuming
# a four-dimensional Gaussian distribution for the first, second, third
# and fourth talk of the same session:
# a) Is there statistical evidence to state that the mean durations of
#    the four talks are different?
# b) Using Bonferroni's inequality, provide eight intervals of 90% global 
#    confidence for the mean and variance of the first, the second, the
#    third and fourth talk.
# On the basis of the introduced model, using the sample estimates of 
# the mean and of the covariance matrix, and knowing that the maximum 
# time available for each session is 2 hours:
# c) Estimate the probability that the time available for questions
#    is less than 15 minutes.
# d) Estimate the probability that the sum of the durations of the four
#    talks exceeds the maximum time of 2 hours.

sco <- read.table('sco2009.txt', header=TRUE)
sco

load("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/LAB_5/mcshapiro.test.RData")

# a) Is there statistical evidence to state that the mean durations of
#    the four talks are different?

# paired measuraments

mcshapiro.test(sco)

n <- dim(sco)[1]
q <- dim(sco)[2]

M <- sapply(sco,mean)
M
S <- cov(sco)
S

C <- rbind(c(-1, 1, 0, 0),
           c(-1, 0, 1, 0),
           c(-1, 0, 0, 1))
C

T02 <- n*t(C%*%M)%*%solve(C%*%S%*%t(C))%*%C%*%M
alpha = 0.1
cfr.fisher <- (n-1)*(q-1)/(n-q+1)*qf(1-alpha,q-1,n-q+1)
T02 < cfr.fisher
# reject
p <- 1-pf(T02/((n-1)*(q-1)/(n-q+1)),q-1,n-q+1)
p
# means are different


# b) Using Bonferroni's inequality, provide eight intervals of 90% global 
#    confidence for the mean and variance of the first, the second, the
#    third and fourth talk.

alpha = 0.1
k <- 8
qT <- qt(1-alpha/(2*k), n-1)
chi_2 <- qchisq(1-alpha/(2*k), n-1)
chi_1 <- qchisq(alpha/(2*k), n-1)

IC1 <- c(M[1]-qT * sqrt(S[1,1]/n), M[1]+qT * sqrt(S[1,1]/n))
IC2 <- c(M[2]-qT * sqrt(S[2,2]/n), M[2]+qT * sqrt(S[2,2]/n))
IC3 <- c(M[3]-qT * sqrt(S[3,3]/n), M[3]+qT * sqrt(S[3,3]/n))
IC4 <- c(M[4]-qT * sqrt(S[4,4]/n), M[4]+qT * sqrt(S[4,4]/n))

IC.M <- rbind(IC1,IC2,IC3,IC4)
IC.M
M

# > ICmedie
# Inf        M      Sup
# primo   29.44280 30.98333 32.52387
# secondo 29.25721 30.13889 31.02056
# terzo   29.49716 30.69444 31.89173
# quarto  19.56280 20.87778 22.19275

IC1 <- c(S[1,1]*(n-1)/chi_2, S[1,1]*(n-1)/chi_1)
IC2 <- c(S[2,2]*(n-1)/chi_2, S[2,2]*(n-1)/chi_1)
IC3 <- c(S[3,3]*(n-1)/chi_2, S[3,3]*(n-1)/chi_1)
IC4 <- c(S[4,4]*(n-1)/chi_2, S[4,4]*(n-1)/chi_1)

IC.sigma <- rbind(IC1,IC2,IC3,IC4)
IC.sigma
c(S[1,1], S[2,2], S[3,3], S[4,4])

# > ICBVar
# Inf.        M       Sup
# primo   2.6618762 5.477941 15.748604
# secondo 0.8718885 1.794281  5.158402
# terzo   1.6078288 3.308791  9.512485
# quarto  1.9394498 3.991242 11.474472

# c) Estimate the probability that the time available for questions
#    is less than 15 minutes.

no_q = rep(0,n)
som = NULL
for(i in 1:n)
{
  no_q[i] = sum(sco[i,])>=45+60
  som = c(som, sum(sco[i,]))
}
no_q
som
prob <- sum(no_q)/n
prob

# stima naive perchè non sfrutto nessuna ipotesi

# P(X1+X2+X3+X4 >= 105)
# S_new <- sum(diag(S))
# 1-pnorm(105, sum(M), sqrt(S_new))

a <- c(1,1,1,1)
S_new <- t(a)%*%S%*%a
S_new
1-pnorm(105, sum(M), sqrt(S_new))



# d) Estimate the probability that the sum of the durations of the four
#    talks exceeds the maximum time of 2 hours.

1-pnorm(120, sum(M), sqrt(S_new))





### Problem 2
# The file 100m.txt contains the time [seconds] used to run the 100m
# by 20 students enrolled in the athletics team of Politecnico.
# For each student 4 times are given: just back from summer holidays, 
# one, two and three weeks after return. Framing the problem in the
# context of repeated measures:
# a) Is there statistical evidence to say that the mean time changes 
#    over time?
# b) Using appropriate confidence intervals, describe the temporal 
#    dynamics the mean times.
# c) After how many weeks one may think that the mean time has
#    stabilized?


# a) Is there statistical evidence to say that the mean time changes 
#    over time?

tempi <- read.table('100m.txt', header=TRUE)
head(tempi)
dim(tempi)

n <- dim(tempi)[1]
q <- dim(tempi)[2]
M <- sapply(tempi,mean)
M
S <- cov(tempi)
S

mcshapiro.test(tempi)

# C <- rbind(c(-1,1,0,0),
#            c(-1,0,1,0),
#            c(-1,0,0,1))
C <- rbind(c(-1,1,0,0),
           c(0,-1,1,0),
           c(0,0,-1,1))
C
Sinv <- solve( C%*%S%*%t(C) )
T02 <- n*t(C%*%M)%*%Sinv%*%C%*%M
T02
w <- (n-1)*(q-1)/(n-q+1)
# pvalue
1-pf(T02/w, q-1, n-q+1)
# reject H0


# b) Using appropriate confidence intervals, describe the temporal 
#    dynamics of the mean times.

k <- q-1 + 1
alpha <- 0.05
qT <- qt(1-alpha/(2*k), n-1)
S_new2 <- diag(C%*%S%*%t(C))
IC <- cbind(C%*%M - qT*sqrt(S_new2/n), C%*%M + qT*sqrt(S_new2/n))
IC

# ATTENZIONE ALLA SCELTA CORRETTA DELLA CONTRAST MATRIX

# initial mean 11.8805894 12.3415 12.8024106
# increment1   -0.8655857 -0.2845  0.2965857
# increment2   -1.1393739 -0.7475 -0.3556261
# increment3   -0.2636326  0.1415  0.5466326

# c) After how many weeks one may think that the mean time has
#    stabilized?

# tra la seconda e la terza settimana, come si vede dal punto precedente



### Problem 3
# In the summer period, hundreds of whales move to the Gulf of Maine to
# feed in views of winter. In this period the whales are in separated 
# and geographically localized colonies. The whales.txt file gives the 
# geographical coordinates of sightings in the sea area in front of
# Boston that it is known to host two colonies.
# a) By using a hierarchical agglomerative clustering algorithm
#    (Euclidean metrics and linkage and Ward), identify the sightings 
#    relative to each of the two colonies.
# b) Discuss about the algorithm's goodness.
# c) By assuming correct the subdivision obtained at point (a) and 
#    introducing suitable assumptions, provide a point estimate and a 
#    90% confidence region for the relative position of the two colonies.


# a) By using a hierarchical agglomerative clustering algorithm
#    (Euclidean metrics and linkage and Ward), identify the sightings 
#    relative to each of the two colonies.
whales <- read.table('whales.txt', header=TRUE)
head(whales)

plot(whales)

d <- dist(whales, method = 'euclidean')
n <- dim(whales)[1]
x11()
image(1:n,1:n,as.matrix(d), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
# misc <- sample(n)
# whales <- whales[misc,]
d <- dist(whales, method = 'euclidean')
n <- dim(whales)[1]
x11()
image(1:n,1:n,as.matrix(d), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
plot(whales)
dev.off()

cl <- hclust(d, method='ward.D')
names(cl)
plot(cl, hang=-0.1, labels=FALSE, sub='')
clusters <- cutree(cl, k=2)
clusters
table(clusters)
rect.hclust(cl,k=2)


# b) Discuss about the algorithm's goodness.

plot(whales, col=ifelse(clusters==1, 'red', 'blue'),pch=19)
table(clusters)
# group 1 --> red
# group 2 --> blue
# PER TESTARE LA GOODNESS DEVO USARE COPHENETIC COEFFICIENT
coph <- cophenetic(cl)
c <- cor(d, coph)
c

# c) By assuming correct the subdivision obtained at point (a) and 
#    introducing suitable assumptions, provide a point estimate and a 
#    90% confidence region for the relative position of the two colonies.

g1 <- which(as.factor(clusters)==1)
g1
g2 <- which(as.factor(clusters)==2)
g2
n1 <- length(g1)
n2 <- length(g2)
group1 <- whales[g1,]
group2 <- whales[g2,]
mcshapiro.test(group1)
mcshapiro.test(group2)
S1 <- cov(group1)
S1
S2 <- cov(group2)
S2
x11()
par(mfrow=c(1,2))
image(1:2,1:2,as.matrix(S1), asp=1)
image(1:2,1:2,as.matrix(S2), asp=1)
Sp <- ((n1-1)*S1 + (n2-1)*S2)/(n1+n2-2)
Sp
p  <- dim(whales)[2] # p=2
p
m1 <- sapply(group1,mean)
m2 <- sapply(group2,mean)
alpha   <- .1
delta.0 <- c(0,0)
Spinv   <- solve(Sp)
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)
# assi
eigen(Sp)$vectors
# raggio
r <- sqrt(cfr.fisher)
r
# semiampiezza
r*sqrt(eigen(Spooled*(1/n1+1/n2))$values)
# grahically
x11()
M = m1-m2
plot(M[1],M[2],pch=19) 
ellipse(center=M, shape=Sp*(1/n1+1/n2), radius=sqrt(cfr.fisher), lwd = 2, col = 'red', lty = 2, center.pch = 4)
grid()




### Problem 4
# The BGMI.txt file contains travel times [hours] of the Bergamo-Milan 
# highway, the departure time from Bergamo [hours] and the day of the 
# week for 1127 vehicles leaving between 6:00 and 9:00. Assume a 
# dependence at most quadratic of the mean travel time on the time of 
# departure, possibly different depending on the day of the week:
# T = alpha.g + beta.g(t.partenza-7.5) + gamma.g(tpartenza-7.5)^2 + eps
# with eps ~ N (0, sigma ^ 2) and g={working, holiday}.
# a) Provide the least squares estimates of the model parameters.
# b) On the basis of a suitable test, is there statistical evidence of a
#    difference in the mean trends between weekdays and weekends?
# c) Build a suitable reduced model with 4 degrees of freedom and 
#    estimate its parameters.
# d) On basis of model (c), provide interval estimates (90% global
#    confidence) for the value of the regression curve and its 
#    derivative for a weekday departure and / or weekend at 7:30.

graphics.off()
tper <- read.table('BGMI.txt', header=TRUE)
head(tper)
dim(tper)
n <- dim(tper)[1]
p <- dim(tper)[2]

# a) Provide the least squares estimates of the model parameters.

dummy <- ifelse(tper$giorno == 'festivo', 1, 0)
# feriale = 1, festivo = 0
tper <- cbind(tper[,c(1,2)], dummy)
fit <- lm(durata ~ I(partenza-7.5) + I((partenza-7.5)^2) + dummy + 
            dummy:I(partenza-7.5) + dummy:I((partenza-7.5)^2), data = tper)
summary(fit)
# dum = tper$dummy
# p = tper$partenza-7.5
# p2 = p^2
# d = tper$durata
# fit2 <- lm(d ~ p + p2 + dummy + dum:p + dum:p2)
# summary(fit2)
x11()
par(mfrow=c(2,2))
plot(fit)
dev.off()

shapiro.test(fit$residuals)

fit$coefficients


# b) On the basis of a suitable test, is there statistical evidence of a
#    difference in the mean trends between weekdays and weekends?

C <- rbind(c(0,0,0,1,0,0),
           c(0,0,0,0,1,0),
           c(0,0,0,0,0,1))
C
b <- c(0,0,0)
linearHypothesis(fit, C, b)
# reject --> there is a difference between festivi e feriali


# c) Build a suitable reduced model with 4 degrees of freedom and 
#    estimate its parameters.

# degrees of freedom = number of regressors ( we called this "r" )
fit2 <- lm(durata ~ I(partenza-7.5) + I((partenza-7.5)^2) + dummy + dummy:I((partenza-7.5)^2), data = tper)
summary(fit2)
fit2$coefficients


# d) On basis of model (c), provide interval estimates (90% global
#    confidence) for the value of the regression curve and its 
#    derivative for a weekday departure and / or weekend at 7:30.
k <- 3 # DICE GLOBAL CONFIDENCE e serve controllare ANCHE la DERIVATA
X.new.festivo <- data.frame(partenza = 7.5, dummy = 1)
X.new.feriale <- data.frame(partenza = 7.5, dummy = 0)
Conf.festivo <- predict(fit2, X.new.festivo, interval='confidence', level=1-0.1/k)  
Conf.festivo
Conf.feriale <- predict(fit2, X.new.feriale, interval='confidence', level=1-0.1/k)  
Conf.feriale

# ALTERNATIVA:
# k <- 3
# n <- dim(tper)[1]
# Z0.new <- data.frame(partenza=c(7.50,7.50), dummy=c(0,1))
# Conf <- predict(fit2, Z0.new, interval='confidence', level=1-.1/k)
# Conf
# > Conf
# fit       lwr       upr
# 1 0.6008189 0.5955584 0.6060795
# 2 0.4969652 0.4865961 0.5073344

# se si calcola la derivata e la si valuta in 7.5, si scopre che 
# di fatto sta chiedendo proprio un IC per il coeff beta_1, 
# perchè tutti gli altri termini sono zero

beta <- fit2$coefficients
beta
beta_1 <- beta[2]
beta_1
vcov(fit2)
dim(vcov(fit2))
sigma_beta_1 <- vcov(fit2)[2,2]
sigma_beta_1
alpha <- 0.1
qT <- qt(1-alpha/(2*k), fit2$df.residual) # fit2$df.residual = n-(r+1) = n-5
IC.derivative <- cbind(beta_1 - qT*sqrt(sigma_beta_1),
                       beta_1 + qT*sqrt(sigma_beta_1))
IC.derivative

# nelle soluzioni, ha posto alpha = 0.05 (anche se nel testo c'è
# scritto che alpha = 0.1)





# ADDITIONAL EXERCISES:

######-----------------------------


##### Problem 3 of 10/09/2010
#####--------------------------
# The file extra.txt reports the representation expenses [$] of the 
# English first minister and of his vice during the first 12 months of
# 2009. Assume those data to be independent realizations of a bivariate
# Gaussian.
# a) Build an ellipsoidal region of confidence 90% for the mean of the 
#    representation expenses
# b) Is there evidence of the fact that the prime minister spends in mean
#    more than twice the expences of its vice?
# c) Build a confidence interval of level 90% for the mean of the sum of
#    the expenses.


# a) Build an ellipsoidal region of confidence 90% for the mean of the 
#    representation expenses

extra <- read.table('extra.txt', header=T)
load("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/LAB_5/mcshapiro.test.RData")
mcshapiro.test(extra)
plot(extra)
dim(extra)
M <- sapply(extra, mean)
M
S <- cov(extra)
S
plot(extra, pch=19)
alpha = 0.1
n <- dim(extra)[1]
p <-2
cfr.fisher <- (n-1)*p/(n-p)*qf(1-alpha,p,n-p)
ellipse(center = M, shape = S/n, radius = sqrt(cfr.fisher))
points(M[1], M[2], col='red', pch=19)
# directions
eigen(S)$vector
# radius 
sqrt(cfr.fisher)
# lenght of the semi-axes
sqrt(eigen(x.cov/n)$values)*sqrt(cfr.fisher)

# b) Is there evidence of the fact that the prime minister spends in mean
#    more than twice the expences of its vice?
# m[1] < m[2]   vs    m[1]>=m[2]

# paired measuraments
attach(extra)
D <- primo - 2*vice
detach(extra)
head(D)
D.m <- mean(D)
shapiro.test(D)
D.m
D.cov = var(D)
D.cov
Dinv.cov = solve(D.cov)
Dinv.cov
p <- 1
F0 <- n*(D.m)*Dinv.cov*(D.m)
F0
p <- 1-pf((n-p)/((n-1)*p)*F0, 1, 11)
p
#  reject H0

# finora è SBAGLIATO

alpha <- 0.1
d = extra$primo - 2 * extra$vice
d
T0 <- (mean(d) - 0)/sqrt(var(d)/12)
T0
p <- 1 - pt(T0, n-1)

p_n <- 1 - pt(abs(T0), n-1) + pt(-abs(T0), n-1)
p_n
1-pf(F0, 1, 11)


# c) Build a confidence interval of level 90% for the mean of the sum of
#    the expenses.

alpha <- 0.1
M
n <- 12
qT <- qt(1-alpha/2, n-1)
S
a = c(1,1)
U <- t(a)%*%S%*%a
c(M[1]+M[2] - qT*sqrt(U/n), M[1]+M[2], M[1]+M[2] + qT*sqrt(U/n))

# alternatively:

lc <- extra[,1] + extra[,2]
t.test(lc, alternative = 'two.sided', mu = 0, conf.level = 0.90)




# Pb 2 of 26/02/2008
# The Warmer Bros produces gas boilers for domestic heating that
# exploit the chemical reaction of combustion CH4 + 2O2 -> CO2 + 2H2O. 
# For 40 Warmer Bros boilers were measured (gas.txt file) the quantities
# [kmol] of H2O, CO2 and CO contained in 1 m^3 of exhaust gas.
# a) Build three T2-simultaneous confidence intervals of global level 
#    90% for the mean of the three exhaust gas.
# The correct operation of the boilers assumes that the number of H2O 
# kmol is twice those of CO2 and that the number of kmol of CO is equal
# to 0.
# b) Is there statistical evidence that the Warmer Bros boilers may not
#    work properly?
# c) Maintaining a global confidence of 90%, together with the intervals
#    built in (a), provide additional T2-simultaneous intervals in 
#    support of the conclusions at point (b).


# a) Build three T2-simultaneous confidence intervals of global level 
#    90% for the mean of the three exhaust gas.

gas <- read.table('gas.txt')
n <- dim(gas)[1]
p <- dim(gas)[2]
head(gas)
mcshapiro.test(gas)
M <- as.vector(sapply(gas,mean))
M
k <- 3
alpha = 0.1
qT <- qt(1-alpha/(2*k), n-1)
S <- cov(gas)
S
s1 <- S[1,1]
s2 <- S[2,2]
s3 <- S[3,3]
IC.bonf <- rbind(c(M[1] - qT*sqrt(s1/n), M[1], M[1] + qT*sqrt(s1/n)),
            c(M[2] - qT*sqrt(s2/n), M[2],  M[2] + qT*sqrt(s2/n)),
            c(M[3] - qT*sqrt(s3/n), M[3],  M[3] + qT*sqrt(s3/n)))
IC.bonf
cfr.fisher <- qf(1-alpha, p, n-p) * (n-1)*p/(n-p)
IC.sim <- rbind(c(M[1] - sqrt(cfr.fisher*s1/n), M[1], M[1] + sqrt(cfr.fisher*s1/n)),
                 c(M[2] - sqrt(cfr.fisher*s2/n), M[2],  M[2] + sqrt(cfr.fisher*s2/n)),
                 c(M[3] - sqrt(cfr.fisher*s3/n), M[3],  M[3] + sqrt(cfr.fisher*s3/n)))
IC.sim

# b) Is there statistical evidence that the Warmer Bros boilers may not
#    work properly?
# test media di una gaussiana
# test per la media di una gaussiana
# test mean gaussian
# test for the mean of a gaussian
C <- cbind(c(1,-2,0),
           c(0,0,1))
C
d <- as.matrix(gas)%*%C
d
mcshapiro.test(d)
M1 <- colMeans(d)
M1
S <- cov(d)
S
Sinv <- solve(S)
n <- 40
T0 <- n*t(M1)%*%Sinv%*%M1
p <- 2
1 - pf(T0*(n-p)/(n*p-p), p, n-p)


# c) Maintaining a global confidence of 90%, together with the intervals
#    built in (a), provide additional T2-simultaneous intervals in 
#    support of the conclusions at point (b).

alpha <- 0.1
ìcfr.fisher <- qf(1-alpha, p, n-p) * (n-1)*p/(n-p)
a1 <- c(1,-2,0)
a2 <- c(0,0,1)
M <- as.vector(sapply(gas,mean))
M
s1 <- t(a1)%*%S%*%a1
s2 <- t(a2)%*%S%*%a2
IC.sim <- rbind(c(M %*% a1 - sqrt(cfr.fisher*s1/n), M %*% a1, M %*% a1 + sqrt(cfr.fisher*s1/n)),
                c(M %*% a2 - sqrt(cfr.fisher*s2/n), M %*% a2, M %*% a2 + sqrt(cfr.fisher*s2/n)))
IC.sim

# > ICT2.bis# test mean gaussian
# L        C         U
# 1  2.6677419  3.04275 3.4177581
# 2 -0.7881581 -0.38400 0.0201581






##### Problem 2 of 12/02/2008
#####--------------------------
# PoliTermos produces thermostats for the Italian market. During the last month
# some faulty thermostats have been introduced accidentally on the market;
# it is estimated that these are about 10% of sales. Lab test demonstrate that,
# in a 1°C temperature environment, the not defective thermostats detect a 
# temperature normally distributed, with mean 1 and variance 1/2, while
# the defective thermostats according to an exponential law with mean 1 and 
# variance 1. All the thermostats sold last month were recalled for possible 
# replacement and subjected to the previous test. Taking into account that the
# replacement of a thermostat in reality not defective produces damage to the company
# of 1 euro, and that the failure to replace a faulty thermostat creates a loss in 
# reputation estimated in 9 euros:
# a) Formulate a criterion for the replacement, based on the temperature measured 
#    during the test, which minimizes the damage expected by the company.
# Two thermostats of your property were recalled for inspection.
# The first during the experiment revealed a temperature of -1°C, the
# second equal to 3°C:
# b) Will the first thermostat be replaced? How likely is it defective?
# c) Will the second thermostat be replaced? How likely is it defective?
# LDA QDA REGIONI COLORATE 
# regioni colorate
# lda qda
# laboratorio 8
# posterior likelihood prior grafico
pf <- 0.1
pv <- 0.9
c.vf <- 9 # euro lo classifico come funzionante, ma non funziona
c.fv <- 1 # eurol o classifico come rotto, ma in realtà funziona

x11()
x <- seq(-2, 10, 0.001)
plot(  x, c.fv * pv * dnorm(x, 1, sqrt(1/2)), type='l', col='blue', ylim=c(0,1))
points(x, c.vf * pf * dexp(x, 1), type='l', col='red')


# b) Will the first thermostat be replaced? How likely is it defective?

points(-1,0, col='black', pch=19) # will be classified as functioning
# prob to be defective: 
pf * dexp(-1, 1)/(pv*dnorm(3, 1, sqrt(.5))+pf*dexp(3, 1))


# c) Will the second thermostat be replaced? How likely is it defective?

points(3,0, col='black', pch=19) # will be classified as defective
# prob to be defective: 
pf * dexp(3, 1) # SBAGLIATO

# NO --> CHIEDE LA POSTERIOR:
pf*dexp(3, 1)/(pv*dnorm(3, 1, sqrt(.5))+pf*dexp(3, 1))






##### Problem 2 of 10/02/10 
# The new fastfood chain Megaburger has made an experiment last month
# to choose the characteristics of his first advertising campaign. During the
# experiment 450 individuals were involved, selected in three different macro-regions
# (Europe, USA, Canada). They were asked to evaluate one of the following
# three types of burgers: Burger, Cheese-burger, Bacon-cheese-burger. The mmm.txt
# file contains the assessments (index of goodness from 0 to 10) for 450
# individuals.
# a) Using an additive ANOVA model with two factors, perform three tests
#    (each of level 1%) to verify that the distribution of index of goodness
#    (i) is not dependent on the macro-region, (ii) does not depend on the type
#    of burger, (iii) is not dependent on the macro-region or on the type of hamburger.
# b) Using six overall 95% confidence intervals for appropriate differences of means,
#    identify homogeneous groups of customers (in terms of the mean index of the goodness).
# c) Based on the above confidence intervals, identify which group (or groups) 
#    of customers is on average more satisfied and which less satisfied.

mmm <- read.table('mmm.txt', header=T)
head(mmm)
dim(mmm)
n <- 450
p <- 3

# a) Using an additive ANOVA model with two factors, perform three tests
#    (each of level 1%) to verify that the distribution of index of goodness
#    (i) is not dependent on the macro-region, (ii) does not depend on the type
#    of burger, (iii) is not dependent on the macro-region or on the type of hamburger.

index <- mmm$index
region <- as.factor(mmm$region)
sandwich <- as.factor(mmm$sandwich)

g <- length(levels(region)) # 2
b <- length(levels(sandwich)) # 2
g
b
M           <- mean(index)
Mregion      <- tapply(index,      region, mean)
Msand       <- tapply(index,       sandwich, mean)

# modello additivo
fit <- aov(index ~ region + sandwich )
summary.aov(fit)

shapiro.test(mmm[which(region=="Canada" & sandwich=="Bacon"),]$index)$p
shapiro.test(mmm[which(region=="Canada" & sandwich=="Burger"),]$index)$p
shapiro.test(mmm[which(region=="Canada" & sandwich=="Cheese"),]$index)$p
shapiro.test(mmm[which(region=="Europe" & sandwich=="Bacon"),]$index)$p
shapiro.test(mmm[which(region=="Europe" & sandwich=="Burger"),]$index)$p
shapiro.test(mmm[which(region=="Europe" & sandwich=="Cheese"),]$index)$p
shapiro.test(mmm[which(region=="USA" & sandwich=="Bacon"),]$index)$p
shapiro.test(mmm[which(region=="USA" & sandwich=="Burger"),]$index)$p
shapiro.test(mmm[which(region=="USA" & sandwich=="Cheese"),]$index)$p


# b) Using six overall 95% confidence intervals for appropriate differences of means,
#    identify homogeneous groups of customers (in terms of the mean index of the goodness).

k <- 6
alpha <- 0.05
SSres <- sum(residuals(fit)^2)
qT <- qt(1-alpha/(2*k), 450-3-3+1)
C <- which(region=="Canada")
E <- which(region=="Europe")
U <- which(region=="USA")
Ba <- which(sandwich=="Bacon")
Bu <- which(sandwich=="Burger")
Ch <- which(sandwich=="Cheese")
mE <- mean(mmm[E,1])
mC <- mean(mmm[C,1])
mU <- mean(mmm[U,1])
mBa <- mean(mmm[Ba,1])
mBu <- mean(mmm[Bu,1])
mCh <- mean(mmm[Ch,1])
infEC <- mE-mC - qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
supEC <- mE-mC + qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
infEU <- mE-mU - qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
supEU <- mE-mU + qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
infUC <- mU-mC - qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
supUC <- mU-mC + qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
n <- 50
g <- 3
W <- SSres
b <- 3
infBaconBurger <- mBa-mBu - qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
supBaconBurger <- mBa-mBu + qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
infCheeseBurger <- mCh-mBu - qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
supCheeseBurger <- mCh-mBu + qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
infCheeseBacon <- mBu-mBa - qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )
supCheeseBacon <- mBu-mBa + qT * sqrt( W/(g*b*n-g-b+1) * (1/150+1/150) )

IC2   <- data.frame(EC=c(infEC, supEC), EU=c(infEU, supEU), UC=c(infUC, supUC),
                    BaconBurger=c(infBaconBurger, supBaconBurger), CheeseBurger=c(infCheeseBurger, supCheeseBurger), CheeseBacon=c(infCheeseBacon, supCheeseBacon))
rownames(IC2) <- c('Inf','Sup')
IC2

matplot(1:(dim(IC2)[2]), t(IC2),pch='',axes=F,xlab='',ylab='')
for(i in 1:dim(IC2)[2])
  segments(i,IC2[1,i],i,IC2[2,i],lwd=2)
axis(1,at=1:dim(IC2)[2],labels=names(IC2), las=2, cex.axis=.8)
axis(2)
box()
abline(h=0)
points(1:(dim(IC2)[2]), sapply(IC2,mean),col='red',lwd=2)

# (c)

# Europa è meno soddisfatta del USA e Canada. USA e Canada sono soddisfatti allo stesso modo (IC ocntiene zero)
# 
# Bacon è più apprezzato del Burger, Chese è meno del Bacon e CHeese e Burger si equivalgono