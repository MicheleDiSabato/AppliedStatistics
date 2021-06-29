# Problem n.1
# The Catalan Food Association has launched an award for the Best Catalan Tapa. As part of the challenge, two
# tasters are sent to evaluate the 35 finalist tapas in Terrassa and the 35 finalist tapas in Girona. Files terrassa.txt
# and girona.txt collect the evaluations on each of the finalist tapas given by the tasters in Terrassa and Girona,
# respectively. Assume the evaluations on different tapas to be independent, and the evaluations of the two tasters
# on the same tapa to come from a bivariate Gaussian distribution.
# a) Perform a statistical test of level 95% to verify if the mean evaluations in the two cities differ. State and verify
# the model assumptions.
# b) Interpret the results of the test at point (a) through two Bonferroni intervals of global level 95% for appropriate
# differences in the mean. Comment the result.
# c) Is there statistical evidence to state that, at level 95%, the average
# evaluations of Girona's tapas are in mean
# higher than those of Terrassa's tapas?
# [by average evaluation of a tapa is meant the one obtained by averaging the evaluations of the two tasters on
# that tapa]

# a) Perform a statistical test of level 95% to verify if the mean evaluations in the two cities differ. State and verify
# the model assumptions.

terrassa <- read.table('terrassa.txt', header=TRUE)
head(terrassa)
girona <- read.table('girona.txt', header=TRUE)
head(girona)
mcshapiro.test(terrassa)$p
mcshapiro.test(girona)$p

x11()
plot(terrassa, pch=19, col="red", xlim=c(66,100))
points(girona, pch=19, col="blue")

n1 = dim(terrassa)[1]
n2 = dim(girona)[1]
p = 2
t1.mean = sapply(terrassa, mean)
t2.mean = sapply(girona, mean)
t1.cov = cov(terrassa)
t2.cov = cov(girona)
x11()
par(mfrow=c(1,2))
image(1:2,1:2,t1.cov)
image(1:2,1:2,t2.cov)
S1 = t1.cov
S2 = t2.cov
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
round(t1.cov, digits=4)
round(t2.cov, digits=4)
# accept omoschedasticity and normality

p = 2
Sp = ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
alpha = 0.05
delta.0 = c(0,0)
Spinv = solve(Sp)
T2 = n1*n2/(n1+n2)*(t1.mean-t2.mean) %*% Spinv %*% (t1.mean-t2.mean)
cfr.fisher = p*(n1+n2-2)/(n1+n2-1-p) * qf(1.-alpha, p, n1+n2-1-p)
w = p*(n1+n2-2)/(n1+n2-1-p)
P = 1-pf(T2/w,p,n1+n2-p-1)
P

x11()
D = data.frame(T1 = terrassa$T1-girona$T1,
               T2 = terrassa$T2-girona$T2)
plot(D, pch=19,xlim=c(-20,3))
ellipse(center=t1.mean-t2.mean, shape=Sp*(1/n1+1/n2), radius=sqrt(cfr.fisher))
points(0,0, pch=19, col="red")
abline(v=-10.397420)
abline(v=-6.95994)
abline(h=-4.848632)
abline(h=-1.13994)
# reject H0 ---> the means are different


# b) Interpret the results of the test at point (a) through two Bonferroni intervals of global level 95% for appropriate
# differences in the mean. Comment the result.

k = p
alpha = 0.05
qT = qt(1-alpha/(2*k), n1+n2-2)
IC.BF.1 = c(t1.mean[1]-t2.mean[1] - qT * sqrt(Sp[1,1]*(1/n1+1/n2)),
            t1.mean[1]-t2.mean[1],
            t1.mean[1]-t2.mean[1] + qT * sqrt(Sp[2,2]*(1/n1+1/n2)))
IC.BF.2 = c(t1.mean[2]-t2.mean[2] - qT * sqrt(Sp[2,2]*(1/n1+1/n2)),
            t1.mean[2]-t2.mean[2],
            t1.mean[2]-t2.mean[2] + qT * sqrt(Sp[2,2]*(1/n1+1/n2)))
IC.BF = rbind(IC.BF.1,
              IC.BF.2)
dimnames(IC.BF)[[2]] = c("inf", "center", "sup")
IC.BF


x11()
D = data.frame(T1 = terrassa$T1-girona$T1,
               T2 = terrassa$T2-girona$T2)
plot(D, pch=19,xlim=c(-20,3))
ellipse(center=t1.mean-t2.mean, shape=Sp*(1/n1+1/n2), radius=sqrt(cfr.fisher))
points(0,0, pch=19, col="red")
abline(v=-10.397420)
abline(v=-6.95994)
abline(h=-4.848632)
abline(h=-1.13994)

# the reason why mu1 != mu2 lies in both the components, that is both testers
# found that on average the two populations are different


# c) Is there statistical evidence to state that, at level 95%, the average
# evaluations of Girona's tapas are in mean
# higher than those of Terrassa's tapas?
# [by average evaluation of a tapa is meant the one obtained by averaging the evaluations of the two tasters on
# that tapa]

# up to now, 1=terrassa, 2=girona. Now we need to switch the labels
temp = (girona$T1+girona$T2)/2
data1 = temp
temp = (terrassa$T1+terrassa$T2)/2
data2 = temp
m1 = mean(data1)
m2 = mean(data2)
a = c(1/2, 1/2)
S = t(a)%*%Sp%*%a
T2 = (m1-m2)/sqrt(S*(1/n1 + 1/n2))
rifiuto = T2 > qt(1-alpha/2, n1+n2-2)
rifiuto

T22 = (m2-m1)/sqrt(S*(1/n1 + 1/n2))
rifiuto2 = T22 < qt(alpha/2, n1+n2-2)
rifiuto2
