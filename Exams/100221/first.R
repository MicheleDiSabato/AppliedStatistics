# # first
# The file shopping.txt contains information on the usage of an online store selling clothing in the last 24 months.
# For each month, it reports the number of accesses to the online store, the number of purchases of men's clothing
# and the number of purchases of women's clothing. Assuming each month to be independent of the others, answer
# the following questions.
# a) Build a confidence region (level 95%) for the mean of the vector whose components are the number of accesses
# to the online store, the number of purchases of men's clothing and the number of purchases of women's clothing.
# Characterize the region by reporting its mathematical expression, its center, the direction of the axes and the
# length of the semi-axes. Introduce and test the hypothesis of Gaussianity; identify and discuss possible issues
# (if any).

shopping <- read.table('shopping.txt', header=TRUE)
head(shopping)
dim(shopping)
n = 24 
p = 3
M = colMeans(shopping)
S = cov(shopping)
open3d()
plot3d(shopping)
points3d(M[1], M[2], M[3], col='red', pch=19)
alpha=0.05
cfr.fisher = (n-1)*p/(n-p)*qf(1-alpha, p, n-p)
plot3d(ellipse3d(S/n, centre=as.vector(M), t = sqrt(cfr.fisher)), col = "green", add=T)
eigen(S)$vec
r = sqrt(cfr.fisher)
r
sqrt(eigen(S/n)$values)*r

mcshapiro.test(shopping)

# b) Build four T2-simultaneous confidence intervals (level 95%) for: the mean number of accesses to the online
# store, the mean number of purchases of men's clothing, the mean number of purchases of women's clothing and
# the mean number of total purchases.

head(shopping)
a <- c(1,0,0)
M_a = t(a)%*%M
S_a = t(a)%*%S%*%a
IC = cbind(M_a - sqrt(cfr.fisher)*sqrt(S_a/n), M_a + sqrt(cfr.fisher)*sqrt(S_a/n))
IC
a <- c(0,1,0)
M_a = t(a)%*%M
S_a = t(a)%*%S%*%a
IC = cbind(M_a - sqrt(cfr.fisher)*sqrt(S_a/n), M_a + sqrt(cfr.fisher)*sqrt(S_a/n))
IC
a <- c(0,0,1)
M_a = t(a)%*%M
S_a = t(a)%*%S%*%a
IC = cbind(M_a - sqrt(cfr.fisher)*sqrt(S_a/n), M_a + sqrt(cfr.fisher)*sqrt(S_a/n))
IC
a <- c(1,1,1)
M_a = t(a)%*%M
S_a = t(a)%*%S%*%a
IC = cbind(M_a - sqrt(cfr.fisher)*sqrt(S_a/n), M_a + sqrt(cfr.fisher)*sqrt(S_a/n))
IC

# c) Perform a test of level 95% to verify the hypothesis according to which, in mean, more than 20% of the accesses
# to the online store result in a purchase. Report the p-value of the test.

a=c(-0.2,1,1)
M_a=t(a)%*%M
S_a=t(a)%*%S%*%a
n=24
T0 = M_a/sqrt(S_a/n)
alpha=0.05
T0 > qt(1-alpha/2, n-1)
1-pt(T0, n-1)
