# Problem n.2
# In a study to develop an automated vision system able to distinguish between three species of rice, 60 images of
# rice grains were processed to obtain two morphological features for each grain of rice: the major axis length and
# the eccentricity. The data are reported in the file rice.txt.
# a) Use a hierarchical clustering method (Euclidean distance and complete linkage) to identify the three species of
# rice. Provide the plot of the dendrogram. Report the number of data within each cluster, the mean within the
# clusters and a plot of the results.

rice <- read.table('rice.txt', header=TRUE)
head(rice)
dim(rice)
n=dim(rice)[1]
p=dim(rice)[2]
d = dist(rice,method="euclidean")
x11()
image(1:n,1:n,as.matrix(d), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
# let's try without permutations
rice_clusters <- hclust(d, method='complete')
plot(rice_clusters, labels=FALSE)
# plot(1,1)
# misc=sample(n)
# rice=rice[misc,]
# d = dist(rice,method="euclidean")
# x11()
# image(1:n,1:n,as.matrix(d), main='metrics: Euclidean', asp=1, xlab='i', ylab='j')
# rice_clusters <- hclust(d, method='complete')
# plot(rice_clusters, labels=FALSE)
rect.hclust(rice_clusters, k=3)
cl = cutree(rice_clusters, k=3)
names(cl)
cl
table(cl)
n1 = table(cl)[1]
n2 = table(cl)[2]
n3 = table(cl)[3]
n1+n2+n3
i1 = which(as.factor(cl)==1)
i2 = which(as.factor(cl)==2)
i3 = which(as.factor(cl)==3)
g1 = rice[i1,]
g2 = rice[i2,]
g3 = rice[i3,]
m1 = colMeans(g1)
m2 = colMeans(g2)
m3=colMeans(g3)
plot(rice, pch=19)
points(g1, col='red')
points(g2, col='green')
points(g3, col='blue')

# b) Evaluate the performances of the clustering method at point (a) and identify the possible issues (if any). Propose,
# if needed, an alternative clustering method to cluster the data (report the number of data within each cluster,
# the mean within the clusters and a plot of the results).

coph <- cophenetic(rice_clusters)
cor(coph,d)

rice_clusters2 <- hclust(d, method='average')
plot(rice_clusters2, labels=FALSE)
rect.hclust(rice_clusters2, k=3)
cl = cutree(rice_clusters2, k=3)
names(cl)
cl
table(cl)
n1 = table(cl)[1]
n2 = table(cl)[2]
n3 = table(cl)[3]
n1+n2+n3
i1 = which(as.factor(cl)==1)
i2 = which(as.factor(cl)==2)
i3 = which(as.factor(cl)==3)
g1 = rice[i1,]
g2 = rice[i2,]
g3 = rice[i3,]
m1 = colMeans(g1)
m2 = colMeans(g2)
m3=colMeans(g3)
plot(rice, pch=19)
points(g1, col='red')
points(g2, col='green')
points(g3, col='blue')

# c) Provide Bonferroni intervals (global level 95%) for the means and the variances of the major axis length of the
# rice grains within each of the three clusters identified. Introduce and verify the appropriate assumptions.

mcshapiro.test(g1)
mcshapiro.test(g2)
mcshapiro.test(g3)
S1 = cov(g1)
S2 = cov(g2)
S3 = cov(g3)
x11()
par(mfrow=c(1,3))
image(S1)
image(S2)
image(S3)
round(S1, digits=4)
round(S2, digits=4)
round(S3, digits=4)

a <- c(1,0)
alpha=0.05
k=6
qT1 = qt(1-alpha/(2*k), n1-1)
qT2 = qt(1-alpha/(2*k), n2-1)
qT3 = qt(1-alpha/(2*k), n3-1)
M1 = t(a)%*%m1
s1 = t(a)%*%S1%*%a
M2 = t(a)%*%m2
s2 = t(a)%*%S2%*%a
M3 = t(a)%*%m3
s3 = t(a)%*%S3%*%a

IC_means = rbind(c(M1 - qT1*sqrt(s1/n1), M1 + qT1*sqrt(s1/n1)),
                 c(M2 - qT2*sqrt(s2/n2), M2 + qT1*sqrt(s2/n2)),
                 c(M3 - qT3*sqrt(s3/n3), M3 + qT3*sqrt(s3/n3)))
IC_means
chi1_1 = qchisq(alpha/(2*k), n1-1)
chi2_1 = qchisq(1-alpha/(2*k), n1-1)
chi1_2 = qchisq(alpha/(2*k), n2-1)
chi2_2 = qchisq(1-alpha/(2*k), n2-1)
chi1_3 = qchisq(alpha/(2*k), n3-1)
chi2_3 = qchisq(1-alpha/(2*k), n3-1)
IC_sigma = rbind(c((n1-1)*s1/chi2_1, (n1-1)*s1/chi1_1),
                 c((n2-1)*s2/chi2_2, (n2-1)*s2/chi1_2),
                 c((n3-1)*s3/chi2_3, (n3-1)*s3/chi1_3))
IC_sigma
