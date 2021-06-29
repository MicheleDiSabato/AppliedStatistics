# Problem n.2
# The file activity.txt collects the results of an experiment carried out with a group of subjects performing some
# daily activities (walking, sitting, laying) while carrying a smartphone with embedded inertial sensors. The dataset
# reports the mean linear acceleration (estimated from the signal recorded by the accelerometer) and the mean
# angular velocity (estimated from the signal recorded by the gyroscope). Knowing that, on the average day, the
# typical smartphone user spends 3 hours walking, 12 hours sitting and 9 hours laying, answer the following questions.
# a) Build a classifier for the variable activity based on the available quantitative features. Report the mean within
#   the groups identified by the variable activity and a plot of the classification regions. Introduce and verify the
#   appropriate assumptions.
# (b) Compute the APER of the classifier.
# (c) If the sensors record a mean body linear acceleration of 0.45 and a mean angular velocity of 0.52, what activity
#   would the subject be performing according to the classifier built at point (a)?
# (d) Build a k-nearest neighbor classifier for the activity choosing the parameter k equal to 5. Report a plot of the
#   classification regions and compute the error rate on the training set. Compare the results with points (a) and
#   (b) and comment on the performances of the classifiers.


data <- read.table('activity.txt', header=TRUE)
head(data)

# a) Build a classifier for the variable activity based on the available quantitative features. Report the mean within
# the groups identified by the variable activity and a plot of the classification regions. Introduce and verify the
# appropriate assumptions.

data$activity
table(data$activity)
i1 = which(data$activity == "walking")
i2 = which(data$activity == "sitting")
i3 = which(data$activity == "laying")
n1 = length(i1)
n2 = length(i2)
n3 = length(i3)
n = n1+n2+n3

# mcshapiro.test(data[i1,1:2])$p
# mcshapiro.test(data[i2,1:2])$p
# mcshapiro.test(data[i3,1:2])$p

lab = data[,3]
colore = as.factor(lab)
data = data[,1:2]
S1 = cov(data[i1,])
S2 = cov(data[i2,])
S3 = cov(data[i3,])
round(S1, digits = 4)
round(S2, digits = 4)
round(S3, digits = 4)
x11()
par(mfrow=c(1,3))
image(S1, col=heat.colors(100),main='Cov. SA', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. SB', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))
image(S3, col=heat.colors(100),main='Cov. SA', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2,S3), (0:100)/100, na.rm=TRUE))

# normality, ma non sembra esserci omoschedasticity: uso qda


data.qda = qda(data,lab)
data.qda

x11()
plot(data, pch=19,main="qda")
points(data[i1,], col="red", pch=19)
points(data[i2,], col="blue", pch=19)
points(data[i3,], col="green", pch=19)
legend("bottomright", c("walking","sitting","laying"), col=c("red", "blue","green"),lty=1)
x = seq(min(data[,1]), max(data[,1]), length=400)
y = seq(min(data[,2]), max(data[,2]), length=400)
xy = expand.grid(accel=x,gyro=y)
z = predict(data.qda,xy)$post
z1 = z[,1]-pmax(z[,2],z[,3])
z2 = z[,2]-pmax(z[,1],z[,3])
z3 = z[,3]-pmax(z[,1],z[,2])
contour(x,y,matrix(z1,400),levels=0,drawlabels = F,add=T)
contour(x,y,matrix(z2,400),levels=0,drawlabels = F,add=T)
contour(x,y,matrix(z3,400),levels=0,drawlabels = F,add=T)


# data.qda = lda(data,lab)
# data.qda
# 
# x11()
# plot(data, pch=19,main="lda")
# points(data[i1,], col="red", pch=19)
# points(data[i2,], col="blue", pch=19)
# points(data[i3,], col="green", pch=19)
# legend("bottomright", c("walking","sitting","laying"), col=c("red", "blue","green"),lty=1)
# x = seq(min(data[,1]), max(data[,1]), length=400)
# y = seq(min(data[,2]), max(data[,2]), length=400)
# xy = expand.grid(accel=x,gyro=y)
# z = predict(data.qda,xy)$post
# z1 = z[,1]-pmax(z[,2],z[,3])
# z2 = z[,2]-pmax(z[,1],z[,3])
# z3 = z[,3]-pmax(z[,1],z[,2])
# contour(x,y,matrix(z1,400),levels=0,drawlabels = F,add=T)
# contour(x,y,matrix(z2,400),levels=0,drawlabels = F,add=T)
# contour(x,y,matrix(z3,400),levels=0,drawlabels = F,add=T)

# (b) Compute the APER of the classifier.

table(rue=lab, assigned=predict(data.qda)$class)
aper=(8+1+8)/n
aper


# (c) If the sensors record a mean body linear acceleration of 0.45 and a mean angular velocity of 0.52, what activity
#   would the subject be performing according to the classifier built at point (a)?

x0.new=data.frame(accel=0.45,gyro=0.52)
predict(data.qda,x0.new)
points(x0.new,pch=19,col="black")


# d) Build a k-nearest neighbor classifier for the activity choosing the parameter k equal to 5. Report a plot of the
# classification regions and compute the error rate on the training set. Compare the results with points (a) and
# (b) and comment on the performances of the classifiers.


k = 5
data.knn = knn(train=data, test=xy, cl=lab, prob=T, k=5)
z=as.numeric(data.knn)
table(z)
x11()
plot(data, pch=19,main="knn")
points(data[i1,], col="red", pch=19)
points(data[i2,], col="blue", pch=19)
points(data[i3,], col="green", pch=19)
legend("bottomright", c("walking","sitting","laying"), col=c("red", "blue","green"),lty=1)
contour(x,y,matrix(z,400),levels=c(1.5,2.5),drawlabels = F,add=T)

data.knn = knn(train=data, test=data, cl=lab, prob=T, k=5)
z=as.numeric(data.knn)
table(rue=lab, assigned=z)
aper = (3+8+4)/n
aper
