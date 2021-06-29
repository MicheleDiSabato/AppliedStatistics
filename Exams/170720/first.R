
# Problem n.1
# In order to design a new smart-office sensing device, a study is conducted aiming at detecting the occupancy of
# an office room from environmental measurements. The file occupancy.txt reports the measurements of relative
# humidity (in percentage) and CO2 concentration (in hundreds of ppm) collected in an office room at 100 time
# instants randomly distributed over the 24 hours of a working day. Moreover, a binary label indicating the occupancy
# of the room (0 for not occupied, 1 for occupied status) is reported in the file.
# a) Assuming that the office room is occupied on average 9 hours a day, build a classifier for the room occupancy
# that minimizes the expected number of misclassifications. Specify the model and verify the model assumptions.
# Report the estimates of the model parameters and a plot of the classification regions.
# b) Compute the APER of the classifier.
# c) Use the classifier to classify a new measurement with 26% of humidity and 900ppm of CO2.
# d) Build a k-nearest neighbor classifier for the room occupancy choosing the parameter k equal to 5. Report a plot
# of the classification regions and compute the error rate on the training set. Compare the results with points (a)
# and (b) and comment on the performances of the classifiers.




# a) Assuming that the office room is occupied on average 9 hours a day, build a classifier for the room occupancy
# that minimizes the expected number of misclassifications. Specify the model and verify the model assumptions.
# Report the estimates of the model parameters and a plot of the classification regions.

# 0 = free; 1 = occupied
data <- read.table('occupancy.txt', header=TRUE)
head(data)
data

lab = data$X
data = data[,1:2]
head(data)
prior = c(15/24, 9/24)
i1 = which(lab==0)
i2 = which(lab==1)
n1 =length(i1)
n2=length(i2)
n=n1+n2
occupied = data[i2,]
free = data[i1,]
x11()
plot(data)
points(occupied, col="red", pch=19)
points(free, col="green", pch=19)

# mcshapiro.test(free)$p
# mcshapiro.test(occupied)$p
S1 = cov(free)
S2 = cov(occupied)
round(S1, digits = 4)
round(S2, digits = 4)
x11()
SA=S1
SB=S2
par(mfrow=c(1,2))
image(SA, col=heat.colors(100),main='Cov. SA', asp=1, axes = FALSE, breaks = quantile(rbind(SA,SB), (0:100)/100, na.rm=TRUE))
image(SB, col=heat.colors(100),main='Cov. SB', asp=1, axes = FALSE, breaks = quantile(rbind(SA,SB), (0:100)/100, na.rm=TRUE))

# proceed with QDA

data.qda = qda(data, lab, prior=prior)
data.qda

x11()
plot(data, pch=19,main="QDA")
points(occupied, col="red", pch=19)
points(free, col="green", pch=19)
legend("bottomright", c("occupied","free"), col=c("red","green"),lty=1)
x = seq(min(data[,1]), max(data[,1]), length=400)
y = seq(min(data[,2]), max(data[,2]), length=400)
xy = expand.grid(Humidity=x,CO2=y)
z = predict(data.qda,xy)$post
z1 = z[,1]-z[,2]
contour(x,y,matrix(z1,400),levels=0,drawlabels = F,add=T)



# b) Compute the APER of the classifier.

misc = table(true=lab, assigned = predict(data.qda, data)$class)
misc
aper = 0
for (g in 1:2)
  aper = aper + sum(misc[g,-g])/sum(misc[g,])*prior[g]
aper


# c) Use the classifier to classify a new measurement with 26% of humidity and 900ppm of CO2.

xnew = data.frame(Humidity=26, CO2 = 9)
points(xnew, pch=19, col="black",lwd=4)
predict(data.qda, xnew)



# d) Build a k-nearest neighbor classifier for the room occupancy choosing the parameter k equal to 5. Report a plot
# of the classification regions and compute the error rate on the training set. Compare the results with points (a)
# and (b) and comment on the performances of the classifiers.

data.knn.train = knn(train=data, test=data, cl=lab, k=5,prob=T)
data.knn.test = knn(train=data, test=xy, cl=lab, k=5,prob=T)
z=as.numeric(data.knn.test)
table(z)
x11()
plot(data, pch=19,main="knn")
points(occupied, col="red", pch=19)
points(free, col="green", pch=19)
legend("bottomright", c("occupied","free"), col=c("red","green"),lty=1)
contour(x,y,matrix(z,400),levels=c(1.5,2.5),drawlabels = F,add=T)


z=as.numeric(data.knn.train)
table(rue=lab, assigned=z)
aper = 2/40 * 9/24 + 4/60 * 15/24
aper



















































































