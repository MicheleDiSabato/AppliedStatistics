# Problem n.2
# The file profiling.txt collects the data about the habits of 1067 people in the city of Terrassa, measured on the
# 4th June 2019 through the smartphone application FollowYou. The dataset reports the habits of the users that
# declared to be either tourists or residents, in terms of the time t1 [minutes] spent walking in the city centre around
# Plaza Grande, and the time t2 [minutes] spent waiting for public transportation. The provider asks for your help
# to build a classifier for profiling new users, in order to provide personalized advertisements.
# a) Build a classifier for the variable type of user based on the available quantitative features. Report the model
# for the data, the estimates of its parameters (means and covariances), the priors within the groups and verify
# the model assumptions. Report a qualitative plot of the classification regions.
# b) Compute the APER of the classifier.
# c) How would you profile a new user with t1 = 35 min and t2 = 3 min?

# a) Build a classifier for the variable type of user based on the available quantitative features. Report the model
# for the data, the estimates of its parameters (means and covariances), the priors within the groups and verify
# the model assumptions. Report a qualitative plot of the classification regions.

profiling <- read.table('profiling.txt', header=TRUE)
head(profiling)

data = profiling[,1:2]
lab = factor(profiling$type)
head(data)
lab
i1 = which(lab == "tourist")
i2 = which(lab == "resident")
n1 = length(i1)
n2 = length(i2)
n = n1+n2
turisti = data[i1,]
residenti = data[i2,]

# mcshapiro.test(turisti)$p
# mcshapiro.test(residenti)$p

x11()
S1 = cov(turisti)
S2 = cov(residenti)
par(mfrow=c(1,2))
image(S1, col=heat.colors(100),main='Cov. S1', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
image(S2, col=heat.colors(100),main='Cov. S2', asp=1, axes = FALSE, breaks = quantile(rbind(S1,S2), (0:100)/100, na.rm=TRUE))
round(S1, digits=4)
round(S2, digits=4)
# c'è normalità, ma non omoschedasticità, procedo con QDA

data.qda = qda(data,lab)
data.qda
data.lda = lda(data,lab)
S1
S2

x11()
plot(data, pch=19, main="QDA")
points(residenti,col="red", pch=19)
points(turisti,col="blue", pch=19)
points(35,5, col="black",pch=19)
legend("bottomright",legend=c("residenti", "tursiti"), col=c("red","blue"),lty=19:19)
x = seq(min(data[,1]), max(data[,1]), length=200)
y = seq(min(data[,2]), max(data[,2]), length=200)
xy = expand.grid(t1 = x, t2 = y)
z = predict(data.qda,xy)$post
z1 = z[,1] - z[,2]
z2 = z[,2] - z[,1]
contour(x,y,matrix(z1,200), levels=0, drawlabels = F, add=T)
contour(x,y,matrix(z2,200), levels=0, drawlabels = F, add=T)

# x11()
# plot(data, pch=19, main="LDA")
# points(residenti,col="red", pch=19)
# points(turisti,col="blue", pch=19)
# x = seq(min(data[,1]), max(data[,1]), length=200)
# y = seq(min(data[,2]), max(data[,2]), length=200)
# xy = expand.grid(t1 = x, t2 = y)
# z = predict(data.lda,xy)$post
# z1 = z[,1] - z[,2]
# z2 = z[,2] - z[,1]
# contour(x,y,matrix(z1,200), levels=0, drawlabels = F, add=T)
# contour(x,y,matrix(z2,200), levels=0, drawlabels = F, add=T)

# b) Compute the APER of the classifier.

# the priors are non informative:
misc = table(true = lab, assigned = predict(data.qda, data)$class)
misc
aper.qda = (10+50)/n
aper.qda

misc = table(true = lab, assigned = predict(data.lda, data)$class)
misc
aper.lda = (80+0)/n
aper.lda


# c) How would you profile a new user with t1 = 35 min and t2 = 3 min?

x_new = data.frame(t1 = 35, t2 = 3)
predict(data.qda, x_new)
predict(data.qda, x_new)$class
predict(data.qda, x_new)$posterior

predict(data.lda, x_new)
# sensato: spende motlo tempo a passeggiare in centro e poco tempo ad aspettare i mezzi




















































