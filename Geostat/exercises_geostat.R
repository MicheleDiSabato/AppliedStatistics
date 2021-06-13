
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics

## Set working directory 
setwd("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/Geostat")
## Functions for graphics 
v.f <- function(x, ...){100-cov.spatial(x, ...)}
v.f.est<-function(x,C0, ...){C0-cov.spatial(x, ...)}





# One of the most relevant consequences of the eruption of volcan 
# Eyjafjoll (in Iceland), in 2010, is the contamination by fluoride. 
# The latter is due to the deposit on the ground of the ash released
# in the atmosphere during the eruption.
# The file "fluoruro.txt" reports the coordinates of 50 measurement sites
# s_i, i=1,...,50, the corresponding concentrations of fluoride (ppm) F(s_i)
# and the distance D.s_i of each site s_i from the crater of the volcano.
# Denoting by delta a zero-mean, second-order stationary and isotropic random
# field:
# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+delta(s_i) and 
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.
# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.
# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.
# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?
# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970) 
# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010.


# a) Estimate two empirical variograms, assuming the following models:
#    F(s_i)=beta_0+delta(s_i) and 
#    F(s_i)=beta_0+beta_1*D.s_i+delta(s_i). 
#    Choose the most appropriate model for the observations.

data=read.table('fluoruro.txt')
head(data)
coordinates(data)=c('X','Y')
head(data)

svgm1 <- variogram(Conc.ppm ~ 1, data) # 2nd order stat and isotropy
plot(svgm1, main = 'Sample Variogram',pch=19)

svgm2 <- variogram(Conc.ppm ~ D, data) # 2nd order stat and isotropy
plot(svgm2, main = 'Sample Variogram',pch=19)

bubble(data,'Conc.ppm')
plot(data$Conc.ppm, data$D)

# scelgo svmg2

# b) Fit to the empirical variogram at point (a), a Gaussian model
#    without nugget, via weighted least squares. Use as initial parameters: 
#    sill=100, range=0.02. Report the estimates of sill and range.

v <- svgm2
v.fit.Gau <- fit.variogram(v, vgm(100, "Gau", 0.02))
plot(v, v.fit.Gau, pch = 19)


# c) Fit to the empirical variogram chosen at point (a), a spherical model
#    without nugget, via weighted least squares. Report the estimates of sill 
#    and range.

v <- svgm2
v.fit.Sph <- fit.variogram(v, vgm(100, "Sph", 0.02))
plot(v, v.fit.Sph, pch = 19)
v.fit.Sph$psill
v.fit.Sph$range


# d) Compare the variograms estimated at points (b) and (c), with the empirical
#    variogram at point (a). Given that the ash deposition is known to be
#    a very regular phenomenon, which variogram model is the most appropriate?
plot(v, v.fit.Gau, pch = 19)
plot(v, v.fit.Sph, pch = 19)

# since it is very regular, Gaussian is better


# e) Based on model (d), estimate the concentration of fluoride due to the eruption
#    in the city of Raufarhofn (s0 = (0.3; 0.24), D.s0 = 0.1970) 

meuse.gstat <- gstat(formula = Conc.ppm ~ D,
                     data = data, model=v.fit.Gau)
meuse.gstat
# v.gls<-variogram(meuse.gstat)
# plot(v.gls)
# v.gls.fit <- fit.variogram(v.gls, vgm(100, "Gau", 0.02))
# plot(v.gls, v.gls.fit, pch = 19)
# meuse.gstat <- gstat(id = 'Conc.ppm', formula = Conc.ppm ~ D,
#                      data = data, nmax = 50, model=v.gls.fit, set = list(gls=1))
s0.new=data.frame(X=0.3, Y=0.24, D = 0.1970) # UTM coordinates
coordinates(s0.new)=c('X','Y')
# s0.vec <- as.vector(slot(s0.new,'coords'))
# s0.new <- as(s0.new, 'SpatialPointsDataFrame')
# s0.new
predict(meuse.gstat, s0.new)


# f) Based on model (d), estimate the concentration of fluoride at the same
#    location, due to a possible new eruption of equivalent intensity, independent 
#    of that of 2010.

predict(meuse.gstat, s0.new,BLUE = TRUE)







# The file radioville.txt reports the information on 158 control units
# in the area around the nuclear power plant of Radioville.
# At each site, available data consist of: radioactivity levels [Bq],
# longitude [?N], latitude [?W] and type of soil [urban/vegetation].
# Denoting by s_i the i-th site, by R the radioactivity level,
# by eps a weakly stationary random field and by D a dummy 
# urban/vegetation:

# a) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram without nugget, estimated
#    via weighted least squares;
# b) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram with nugget, estimated
#    via weighted least squares;
# c) choose the best variogram model by comparing the fitted model 
#    with the corresponding empirical variograms (report qualitative
#    plots and the estimated variogram parameters)
# d) on the basis of model (c), predict the radioactivity level at the
#    parking lot of the shopping centre of Radioville (lon = 78.59, 
#    lat = 35.34), and in the park of Radioville (lon = 77.6, 
#    lat = 34.99);
# e) estimate variance of prediction error at the same locations
#    as at point d).


# a) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram without nugget, estimated
#    via weighted least squares;

data <- read.table('radioville.txt',header=TRUE)
dummy <- ifelse(data$D=="U",0,1)
data <- data.frame(Lat=data$Lat, Long=data$Long, Bq=data$Bq, D=dummy)
head(data)
coordinates(data) <- c('Lat', 'Long')
head(data)

v1 <- variogram(Bq ~ D, data) # 2nd order stat and isotropy
plot(v, main = 'Sample Variogram',pch=19)
v.fit1 <- fit.variogram(v, vgm(1, "Sph", 0.5))
plot(v1, v.fit1, pch = 3)
v.fit1

g.tr <- gstat(formula = Bq ~ D, data = data, model = v.fit1)
pred <- predict(g.tr, data[6,],BLUE=TRUE)
beta_0 <- pred$var1.pred
pred <- predict(g.tr, data[1,],BLUE=TRUE)
beta_1 <- pred$var1.pred - beta_0

# b) estimate the parameters of the linear model 
#    R(s_i) = beta_0 + beta_1 D(s_i) + eps(s_i) 
#    assuming for eps a spherical variogram with nugget, estimated
#    via weighted least squares;

v2 <- variogram(Bq ~ D, data) # 2nd order stat and isotropy
plot(v2, main = 'Sample Variogram',pch=19)
v.fit2 <- fit.variogram(v2, vgm(1, "Sph", 0.5,0.1))
plot(v2, v.fit2, pch = 3)
v.fit2

g.tr <- gstat(formula = Bq ~ D, data = data, model = v.fit2)
pred <- predict(g.tr, data[6,],BLUE=TRUE)
beta_0 <- pred$var1.pred
pred
pred <- predict(g.tr, data[1,],BLUE=TRUE)
beta_1 <- pred$var1.pred - beta_0
pred


# c) choose the best variogram model by comparing the fitted model 
#    with the corresponding empirical variograms (report qualitative
#    plots and the estimated variogram parameters)

plot(v1,v.fit1)
plot(v2,v.fit2)

# scelgo il primo perchè tra i due non cambia molto e non avere il nugget è meglio


# d) on the basis of model (c), predict the radioactivity level at the
#    parking lot of the shopping centre of Radioville (lon = 78.59, 
#    lat = 35.34), and in the park of Radioville (lon = 77.6, 
#    lat = 34.99);

meuse.gstat <- gstat(formula = Bq ~ D,
                     data = data, model=v.fit1)
meuse.gstat
s0.new1=data.frame(Lat=35.34,Long=78.59, D = 0) # UTM coordinates
s0.new2=data.frame(Lat=34.99, Long=77.6, D = 1) # UTM coordinates
coordinates(s0.new1)=c('Lat','Long')
coordinates(s0.new2)=c('Lat','Long')

predict(meuse.gstat, s0.new1)
predict(meuse.gstat, s0.new2)

