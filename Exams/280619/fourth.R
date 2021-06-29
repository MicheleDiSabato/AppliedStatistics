# Problem n.4
# The file montserrat.txt collects the wind speeds (w(si)) registered on the 5th June 2019 at 134 measurement sites
# (si
#   , i = 1, ..., 134), in the region around the Montserrat (Spain). The dataset also reports the UTM coordinates
# of the measurement sites, and their Euclidean distance to the top of the mountain d(si) = ksi ??? s0k, with s0 =
#   (402476, 4605558).
# a) Estimate two empirical variograms, assuming the following models:
#   (M1) w(si) = a0 + ??(si);
#   (M2) w(si) = a0 + a1 · d(si) + ??(si).
#   Report a qualitative plot of the variograms. Comment the results and choose the model you deem more
#   appropriate for the observations.
#   b) Fit to the empirical variogram chosen at point (a), a spherical model without nugget, via weighted least
# squares. Report the estimates of sill and range. Comment the results.
# c) Estimate, via Generalized Least Squares, the parameter(s) a of the model chosen at point (a).
# d) Predict the wind speed at the top of the mountain, s0 = (402476, 4605558). Report the associated prediction
# variance.

# a) Estimate two empirical variograms, assuming the following models:
#   (M1) w(si) = a0 + ??(si);
#   (M2) w(si) = a0 + a1 · d(si) + ??(si).
#   Report a qualitative plot of the variograms. Comment the results and choose the model you deem more
#   appropriate for the observations.

data <- read.table('montserrat.txt',header=TRUE)
head(data)
coordinates(data) <- c('x','y')
head(data)

plot(data$distance, data$speed, pch=19, col="black")

v1 <- variogram(speed ~ 1, data = data)
plot(v1, main="stationary")
v2 <- variogram(speed ~ distance, data = data)
plot(v2, main="non stationary")
v.fit1 <- fit.variogram(v1, vgm(13, "Sph", 30))
plot(v1, v.fit1, pch = 19)
v.fit2 <- fit.variogram(v2, vgm(8, "Sph", 25, 1))
plot(v2, v.fit2, pch = 19)

# given the plot of speed vs distance, the second model is more appropriate

# b) Fit to the empirical variogram chosen at point (a), a spherical model without nugget, via weighted least
# squares. Report the estimates of sill and range. Comment the results.

v = v2
v.fit <- fit.variogram(v, vgm(8, "Sph", 25))
plot(v, v.fit, pch = 19)
v.fit

# COMMENT THE RESULTS:
# - the oscillations of the empirical variogram fro high values of h
#   are due to the estimator for the empirical variogram
# - the Spherical mode lseems appropriate, since, the behaviour
#   of the empirical variogram near 0 seems to be linear


# c) Estimate, via Generalized Least Squares, the parameter(s) a of the model chosen at point (a).

g.tr <- gstat(formula = speed ~ distance, data = data, model = v.fit)
# predict(g.tr, data[1,], BLUE = FALSE)
# predict(g.tr, data[2,], BLUE = FALSE)

x0.intercept = data.frame(x=4, y=0, distance = 0)
coordinates(x0.intercept) <- c('x','y')
x0.slope = data.frame(x=0, y=4, distance = 1)
coordinates(x0.slope) <- c('x','y')

predict(g.tr, x0.intercept, BLUE = TRUE)
predict(g.tr, x0.slope, BLUE = TRUE)



# d) Predict the wind speed at the top of the mountain, s0 = (402476, 4605558). Report the associated prediction
# variance.

x0.new = data.frame(x=4, y=0, distance = 0)
coordinates(x0.new) <- c('x','y')
predict(g.tr, x0.new, BLUE = FALSE)



