# Problem n.4
# A satellite carrying an infrared spectrometer is collecting data of the surface of an asteroid in order to determine
# its chemical composition. The file spectra.txt reports the reflectance spectra of 10 independent measurements
# collected at different positions of the satellite along its orbit. Each spectrum has been sampled on the same grid of
# wavelengts (wl1,. . .,wl80). It is known that the spectrometer measurements are affected by small errors. Answer
# the following questions, considering a functional data analysis approach.
# a) Consider the first spectrum and perform a smoothing of this datum using a B-spline basis of degree 3. Choose
# the number of basis functions using a generalized cross-validation (GCV) criterion and provide a plot of the
# value of the GCV statistic versus the number of basis elements considered. Report the number of basis functions
# chosen and the first 3 coefficients of the basis expansion.
# b) Perform a smoothing of the other spectra using the basis chosen at point (a). Provide a plot of the smoothed
# data.
# c) Use the k-mean alignment algorithm to simultaneously cluster (k=3) and align the data allowing affine transformation for the abscissas. Use the correlation between the curves as similarity measure. Provide a plot of the
# aligned data colored according to the cluster assignment and a plot of the warping functions colored according
# to the cluster assignment. Comment on the results.


# a) Consider the first spectrum and perform a smoothing of this datum using a B-spline basis of degree 3. Choose
# the number of basis functions using a generalized cross-validation (GCV) criterion and provide a plot of the
# value of the GCV statistic versus the number of basis elements considered. Report the number of basis functions
# chosen and the first 3 coefficients of the basis expansion.
data <- read.table('spectra.txt', header=TRUE)
head(data)

Xobs0 <- as.numeric(data[1,])
abscissa <- 1:80
NT <- length(abscissa)
# plot(abscissa,Xobs0,xlab="t",ylab="observed data")
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])

# par(mfrow=c(1,2),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)
# plot(abscissa,Xobs0,xlab="t",ylab="observed data")
# plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
# dev.off()

m <- 4           # spline order 
degree <- m-1    # spline degree

nbasis <- 4:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(range(abscissa), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]

basis <- create.bspline.basis(range(abscissa), nbasis[which.min(gcv)], m)
basismat <- eval.basis(abscissa, basis)
basismat
lsfit(basismat, Xobs0, intercept=FALSE)$coef



# b) Perform a smoothing of the other spectra using the basis chosen at point (a). Provide a plot of the smoothed
# data.

Xsp0 <- basismat %*% lsfit(basismat, Xobs0, intercept=FALSE)$coef

# plot(abscissa,Xobs0,xlab="t",ylab="observed data")
# points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
# abline(v=basis$params) # numbasis  - m

par(mfrow=c(3,3))
for (i in 2:10)
{
  Xobs0 <- as.numeric(data[i,])
  abscissa <- 1:80
  NT <- length(abscissa)
  basismat <- eval.basis(abscissa, basis)
  Xsp0 <- basismat %*% lsfit(basismat, Xobs0, intercept=FALSE)$coef
  plot(abscissa,Xobs0,xlab="t",ylab="observed data",main=i)
  points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
}

set.seed(1)
y0 = Xobs0
x = abscissa
fdakma_example <- kma(
  x=abscissa, y0=data, n.clust = 3, 
  warping.method = 'affine', 
  similarity.method = 'd0.pearson',  # similarity computed as the cosine
  # between the first derivatives 
  # (correlation)
  center.method = 'k-means'
  #seeds = c(1,21) # you can give a little help to the algorithm...
)


kma.show.results(fdakma_example)

























































































