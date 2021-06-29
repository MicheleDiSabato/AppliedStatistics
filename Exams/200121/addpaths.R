rm(list=ls())
graphics.off()

setwd("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/OLD_EXAMS/Old exams (in English)/200121")
load("C:/Users/Michele/Desktop/POLIMI - MAGISTRALE/APPLIED STATISTCS/LAB_5/mcshapiro.test.RData")

data <- read.table('', header=TRUE)
head(data)

# pdf(file = "myplot2.pdf", onefile = T)
# plot(rnorm(10),rnorm(10))
# plot(rnorm(10),rnorm(10))
# plot(rnorm(10),rnorm(10))
# dev.off()

#        ~      

library(MASS)
library(car)
library(rgl)
library(mvtnorm)
library(glmnet)
library(mvnormtest)
library(rgl)
library(fda)
library(KernSmooth)
library(fdakma)
library(fields)
library(sp)           ## Data management
library(lattice)      ## Data management
library(geoR)         ## Geostatistics
library(gstat)        ## Geostatistics
library(e1071)
library(class)