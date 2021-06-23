# fourth


# Problem n.4
# The manager of the hotel Boule de Neige in Courmayeur is designing the pricing strategy for the carnival festivities
# 2021. The file hotels.txt collects the prices per night y [e/night] in hotels in Courmayeur and neighboring
# villages, as observed for 55 hotels during 2019. The dataset also reports the UTM coordinates si of the hotels,
# whether the price refers to a day during the winter season or not (winter = yes or winter = no), and the
# distance of the considered hotel from the funicular connecting to the ski slopes, d(si) = ksi ??? sf k, with sf =
#   (342362.58, 5072518.24). Consider for the price the following model
# y(si) = a0,g + a1,g · d(si) + ??(si),
# with ??(si) a stationary residual with spherical variogram with nugget, and g = 1, 2 the grouping induced by the
# variable winter (g = 1 for winter = yes, g = 2 otherwise).



# a) Assuming a1,g = 0 for g = 1, 2 and a0,1 = a0,2 = a0, estimate the parameter a0 of the model via generalized least squares. Report the point estimate of a0 and the model estimated for ??(si); 
# discuss the model assumptions.

graphics.off()

hotels <- read.table('hotels.txt', header=TRUE)
head(hotels)
dummy = ifelse(hotels$winter=="yes",1,0)
temp = data.frame(x=hotels$x, y=hotels$y, distance=hotels$distance, price=hotels$price,dummy=dummy)
hotels=temp
coordinates(hotels) <- c('x','y')
head(hotels)
 
v=variogram(price ~ 1, data=hotels)
plot(v,pch=19)

v.fit <- fit.variogram(v, vgm(4000, "Sph", 400))
plot(v, v.fit, pch = 19)
v.fit

g.tr <- gstat(formula = price ~ 1, data = hotels, model = v.fit)
predict(g.tr, hotels[1,], BLUE = TRUE)
predict(g.tr, hotels[2,], BLUE = TRUE)
predict(g.tr, hotels[3,], BLUE = TRUE)

# STIMA DI a0 E' 263.5373


# b) Assuming a1,g != 0, estimate the parameters a0,g, a1,g of the model via generalized least squares. Report the
# point estimate of a0,g, a1,g and the model estimated for ??(si); discuss the model assumptions.

vb=variogram(price ~ distance, data=hotels)
plot(vb,pch=19)

v.fitb <- fit.variogram(vb, vgm(4000, "Sph", 500))
plot(vb, v.fitb, pch = 19)
v.fitb

g.tr <- gstat(formula = price ~ distance, data = hotels, model = v.fitb)
predict(g.tr, hotels[1,], BLUE = TRUE)
predict(g.tr, hotels[2,], BLUE = TRUE)
predict(g.tr, hotels[3,], BLUE = TRUE)
predict(g.tr, hotels[5,], BLUE = TRUE)
hotels[1,]
hotels[2,]
hotels[3,]
hotels[5,]


# c) Choose the best model between those estimated at points (a) and (b). Comment on your choice.


plot(hotels$distance, hotels$price, pch=19)
i1 = which(hotels$dummy==1)
i2 = which(hotels$dummy==0)
plot(hotels$distance, hotels$price, pch=19)
points(hotels[i1,]$distance, hotels[i1,]$price, pch=19, col = 'red')
points(hotels[i2,]$distance, hotels[i2,]$price, pch=19, col = 'green')

# il modello più approrpriato sembra essere il secondo, in quanto da questo plot si vede 
# che il prezzo dipende inversamente dalla distanza e da dummy


# d) Suggest to the hotel manager a pricing strategy for a stay of 4 nights at Boule de Neige in the period Feb.
# 17th to Feb. 21st (winter = yes, s0 = (342399.74, 5072272.75)). Motivate your response and detail your
# assumptions

















































