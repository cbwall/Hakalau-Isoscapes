# Kriging tests for isotope data in Hakalau Forest
# CB Wall
# August 27 2019

# help from here: https://rpubs.com/nabilabd/118172
################################################

#load packages
library(sp)
library(gstat)

suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})

################################################

# load data
krig<-read.csv("~/Google Drive/Hynson Lab/Hakalau/R/kriging.test.csv")

# simulate data
krig <- krig %>% 
  mutate(d15N.ex = as.numeric(rnorm(n=166, mean=2, sd=3)))

# just AK site
krig.AK<- krig %>% filter(site=="AK")

# example map with 
krig.AK %>% as.data.frame %>% 
  ggplot(aes(x.meter, y.meter)) + geom_point(aes(size=d15N.ex), color="forestgreen", alpha=3/4) + 
  ggtitle("d15N simulated (permil)") + coord_equal() + theme_bw()


# convert to spatial data, now a SPDF class object with 5 data slots
coordinates(krig.AK)<- ~x.meter + y.meter
str(krig.AK)
# data: contains all the variables associated with different spatial locations
# coords slot:  a matrix of all spatial locations with corresponding values
# bbox:  the bounding box or the 4 corners denoting spatial extent
# proj4string: projection inform. ie., what are the coordinates in?

# can coerce back to dataframe for plotting etc
# krig.AK<-krig.AK %>% as.data.frame %>% glimpse


##### VARIOGRAM, must be the SPDF object
# variogram function can take two arguments: 
# first: how one or more variables interact spatially
# second: SPDF object where those variables reside.


# no trend variogram
# example: variogram(log(zinc)~1, meuse)
vario.notrend <- variogram(d15N.ex~1, krig.AK)

# residual variogram w.r.t. a linear trend:
# example variogram(log(zinc)~x+y, meuse)
vario.resid <- variogram(d15N.ex~x.meter+y.meter, krig.AK) # calculates sample variogram values

# directional variogram:
# example: variogram(log(zinc)~x+y, meuse, alpha=c(0,45,90,135))
# example: variogram(log(zinc)~1, meuse, width=90, cutoff=1300)
vario.dir1 <- variogram(d15N.ex~x.meter+y.meter, krig.AK, alpha=c(0,45,90,135))
vario.dir2 <- variogram(d15N.ex~1, krig.AK, width=90, cutoff=1300)


######### fitting the variogram 
# first: variogram testing
# second: model with parameters to be fit to sample variogram
vario.fit <- fit.variogram(vario.resid, model=vgm(1, "Sph", 900, 1)) # fit model

plot(vario.resid, vario.fit) # plot the sample values, along with the fit model


##########################
# Kriging 

# need to make a grid to match where kriging/estimation is done

grid<-makegrid(krig.AK, n=1000)
colnames(grid)<-c('x.meter', 'y.meter')
grid<-as.data.frame(grid)

#where we have measurements
plot1 <- krig.AK %>% as.data.frame %>%
  ggplot(aes(x.meter, y.meter)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# where we WANT measurements
plot2 <- grid %>% as.data.frame %>%
  ggplot(aes(x.meter, y.meter)) + geom_point(size=0.5) + coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

# Once we have the prepared all of the above, we are now ready to krige. This can be done with the gstat::krige function, which usually takes four arguments:
# The model formula.
#  An SPDF of the spatial domain that has measurements.
#  An SPDF of the spatial domain to krige over.
#  A variogram model fitted to the data.
# Note that the second and third arguments have to be SPDF’s and cannot just be dataframes.


# make coordinates for grid (as we did for test data)
coordinates(grid)<- ~ x.meter + y.meter

# fit the model for the krig object (grid) and use model fit from corresponding model data above
test.krig<-krige(d15N.ex~1, krig.AK, grid, model=vario.fit)

test.krig %>% as.data.frame %>%
  ggplot(aes(x=x.meter, y=y.meter)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()



