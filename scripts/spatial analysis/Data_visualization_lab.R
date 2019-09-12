#####Lab 2.2 ##Data visualization
#################################
#########Visualization
library(ggplot2)
library(dplyr)
library(gstat)
library(STRbook)

set.seed(1)

data(NOAA_df_1990)
Tmax <- filter(NOAA_df_1990, # subset the data
               proc=="Tmax" & # only max temperature
                 month %in% 5:9 & # May to July
                 year == 1993) # year of 1993

Tmax %>% select(lon,lat,julian,z) %>% head() ##### %>% means do one operation after the other on the initial dataframe
Tmax$t <- Tmax$julian - 728049 # create a new time variable

Tmax_1 <- subset(Tmax,t %in% c(1,15,30)) # extract a subset of the data on the 1, 15 and 30th of the month

NOAA_plot <- ggplot(Tmax_1) +  # plot points
  geom_point(aes(x=lon,y=lat, # lon and lat
                 colour=z), # attribute colour
             size=4) + # make all points larger
  col_scale("Tmax (degF)") + # attach colour scale
  geom_path(data=map_data("world"), # add world map
            aes(x=long,y=lat,group=group)) +
  facet_grid(~t) + # facet by time
  coord_fixed(xlim=c(-105,-75),
              ylim=c(25,50)) + # zoom in
  theme_bw() # B&W theme

NOAA_plot #visualize the plot

UIDs <- unique(Tmax$id) # extract IDs
UIDs_sub <- sample(UIDs,10) # subset 10 IDs at random
Tmax_sub <- filter(Tmax,id %in% UIDs_sub) # subset data

TmaxTS <- LinePlotTheme(Tmax_sub) +
  geom_line(aes(t,z)) + # line plot of z against t
  facet_wrap(~id,ncol = 5) + # facet by station
  xlab("day number") + # x label
  ylab("Tmax (degF)") + # y label
  theme(panel.spacing = unit(1,"lines")) # increase facet spacing, use panel.spacing instead of panel.margin 

TmaxTS #visualize the plot

###########Hovmoller plots

lim_lat <- range(Tmax$lat) # latitude range
lim_t <- range(Tmax$t) # time range
lat_axis <- seq(lim_lat[1], # latitude axis, specifically take the first element of the list
                lim_lat[2], # take the second element in the list
                length=25) # increment the range into 25 increments

t_axis <- seq(lim_t[1], # time axis
              lim_t[2], 
              length=100) # break into 100 increments

lat_t_grid <- expand.grid(lat=lat_axis, #Create lat x time grid
                          t = t_axis)

Tmax_grid <- Tmax #remae the the Tmax dataset to create variable that be placed on the grid 
dists <- abs(outer(Tmax$lat,lat_axis,"-")) #get the absolute value of the latitudes in Tmax and in lat_axis
Tmax_grid$lat <- lat_axis[apply(dists,1,which.min)] #get the minimum distance at each 1 degree of latitude

Tmax_lat_Hov <- group_by(Tmax_grid,lat,t) %>% summarise(z = mean(z)) #Group the temperatures by latitude and time and summerize

Hovmoller_lat <- ggplot(Tmax_lat_Hov) + # take the data
  geom_tile(aes(x=lat, y=t, fill = z)) + # plot lat against time
  fill_scale("Tmax (degF)") + # add color scale
  ylab("day") + # add y label
  scale_y_reverse() + # reverse time axis
  theme_bw() # change theme

Hovmoller_lat ###visualize plot

