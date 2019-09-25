
## d15N koa plot
krig.AK<- scape.data %>% filter(Plot=="AK") # just AK

KOA.AK<-  krig.AK[c(krig.AK$Sample=="Koa"),]
coordinates(KOA.AK)<- ~x.meter + y.meter # coordinate system

# create sequences that represent the center of the columns of pixels
# change "by" to change the resolution of the raster
Columns=seq(from=1, to=35, by=1)

# And the rows of pixels:
Rows=seq(from=1, to=20, by=1)

# Create a grid of "Pixels" using x as columns and y as rows
Grid.KOA.AK <- expand.grid(x=Columns,y=Rows)

# Convert Thegrid to a SpatialPixel class
coordinates(Grid.KOA.AK) <- ~ x+y
gridded(Grid.KOA.AK) <- TRUE

#Perform ordinary kriging and store results inside object of type "autoKrige" "list"
kriging_result.KOA.AK = autoKrige(d15N ~ 1, KOA.AK , Grid.KOA.AK)
plot(kriging_result.KOA.AK)
