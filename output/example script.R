
# kriggin example

scape.data<- read.csv("~/Desktop/scape.data.csv")

## d15N koa plot
krig.AK<- scape.data %>% filter(Plot=="AK") # just AK

KOA.AK<-  krig.AK %>%
  mutate(d15N=replace(d15N, Sample=="Soil" | Sample=="RUBARG" | Sample=="RUBHAW", NA)) # leave only Koa

coordinates(KOA.AK)<- ~x.meter + y.meter # coordinate system
Grid.KOA.AK <- spsample(KOA.AK, type='regular', n=1e5)
gridded(Grid.KOA.AK) <- TRUE

d15N.KOA.AK <- krige(d15N ~ 1, KOA.AK, Grid.KOA.AK) # ordinary kriging
plot(variogram(d15N ~ 1, KOA.AK)) # variogram
