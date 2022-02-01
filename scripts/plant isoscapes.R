### Soil krigs    

#### d15N Soil isoscape

###### AK site d15N

## d15N plant plot
krig.AK<- scape.data %>% filter(Plot=="AK") # just AK
AK.plants<-  krig.AK[!(krig.AK$Sample=="Soil"),] #remove soil
AK.plants$Sample<-droplevels(AK.plants$Sample)

#bubble plot
# d15N
AK.map.d15N<-AK.plants %>% as.data.frame %>% 
  ggplot(aes(x.meter, y.meter, group=Sample)) + geom_point(aes(size=d15N, color=Sample), 
                                                           alpha=3/4) + 
  ggtitle("AK plot--d15N (permil)") + coord_equal() + theme_bw()


#d13C
AK.map.d13C<-AK.plants %>% as.data.frame %>% 
  ggplot(aes(x.meter, y.meter, group=Sample)) + geom_point(aes(size=d13C, color=Sample), 
                                                           alpha=3/4) + 
  ggtitle("AK plot--d13C (permil)") + coord_equal() + theme_bw()


## Krig AK sites
coordinates(AK.plants)<- ~x.meter + y.meter

# Create a grid of "Pixels" using x as columns and y as rows
# And the rows and columns of pixels:
Columns=seq(from=-1, to=36, by=0.05)
Rows=seq(from=-1, to=21, by=0.05)
Grid.AK <- expand.grid(x=Columns,y=Rows)
coordinates(Grid.AK) <- ~ x+y
gridded(Grid.AK) <- TRUE # Plot the grid and points

## expand binding box 
#AK.soil@bbox # current data binding box
x<-c(-1, 36)
y<-c(-1, 21)
xy<-cbind(x,y)
S<-SpatialPoints(xy)
bbox(S)
AK.plants@bbox<-bbox(S) # expanded binding box for data
Grid.AK@bbox<-bbox(S) # expand for plot corner

# autokrige
d15N.AK.plant <- autoKrige(d15N ~ 1, AK.plants, Grid.AK) # ordinary kriging
plot(d15N.AK.plant, sp.layout = list(pts = list("sp.points", AK.plants)))
#d15N.AK.soil$krige_output

# make to dataframes for lm, combine
AK.plant.pred.N <- d15N.AK.plant[1] %>% as.data.frame() # model predictions
AK.plant.df.N <- AK.plants %>% as.data.frame() # dataframe

# make dataframe with data and predictions (krig) to assess fit
pred.AK.plants.df.N<- left_join(AK.plants.df.N, AK.plants.pred.N, by = c("x.meter"="krige_output.x"))#longest axis is x
mod <- lm(krige_output.var1.pred ~ d15N, data=pred.AK.plants.df.N)
summary(mod) #R squared = 0.19

mean(pred.AK.plants.df.N$krige_output.var1.pred, na.rm=T) # mean of predictions 1.6 d15N
mean(pred.AK.plants.df.N$krige_output.var1.stdev, na.rm=T) # 0.72

# inspect model
# plot(pred.AK.soil.df.N$krige_output.var1.pred ~ pred.AK.soil.df.N$d15N, xlab = "Observed", ylab = "Predicted", cex=0.2); abline(0,1, lty=2); abline(mod, col = "blue")

# Adding the sp.layout parameter shows the locations of the measurements
my.palette <- brewer.pal(n = 9, name = "RdBu")

# map in automap
#automapPlot(d15N.AK.soil$krige_output, "var1.pred", sp.layout = list("sp.points", AK.soil, pch=16, col="gray20", cex=0.5), col.regions=my.palette)

# map in ssplot
# variance
# spplot(d15N.AK.soil$krige_output,"var1.stdev")

#predicted krige
plotd15N.krig.AK.plants<-spplot(d15N.AK.plant$krige_output["var1.pred"], col.regions=col.scheme.N, 
                              sp.layout = list("sp.points", AK.plants, pch=1, 
                                               col=c("black"), cex=0.5, alpha=0.5),
                              main=list(label=expression(paste(delta^{15}, N, " - AK (Restored Forest)")), cex=0.8), colorkey=FALSE)




###########################
# RK Site Krig
###########################

## d15N plant plot
krig.RK<- scape.data %>% filter(Plot=="RK") # just AK
RK.plants<-  krig.RK[!(krig.RK$Sample=="Soil"),] #just soil
RK.plants$Sample<-droplevels(RK.plants$Sample)


#bubble plot
# d15N
RK.map.d15N<-RK.plants %>% as.data.frame %>% 
  ggplot(aes(x.meter, y.meter, group=Sample)) + geom_point(aes(size=d15N, color=Sample), 
                                                           alpha=3/4) + 
  ggtitle("RK plot--d15N (permil)") + coord_equal() + theme_bw()


#d13C
RK.map.d13C<-RK.plants %>% as.data.frame %>% 
  ggplot(aes(x.meter, y.meter, group=Sample)) + geom_point(aes(size=d13C, color=Sample), 
                                                           alpha=3/4) + 
  ggtitle("RK plot--d13C (permil)") + coord_equal() + theme_bw()




## Krig RK sites
coordinates(RK.plants)<- ~x.meter + y.meter

# Create a grid of "Pixels" using x as columns and y as rows
# And the rows and columns of pixels:
Columns=seq(from=-1, to=21, by=0.05)
Rows=seq(from=-1, to=36, by=0.05)
Grid.RK <- expand.grid(x=Columns,y=Rows)
coordinates(Grid.RK) <- ~ x+y
gridded(Grid.RK) <- TRUE # Plot the grid and points

## expand binding box 
#RK.soil@bbox # current data binding box
x<-c(-1, 21)
y<-c(-1, 36)
xy<-cbind(x,y)
S<-SpatialPoints(xy)
bbox(S)
RK.plants@bbox<-bbox(S) # expanded binding box for data
Grid.RK@bbox<-bbox(S) # expand for plot corner

# autokrige
d15N.RK.plants <- autoKrige(d15N ~ 1, RK.plants, Grid.RK) # ordinary kriging
plot(d15N.RK.plants, sp.layout = list(pts = list("sp.points", RK.plants)))
#d15N.RK.soil$krige_output

# make to dataframes for lm, combine
RK.plants.pred.N <- d15N.RK.plants[1] %>% as.data.frame() # model predictions
RK.plants.df.N <- RK.plants %>% as.data.frame() # dataframe

# make dataframe with data and predictions (krig) to assess fit
pred.RK.plants.df.N<- left_join(RK.plants.df.N, RK.plants.pred.N, by = c("x.meter"="krige_output.x"))#longest axis is x
mod <- lm(krige_output.var1.pred ~ d15N, data=pred.RK.plants.df.N)
summary(mod) #R squared = 0.01

mean(pred.RK.plants.df.N$krige_output.var1.pred, na.rm=T) # mean of predictions 6.4 d15N
mean(pred.RK.plants.df.N$krige_output.var1.stdev, na.rm=T) # 0.8

# inspect model
# plot(pred.RK.soil.df.N$krige_output.var1.pred ~ pred.RK.soil.df.N$d15N, xlab = "Observed", ylab = "Predicted", cex=0.2); abline(0,1, lty=2); abline(mod, col = "blue")

# Adding the sp.layout parameter shows the locations of the measurements
my.palette <- brewer.pal(n = 9, name = "RdBu")

# map in automap
#automapPlot(d15N.RK.soil$krige_output, "var1.pred", sp.layout = list("sp.points", RK.soil, pch=16, col="gray20", cex=0.5), col.regions=my.palette)

# map in ssplot
# variance
# spplot(d15N.RK.soil$krige_output,"var1.stdev")

#predicted krige
plotd15N.krig.RK.plants<-spplot(d15N.RK.plants$krige_output["var1.pred"], col.regions=col.scheme.N, 
                              sp.layout = list("sp.points", RK.plants, pch=1, 
                                               col=c("black"), cex=0.5, alpha=0.5),
                              main=list(label=expression(paste(delta^{15}, N, " - RK (Remnant Forest)")), cex=0.8), colorkey=TRUE)


#####################
grid.draw.ggmatrix <- function(x, recording = TRUE) {
  print(x)
}
### combined Ak-Rk krig d15N plot 
par(mfrow=c(1,2))
gridExtra::grid.arrange(plotd15N.krig.AK.plants, plotd15N.krig.RK.plants, ncol=2, nrow=1, widths = c(1.2,1))
dev.copy(png, "figures/RKAK.d15N.plants.png", width = 8, height = 4, units = 'in', res = 300)
dev.off()







#### d13C Plant isoscape

###### AK site d13C

# autokrige
d13C.AK.plants <- autoKrige(d13C ~ 1, AK.plants, Grid.AK) # ordinary kriging
plot(d13C.AK.plants, sp.layout = list(pts = list("sp.points", AK.plants)))
#d13C.AK.plants$krige_output

# make to dataframes for lm, combine
AK.plants.pred.C <- d13C.AK.plants[1] %>% as.data.frame() # model predictions
AK.plants.df.C <- AK.plants %>% as.data.frame() # dataframe

# make dataframe with data and predictions (krig) to assess fit
pred.AK.plants.df.C<- left_join(AK.plants.df.C, AK.plants.pred.C, by = c("x.meter"="krige_output.x"))#longest axis is x
mod <- lm(krige_output.var1.pred ~ d13C, data=pred.AK.plants.df.C)
summary(mod) #R squared = 0.003

mean(pred.AK.plants.df.C$krige_output.var1.pred, na.rm=T) # mean of predictions -29.2 d13C
mean(pred.AK.plants.df.C$krige_output.var1.stdev, na.rm=T) # 0.9

# inspect model
# plot(pred.AK.plants.df.C$krige_output.var1.pred ~ pred.AK.plants.df.C$d13C, xlab = "Observed", ylab = "Predicted", cex=0.2); abline(0,1, lty=2); abline(mod, col = "blue")

# Adding the sp.layout parameter shows the locations of the measurements
my.palette <- brewer.pal(n = 9, name = "RdBu")

# map in automap
#automapPlot(d13C.AK.plants$krige_output, "var1.pred", sp.layout = list("sp.points", AK.plants, pch=16, col="gray20", cex=0.5), col.regions=my.palette)

# map in ssplot
# variance
# spplot(d13C.AK.plants$krige_output,"var1.stdev")

#predicted krige
plotd13C.krig.AK.plants<-spplot(d13C.AK.plants$krige_output["var1.pred"], col.regions=col.scheme.C, 
                              sp.layout = list("sp.points", AK.plants, pch=1, 
                                               col=c("black"), cex=0.5, alpha=0.5),
                              main=list(label=expression(paste(delta^{13}, C, " - AK (Restored Forest)")), cex=0.8), colorkey=FALSE)



###########################
# RK Site Krig
###########################

# autokrige
d13C.RK.plants <- autoKrige(d13C ~ 1, RK.plants, Grid.RK) # ordinary kriging
plot(d13C.RK.plants, sp.layout = list(pts = list("sp.points", RK.plants)))
#d13C.RK.plants$krige_output

# make to dataframes for lm, combine
RK.plants.pred.C <- d13C.RK.plants[1] %>% as.data.frame() # model predictions
RK.plants.df.C <- RK.plants %>% as.data.frame() # dataframe

# make dataframe with data and predictions (krig) to assess fit
pred.RK.plants.df.C<- left_join(RK.plants.df.C, RK.plants.pred.C, by = c("x.meter"="krige_output.x"))#longest axis is x
mod <- lm(krige_output.var1.pred ~ d13C, data=pred.RK.plants.df.C)
summary(mod) #R squared = 0.16

mean(pred.RK.plants.df.C$krige_output.var1.pred, na.rm=T) # mean of predictions -30 d13C
mean(pred.RK.plants.df.C$krige_output.var1.stdev, na.rm=T) # 0.8

# inspect model
# plot(pred.RK.plants.df.C$krige_output.var1.pred ~ pred.RK.plants.df.C$d13C, xlab = "Observed", ylab = "Predicted", cex=0.2); abline(0,1, lty=2); abline(mod, col = "blue")

# Adding the sp.layout parameter shows the locations of the measurements
my.palette <- brewer.pal(n = 9, name = "RdBu")

# map in automap
#automapPlot(d13C.RK.plants$krige_output, "var1.pred", sp.layout = list("sp.points", RK.plants, pch=16, col="gray20", cex=0.5), col.regions=my.palette)

# map in ssplot
# variance
# spplot(d13C.RK.plants$krige_output,"var1.stdev")

#predicted krige
plotd13C.krig.RK.plants<-spplot(d13C.RK.plants$krige_output["var1.pred"], col.regions=col.scheme.C, 
                              sp.layout = list("sp.points", RK.plants, pch=1, 
                                               col=c("black"), cex=0.5, alpha=0.5),
                              main=list(label=expression(paste(delta^{13}, C, " - RK (Remnant Forest)")), cex=0.8), colorkey=TRUE)


#####################
grid.draw.ggmatrix <- function(x, recording = TRUE) {
  print(x)
}
### combined Ak-Rk krig d13C plot 
par(mfrow=c(1,2))
gridExtra::grid.arrange(plotd13C.krig.AK.plants, plotd13C.krig.RK.plants, ncol=2, nrow=1, widths = c(1.2,1))
dev.copy(png, "figures/RKAK.d13C.plants.png", width = 8, height = 4, units = 'in', res = 300)
dev.off()

```

Density plots for plants isoscapes  
```{r, plants density plots, fig.show='hold', out.width='50%'}
# make density plots using predictions
# combine the predictions for each plot


############## Nitrogen plot
AK.df.plants.N<-as.data.frame(pred.AK.plants.df.N$krige_output.var1.pred); AK.df.plants.N$Plot<-"AK"
RK.df.plants.N<-as.data.frame(pred.RK.plants.df.N$krige_output.var1.pred); RK.df.plants.N$Plot<-"RK" 
colnames(AK.df.plants.N)<-c("d15N.pred", "Plot")
colnames(RK.df.plants.N)<-c("d15N.pred", "Plot")

# new dataframe
pred.plants.N<-rbind(AK.df.plants.N, RK.df.plants.N)
pred.plants.N$Plot<-as.factor(pred.plants.N$Plot)
pred.plants.N %>% 
  group_by(Plot) %>%
  summarise(no_rows = length(Plot))

#man whitney for significance (not normal data)
mwu(pred.plants.N, d15N.pred, Plot) # signif difference in predictions (p<0.001)

# Density plot

#hist(pred.plants.N$d15N.pred[(pred.plants.N$Plot=="AK")], ylim=c(0,10000), xlab="d15N-predict")
#hist(pred.plants.N$d15N.pred[(pred.plants.N$Plot=="RK")], ylim=c(0,10000), xlab="d15N-predict")

# plot.means
predict.mean.N <- ddply(pred.plants.N, "Plot", summarise, grp.mean=mean(d15N.pred, na.rm=TRUE))

density.predict.Nplants<-ggplot(pred.plants.N, aes(x=d15N.pred)) +
  geom_density(aes(color=Plot, fill=Plot), alpha=0.5)+ ylim(0,1.2) +
  xlab(expression(paste(delta^{15}, N["predicted"], sp="")))+
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  geom_vline(data=predict.mean.N, aes(xintercept=grp.mean, color=Plot),
             linetype="dashed", lwd=0.2) + BW.back2


############## Carbon
AK.df.plants.C<-as.data.frame(pred.AK.plants.df.C$krige_output.var1.pred); AK.df.plants.C$Plot<-"AK"
RK.df.plants.C<-as.data.frame(pred.RK.plants.df.C$krige_output.var1.pred); RK.df.plants.C$Plot<-"RK" 
colnames(AK.df.plants.C)<-c("d13C.pred", "Plot")
colnames(RK.df.plants.C)<-c("d13C.pred", "Plot")

# new dataframe
pred.plants.C<-rbind(AK.df.plants.C, RK.df.plants.C)
pred.plants.C$Plot<-as.factor(pred.plants.C$Plot)
pred.plants.C %>% 
  group_by(Plot) %>%
  summarise(no_rows = length(Plot))

#man whitney for significance (not normal data)
mwu(pred.plants.C, d13C.pred, Plot) # signif difference in predictions (p<0.001)

# Density plot

#hist(pred.plants.C$d13C.pred[(pred.plants.C$Plot=="AK")], ylim=c(0,10000), xlab="d13C-predict")
#hist(pred.plants.C$d13C.pred[(pred.plants.C$Plot=="RK")], ylim=c(0,10000), xlab="d13C-predict")

# plot.means
predict.mean.C <- ddply(pred.plants.C, "Plot", summarise, grp.mean=mean(d13C.pred, na.rm=TRUE))

density.predict.Cplants<-ggplot(pred.plants.C, aes(x=d13C.pred)) +
  geom_density(aes(color=Plot, fill=Plot), alpha=0.5)+ ylim(0,0.8) +
  xlab(expression(paste(delta^{13}, C["predicted"], sp="")))+
  scale_color_manual(values=c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
  geom_vline(data=predict.mean.C, aes(xintercept=grp.mean, color=Plot),
             linetype="dashed", lwd=0.2) + BW.back2

# get the legend, # create some space to the left of the legend
density.legend <- get_legend(
  density.predict.Nplants + theme(legend.box.margin = margin(0, 0, 0, 12)) + 
    theme(legend.key.size = unit(0.3, "cm")))

gridExtra::grid.arrange(density.predict.Nplants + theme(legend.position = "none"),
                        density.predict.Cplants + theme(legend.position = "none"),
                        density.legend,
                        ncol=3, nrow=1, widths = c(1,1,0.3))
dev.copy(pdf, "figures/Fig Sx.mod.pred.pdf", width = 5, height = 2.5)
dev.off()

```

```{r, AK plants krig, eval= FALSE}
#### d15N Plants isoscape
Subset data and only examine PLANTS     
- *Restored forest (AK)* and *Remnant Forest (RK)*
  ###### AK site d15N
  
  ## d15N plants plot
  krig.AK<- scape.data %>% filter(Plot=="AK") # just AK
AK.plants<-  krig.AK[!(krig.AK$Sample=="plants"),] #just plants
AK.plants$Sample<-droplevels(AK.plants$Sample)

## Krig AK sites
coordinates(AK.plants)<- ~x.meter + y.meter

# Create a grid of "Pixels" using x as columns and y as rows
# And the rows and columns of pixels:
Columns=seq(from=-1, to=36, by=0.05)
Rows=seq(from=-1, to=21, by=0.05)
Grid.AK <- expand.grid(x=Columns,y=Rows)
coordinates(Grid.AK) <- ~ x+y
gridded(Grid.AK) <- TRUE # Plot the grid and points

## expand binding box 
#AK.plants@bbox # current data binding box
x<-c(-1, 36)
y<-c(-1, 21)
xy<-cbind(x,y)
S<-SpatialPoints(xy)
bbox(S)
AK.plants@bbox<-bbox(S) # expanded binding box for data
Grid.AK@bbox<-bbox(S) # expand for plot corner

# autokrige
d15N.AK.plants <- autoKrige(d15N ~ 1, AK.plants, Grid.AK, fix.value=c(0.2, 3, 0.6)) # ordinary kriging
plot(d15N.AK.plants, sp.layout = list(pts = list("sp.points", AK.plants)))
#d15N.AK.plants$krige_output

# make to dataframes for lm, combine
AK.plants.pred.N <- d15N.AK.plants[1] %>% as.data.frame() # model predictions
AK.plants.df.N <- AK.plants %>% as.data.frame() # dataframe

# make dataframe with data and predictions (krig) to assess fit
pred.AK.plants.df.N<- left_join(AK.plants.df.N, AK.plants.pred.N, by = c("x.meter"="krige_output.x"))#longest axis is x
mod <- lm(krige_output.var1.pred ~ d15N, data=pred.AK.plants.df.N)
summary(mod) #R squared = 0.19

mean(pred.AK.plants.df.N$krige_output.var1.pred, na.rm=T) # mean of predictions 1.6 d15N
mean(pred.AK.plants.df.N$krige_output.var1.stdev, na.rm=T) # 0.7

# inspect model
# plot(pred.AK.plants.df.N$krige_output.var1.pred ~ pred.AK.plants.df.N$d15N, xlab = "Observed", ylab = "Predicted", cex=0.2); abline(0,1, lty=2); abline(mod, col = "blue")

# Adding the sp.layout parameter shows the locations of the measurements
my.palette <- brewer.pal(n = 9, name = "RdBu")

# map in automap
#automapPlot(d15N.AK.plants$krige_output, "var1.pred", sp.layout = list("sp.points", AK.plants, pch=16, col="gray20", cex=0.5), col.regions=my.palette)

# map in ssplot
# variance
# spplot(d15N.AK.plants$krige_output,"var1.stdev")

#predicted krige
plotd15N.krig.AK.plants<-spplot(d15N.AK.plants$krige_output["var1.pred"], col.regions=col.scheme.N, 
                                sp.layout = list("sp.points", AK.plants, pch=c(16,17)[AK.plants$Sample],
                                                 col=c("black"), cex=0.5, alpha=0.5),
                                main=list(label=expression(paste(delta^{15}, N, " - AK (Restored Forest)")), cex=0.8), colorkey=FALSE)

plotd15N.krig.AK.plants


###########################
# RK Site Krig
###########################

## d15N plants plot
krig.RK<- scape.data %>% filter(Plot=="RK") # just AK
RK.plants<-  krig.RK[!(krig.RK$Sample=="plants"),] #just plants
RK.plants$Sample<-droplevels(RK.plants$Sample)

## Krig RK sites
coordinates(RK.plants)<- ~x.meter + y.meter

# Create a grid of "Pixels" using x as columns and y as rows
# And the rows and columns of pixels:
Columns=seq(from=-1, to=21, by=0.05)
Rows=seq(from=-1, to=36, by=0.05)
Grid.RK <- expand.grid(x=Columns,y=Rows)
coordinates(Grid.RK) <- ~ x+y
gridded(Grid.RK) <- TRUE # Plot the grid and points

## expand binding box 
#RK.plants@bbox # current data binding box
x<-c(-1, 21)
y<-c(-1, 36)
xy<-cbind(x,y)
S<-SpatialPoints(xy)
bbox(S)
RK.plants@bbox<-bbox(S) # expanded binding box for data
Grid.RK@bbox<-bbox(S) # expand for plot corner

# autokrige
d15N.RK.plants <- autoKrige(d15N ~ 1, RK.plants, Grid.RK) # ordinary kriging
plot(d15N.RK.plants, sp.layout = list(pts = list("sp.points", RK.plants)))
#d15N.RK.plants$krige_output

# make to dataframes for lm, combine
RK.plants.pred.N <- d15N.RK.plants[1] %>% as.data.frame() # model predictions
RK.plants.df.N <- RK.plants %>% as.data.frame() # dataframe

# make dataframe with data and predictions (krig) to assess fit
pred.RK.plants.df.N<- left_join(RK.plants.df.N, RK.plants.pred.N, by = c("x.meter"="krige_output.x"))#longest axis is x
mod <- lm(krige_output.var1.pred ~ d15N, data=pred.RK.plants.df.N)
summary(mod) #R squared = 0.15

mean(pred.RK.plants.df.N$krige_output.var1.pred, na.rm=T) # mean of predictions 2.8 d15N
mean(pred.RK.plants.df.N$krige_output.var1.stdev, na.rm=T) # 0.5

# inspect model
# plot(pred.RK.plants.df.N$krige_output.var1.pred ~ pred.RK.plants.df.N$d15N, xlab = "Observed", ylab = "Predicted", cex=0.2); abline(0,1, lty=2); abline(mod, col = "blue")

# Adding the sp.layout parameter shows the locations of the measurements
my.palette <- brewer.pal(n = 9, name = "RdBu")

# map in automap
#automapPlot(d15N.RK.plants$krige_output, "var1.pred", sp.layout = list("sp.points", RK.plants, pch=16, col="gray20", cex=0.5), col.regions=my.palette)

# map in ssplot
# variance
# spplot(d15N.RK.plants$krige_output,"var1.stdev")

#predicted krige
plotd15N.krig.RK.plants<-spplot(d15N.RK.plants$krige_output["var1.pred"], col.regions=col.scheme.N, 
                                sp.layout = list("sp.points", RK.plants, pch=c(16,17)[RK.plants$Sample],
                                                 col=c("black"), cex=0.5, alpha=0.5),
                                main=list(label=expression(paste(delta^{15}, N, " - RK (Remnant Forest)")), cex=0.8), colorkey=TRUE)
plotd15N.krig.RK.plants


#####################
grid.draw.ggmatrix <- function(x, recording = TRUE) {
  print(x)
}
### combined Ak-Rk krig d15N plot 
par(mfrow=c(1,2))
gridExtra::grid.arrange(plotd15N.krig.AK.plants, plotd15N.krig.RK.plants, ncol=2, nrow=1, widths = c(1.2,1))
dev.copy(png, "figures/RKAK.d15N.plants.png", width = 8, height = 4, units = 'in', res = 300)
dev.off()

```



```{r, eval=FALSE}
#####################
# Carbon kriging, leave alone for now.
#####################

## Krig AK site d13C
coordinates(krig.AK)<- ~x.meter + y.meter
col.scheme.C <- colorRampPalette(c('coral', 'cadetblue2', 'gray5'))(5)

Grid.AK <- spsample(krig.AK, type='regular', n=1e5)
gridded(Grid.AK) <- TRUE

## d13C plot
d13C.krig.AK <- krige(d13C ~ 1, krig.AK, Grid.AK)
plotd13C.krig.AK<-spplot(d13C.krig.AK["var1.pred"], col.regions=colorRampPalette(col.scheme.C), 
                         main=expression(paste(delta^{13}, C, "--AK (Afforested Koa Plot)")))

## Krig RK site d13C
coordinates(krig.RK)<- ~x.meter + y.meter
GridRK <- spsample(krig.RK, type='regular', n=1e5)
gridded(GridRK) <- TRUE

## d13C plot
d13C.krig.RK <- krige(d13C ~ 1, krig.RK, GridRK)
plotd13C.krig.RK<-spplot(d13C.krig.RK["var1.pred"], col.regions=colorRampPalette(col.scheme.C), 
                         main=expression(paste(delta^{13}, C, "--RK (Remnant Koa Plot)")))


```


