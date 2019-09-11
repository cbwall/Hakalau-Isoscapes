# Hakalau GPS, assessment of isotope data in Hakalau Forest
# CB Wall
# August 27 2019

################################################

################################################
library("RgoogleMaps")
library("SDMTools")
library("rgdal")
library("ggmap")
library("gridExtra")

# load data
Hak.gps<-read.csv("~/Google Drive/Hynson Lab/Hakalau/R/hakalau.gps.csv")


ggplot(Hak.gps, aes(x = longitude, y = latitude)) +
  coord_quickmap() +
  geom_point()

Hak=c(19.82158002,	-155.317)
map <- GetMap(center = Hak, zoom = 17, maptype = "satellite", SCALE = 2, 
              API_console_key="xxx") # enter use API here

# plot and export
#png(file= "Google Drive/Hynson Lab/Hakalau/R/sitemap.test.png", height=3.5, width=3.5, units="in", res=300)
par(oma=c(3,3,0,0))

PlotOnStaticMap(map, lat=Hak.gps$latitude, lon=Hak.gps$longitude, 
                col="white", pch=16, cex=0.5)
#dev.off()

######## using ggmap
register_google(key="AIzaSyBPQLRsZ6EgkW5hP6spsnAFNqktUTse8YM")

Hak.rev=c(-155.317, 19.82158002)
map2<-get_map(Hak.rev, 
                      zoom=17, 
                      scale = 2, 
                      maptype= "satellite",
                      source="google", extent= "device", legend="topright")

png(file= "sitemap.test.png", height=5, width=5, units="in", res=300)
par(oma=c(3,3,0,0))
ggmap(map2)+
  geom_point(aes(x=longitude, y=latitude), data=Hak.gps, alpha=0.5, color="dodgerblue", size=1)
dev.off()


# AK or RK only
AK<- Hak.gps[(Hak.gps$Site=="AK"),]
RK<- Hak.gps[(Hak.gps$Site=="RK"),]

####### AK map
AK.gps<-c(-155.3189, lat=19.8219)
mapAK<-get_map(AK.gps,
              zoom=20, 
              scale = 2, 
              maptype= "satellite",
              source="google", extent= "device", legend="topright")
AK.map<-ggmap(mapAK, group=type)+
  geom_point(aes(x=longitude, y=latitude, shape=type, color=type), data=AK, alpha=0.5, size=3)+
  theme(legend.position="top",
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white"),
    legend.key = element_rect(fill = "white")) +
  guides(colour= guide_legend(override.aes = list(color = "white"))) +
  scale_color_manual(values=c('red','mediumseagreen'))+
  labs(x="Longitude", y="Latitude") + ggtitle("AK-Aforested")


####### RK map
RK.gps<-c(-155.315, 19.8216)
mapRK<-get_map(RK.gps,
               zoom=20, 
               scale = 2, 
               maptype= "satellite",
               source="google", extent= "device", legend="topright")
RK.map<-ggmap(mapRK, group=type)+
  geom_point(aes(x=longitude, y=latitude, shape=type, color=type), data=RK, alpha=0.5, size=3)+
  scale_color_manual(values=c('red','mediumseagreen', "dodgerblue"))+
  scale_shape_manual(values=c(16, 17, 3))+
  theme(legend.position="top",
        legend.title=element_blank(), 
        legend.key = element_rect(fill = "white"))+
  labs(x="Longitude", y="") + ggtitle("RK-Remnant")

### export it
png(file= "RK.AK.sitemaps.png", height=5, width=9, units="in", res=300)
grid.arrange(AK.map, RK.map, ncol = 2)
dev.off()

                        