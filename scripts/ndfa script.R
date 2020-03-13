############################

#load data
Ndfa<-read.csv("data/Ndfa.Rub.csv")

# rename the d15N so we know it is for rubus
Ndfa$d15N.Rub<-Ndfa$d15N
Ndfa$d15N.Koa<-Ndfa$mean.d15N.Ndfa

####################
#################### relationship between koa and rubus d15N?
# Yes, but a bit stronger in the RK site
mod.AK<-lm(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Rub~Ndfa[(Ndfa$Plot=="AK"),]$mean.d15N.Ndfa); anova(mod.AK)
int.rubAK<-coef(summary(mod.AK))[1] #intercept
slop.rubAK<-coef(summary(mod.AK))[2] #slope
                                  
mod.RK<-lm(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Rub~Ndfa[(Ndfa$Plot=="RK"),]$mean.d15N.Ndfa); anova(mod.RK)
int.RKoa<-coef(summary(mod.RK))[1] #intercept
slop.RKoa<-coef(summary(mod.RK))[2] #slope

#### plot
plot(Ndfa$d15N.Rub~Ndfa$d15N.Koa, col=Ndfa$Plot, xlab="Koa d15N (permil)", ylab= "Rubus d15N (permil)")
legend("topleft", legend=levels(Ndfa$Plot), col=c("black", "red"), lwd=1, pch=21, box.lty=0)
ablineclip(int.rubAK, slop.rubAK, col="black",
           x1 = min(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Koa, na.rm=T), 
           x2 = max(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Koa, na.rm=T)) # AK model
ablineclip(int.RKoa, slop.RKoa, col="red",
           x1 = min(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Koa, na.rm=T), 
           x2 = max(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Koa, na.rm=T)) # RK model

############################
# calculations
# delta values to R ratios
# First d15N standard is nitrogen air -- Rstd =15N/14N = 0.0036765  and   %15N = 0.3663
# d15N values to R.... d15N = (Rsample/Rstandard -1) *1000
# continued...  Rsample = (d15N/1000 +1)*Rstandard, where Rstd = 0.0036765

# from R standard and R sample calculate F, then atom %
# F-heavy = (delta +1000) / [(delta+1000 + (1000/Rstandard))]

# F to Atom %
# Heavy-AP = 100*F-heavy

Rstd<-0.0036765 
N.at.ex.st<-0.3663

# calculate R sample for Koa
Ndfa<- Ndfa %>% 
  mutate(koa.Rsam = (d15N.Koa/1000 +1)*Rstd)

# calcualte F-values for koa

Ndfa<- Ndfa %>% 
  mutate(koa.Fval = (d15N.Koa+1000)/(d15N.Koa+1000 +(1000/Rstd)))

# calcualte %15N for koa
Ndfa<- Ndfa %>% 
  mutate(koa.per.15N = koa.Fval*100)

# calcualte %15N-atom excess for koa
Ndfa<- Ndfa %>% 
  mutate(koa.N.At.ex = koa.per.15N-N.at.ex.st)

### Now do the same for Rub

# calculate R sample for Rub
R.Rub<-(Ndfa$d15N.Rub/1000 +1)*Rstd
# calcualte F-values for Rub
F.Rub<-(Ndfa$d15N.Rub+1000)/(Ndfa$d15N.Rub+1000 +(1000/Rstd))
# calcualte %15N for Rub
Rub.per15N<-F.Rub*100
# calcualte %15N-atom excess for Rub
Ndfa<- Ndfa %>% 
  mutate(Rub.N.At.ex = Rub.per15N-N.at.ex.st)

##########################################
# examine the relationship

#######
Beta0.5<-0.5
Beta0<-0
Beta.neg1<- -1.0
Beta.neg2<- -2.0

# Ndf of koa
Ndfa$Diff.5<-((Ndfa$d15N.Rub - Ndfa$d15N.Koa)/(Ndfa$d15N.Rub-Beta0.5))*100
Ndfa$Diff.neg1<-((Ndfa$d15N.Rub - Ndfa$d15N.Koa)/(Ndfa$d15N.Rub-Beta.neg1))*100
Ndfa$Diff.neg2<-((Ndfa$d15N.Rub - Ndfa$d15N.Koa)/(Ndfa$d15N.Rub-Beta.neg2))*100

######### plot 0.5
# plot rub vs diff
plot(Ndfa$d15N.Koa~Ndfa$Diff.5, col=c("salmon","lightseagreen")[as.factor(Ndfa$Plot)], xlab="Ndf-koa (%)", ylab= "Koa d15N (permil)",
     ylim=c(-2,5), xlim=c(100, -30), cex=0.7, pch=20)
legend("topleft", legend= c("AK", "RK"), 
       col=c("salmon", "lightseagreen"), 
       lty=c(1,1), pch=20, box.lty=0)

# model for Koa
mod.AKoa<-lm(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Koa~Ndfa[(Ndfa$Plot=="AK"),]$Diff.5)
mod.RKoa<-lm(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Koa~Ndfa[(Ndfa$Plot=="RK"),]$Diff.5)

# add model lines
abline(mod.AKoa, col="coral", lty=1)
abline(mod.RKoa, col="darkcyan", lty=1)


######### plot beta =-1
# plot rub vs diff
par(new=T)
plot(Ndfa$d15N.Koa~Ndfa$Diff.neg1,  col=c("salmon","lightseagreen")[as.factor(Ndfa$Plot)], xaxt="n", yaxt="n", xlab="", ylab="",
     ylim=c(-2,5), xlim=c(100, -30),  cex=0.7, pch=20)

# model for Koa
mod.AKoa<-lm(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Koa~Ndfa[(Ndfa$Plot=="AK"),]$Diff.neg1)
mod.RKoa<-lm(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Koa~Ndfa[(Ndfa$Plot=="RK"),]$Diff.neg1)

# add model lines
abline(mod.AKoa, col="coral", lty=2)
abline(mod.RKoa, col="darkcyan", lty=2)


######### plot beta = -2
# plot rub vs diff

par(new=T)
plot(Ndfa$d15N.Koa~Ndfa$Diff.neg2,  col=c("salmon","lightseagreen")[as.factor(Ndfa$Plot)], xaxt="n", yaxt="n", xlab="", ylab="",
     ylim=c(-2,5), xlim=c(100, -30), cex=0.7, pch=20)
legend("topright", legend= c("Beta = 0.5", "Beta = -1", "Beta = -2"), 
       col="black", lty=c(1,2,3),
       lwd=2, box.lty=0)

# model for Koa
mod.AKoa<-lm(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Koa~Ndfa[(Ndfa$Plot=="AK"),]$Diff.neg2)
mod.RKoa<-lm(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Koa~Ndfa[(Ndfa$Plot=="RK"),]$Diff.neg2)

# add model lines
abline(mod.AKoa, col="coral", lty=3)
abline(mod.RKoa, col="darkcyan", lty=3)













################# Need new dataframe of KOA and Rubus 
########### separate data frame
# full Hak data dataframe
Ndfa.Hak<- Hak.data %>%
  select(c(Plot, Sample, d15N))

Ndfa.AK<-Ndfa.Hak[(Ndfa.Hak$Plot=="AK"),]
Ndfa.RK<-Ndfa.Hak[(Ndfa.Hak$Plot=="RK"),]

# AK Rubs
AK.R.min<-min(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 0.6
AK.R.max<-max(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 3.15
AK.R.med<-median(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 1.94
AK.R.av<-mean(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 1.9

# RK Rubs
RK.R.min<-min(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 0.99
RK.R.max<-max(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 4.53
RK.R.med<-median(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 3.03
RK.R.av<-mean(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 2.93

# all mean, all mode
All.Rub<-Ndfa.Hak[(Ndfa.Hak$Sample=="RUBHAW" | Ndfa.Hak$Sample=="RUBARG"),]
Rub.av<-mean(All.Rub$d15N) # 2.59
Rub.med<-median(All.Rub$d15N) # 2.59

# AK and RK Koa dataframes
All.Koa<- Ndfa.Hak[(Ndfa.Hak$Sample=="Koa"),]

AK.Koa.df<-Ndfa.AK[(Ndfa.AK$Sample=="Koa"),]
RK.Koa.df<-Ndfa.RK[(Ndfa.RK$Sample=="Koa"),]


##### Beta -1 plot
# Beta negative 1
# AK koa Ndfa
AK.Koa.df.betneg1<- AK.Koa.df %>%
  mutate(nfix.AK.min = (AK.R.min - d15N)/(AK.R.min-Beta.neg1)*100) %>%
  mutate(nfix.AK.max = (AK.R.max - d15N)/(AK.R.max-Beta.neg1)*100)  %>%
  mutate(nfix.AK.med = (AK.R.med - d15N)/(AK.R.med-Beta.neg1)*100) %>%
  mutate(nfix.AK.av = (Rub.av - d15N)/(Rub.av-Beta.neg1)*100) %>%
  mutate(nfix.AK.all.med = (Rub.med - d15N)/(Rub.med-Beta.neg1)*100)

# RK koa Ndfa
RK.Koa.df.betneg1<- RK.Koa.df %>%
  mutate(nfix.RK.min = (RK.R.min - d15N)/(RK.R.min-Beta.neg1)*100) %>%
  mutate(nfix.RK.max = (RK.R.max - d15N)/(RK.R.max-Beta.neg1)*100)  %>%
  mutate(nfix.RK.med = (RK.R.med - d15N)/(RK.R.med-Beta.neg1)*100) %>%
  mutate(nfix.RK.av = (Rub.av - d15N)/(Rub.av-Beta.neg1)*100) %>%
  mutate(nfix.RK.all.med = (Rub.med - d15N)/(Rub.med-Beta.neg1)*100)

All.Koa.neg1<- All.Koa %>%
  mutate(nfix.allKoa.med = (Rub.med - d15N)/(Rub.med-Beta.neg1)*100)

## Max
par(mfrow=c(1,2), mar=c(5,5,4,2))

# build empty plot
plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.max, yaxt="n", xaxt="n", ylim=c(-2,3.5), xlim=c(100, -30), 
     xlab="", ylab="", type="n")

# add shaded areas
# max and median are highest and lowest respectively

# models Max (highest)
B1.mod.AKoa.max<-lm(d15N~nfix.AK.max, data=AK.Koa.df.betneg1) 
B1.mod.RKoa.max<-lm(d15N~nfix.RK.max, data=RK.Koa.df.betneg1)

# models Median (lowest)
B1.mod.AKoa.med<-lm(d15N~nfix.AK.med, data=AK.Koa.df.betneg1)
B1.mod.RKoa.med<-lm(d15N~nfix.RK.med, data=RK.Koa.df.betneg1)

par(new=T)

#for AK
x <- seq(-40,100, 1) # sequence
ymax.AK <- x*(B1.mod.AKoa.max$coefficients[2])+B1.mod.AKoa.max$coefficients[1] # x*slope + intercept
ymed.AK <- x*(B1.mod.AKoa.med$coefficients[2])+B1.mod.AKoa.med$coefficients[1]
polygon(c(rev(x), x),c(rev(ymax.AK), ymed.AK),col=adjustcolor("salmon",alpha.f=0.2), border=NA)

#for RK
ymax.RK <- x*(B1.mod.RKoa.max$coefficients[2])+B1.mod.RKoa.max$coefficients[1] # x*slope + intercept
ymed.RK <- x*(B1.mod.RKoa.med$coefficients[2])+B1.mod.RKoa.med$coefficients[1]
polygon(c(rev(x), x),c(rev(ymax.RK), ymed.RK),col=adjustcolor("lightseagreen",alpha.f=0.2), border=NA)

# plot points and model fits
par(new=T)

# AK 
plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.max, col="salmon", xlab="Ndf-koa (%)",
     ylab= expression(paste(delta^{15}, N, " (\u2030)")), cex.axis=0.7, main= "Beta = -1", cex.main=0.7,
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)

#RK
par(new=T)
plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.max, col="lightseagreen", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
legend("topleft", legend= c("AK", "RK"), bg="transparent", 
       col=c("salmon", "lightseagreen"), 
       pch=20, box.lty=0)

# add model lines
abline(B1.mod.AKoa.max, col="coral", lty=1)
abline(B1.mod.RKoa.max, col="darkcyan", lty=1)

## Median
# AK 
par(new=T)
plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.med, col="salmon", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)

#RK
par(new=T)
plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.med, col="lightseagreen", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
legend("bottom", legend= c("max", "site median", "group  median"), bg="transparent",
       col="black", lty=c(1,4,5), seg.len=1, 
       cex=0.6, box.lty=0)

# model
B1.mod.AKoa.med<-lm(d15N~nfix.AK.med, data=AK.Koa.df.betneg1)
B1.mod.RKoa.med<-lm(d15N~nfix.RK.med, data=RK.Koa.df.betneg1)

# add model lines
abline(B1.mod.AKoa.med, col="coral", lty=4)
abline(B1.mod.RKoa.med, col="darkcyan", lty=4)


###
#### all sample median
B1.mod.allKoa.med<-lm(d15N~nfix.allKoa.med, data=All.Koa.neg1)
# add model lines
abline(B1.mod.allKoa.med, col="gray40", lty=5)

par(new=T)
#AK
plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.all.med, col="salmon", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")

par(new=T)
#RK
plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.all.med, col="lightseagreen", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")

# means, add to plot
AK.mean.nfix<-mean(AK.Koa.df.betneg1$nfix.AK.all.med) # mean 36.3
AK.mean.d15N<-mean(AK.Koa.df.betneg1$d15N) # mean 1.28
RK.mean.nfix<-mean(RK.Koa.df.betneg1$nfix.RK.all.med) # mean 12.3
RK.mean.d15N<-mean(RK.Koa.df.betneg1$d15N) # mean 2.15

points(x=AK.mean.nfix, y=AK.mean.d15N, pch=3, col="firebrick3", lwd=1.5, cex=0.9) # AK means
points(x=RK.mean.nfix, y=RK.mean.d15N, pch=3, col="darkgreen", lwd=1.5, cex=0.9) # RK means



###################
###################
# Beta negative 2

##### Beta -2 plot
# Beta negative 2
# AK koa Ndfa

AK.Koa.df.betneg2<- AK.Koa.df %>%
  mutate(nfix.AK.min = (AK.R.min - d15N)/(AK.R.min-Beta.neg2)*100) %>%
  mutate(nfix.AK.max = (AK.R.max - d15N)/(AK.R.max-Beta.neg2)*100)  %>%
  mutate(nfix.AK.med = (AK.R.med - d15N)/(AK.R.med-Beta.neg2)*100) %>%
  mutate(nfix.AK.av = (Rub.av - d15N)/(Rub.av-Beta.neg2)*100) %>%
  mutate(nfix.AK.all.med = (Rub.med - d15N)/(Rub.med-Beta.neg2)*100)

# RK koa Ndfa
RK.Koa.df.betneg2<- RK.Koa.df %>%
  mutate(nfix.RK.min = (RK.R.min - d15N)/(RK.R.min-Beta.neg2)*100) %>%
  mutate(nfix.RK.max = (RK.R.max - d15N)/(RK.R.max-Beta.neg2)*100)  %>%
  mutate(nfix.RK.med = (RK.R.med - d15N)/(RK.R.med-Beta.neg2)*100) %>%
  mutate(nfix.RK.av = (Rub.av - d15N)/(Rub.av-Beta.neg2)*100) %>%
  mutate(nfix.RK.all.med = (Rub.med - d15N)/(Rub.med-Beta.neg2)*100)

All.Koa.neg2<- All.Koa %>%
  mutate(nfix.allKoa.med = (Rub.med - d15N)/(Rub.med-Beta.neg2)*100)

## Max

# build empty plot
plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.max, yaxt="n", xaxt="n", ylim=c(-2,3.5), xlim=c(100, -30), 
     xlab="", ylab="", type="n")

# add shaded areas
# max and median are highest and lowest respectively

# models Max (highest)
B1.mod.AKoa.max<-lm(d15N~nfix.AK.max, data=AK.Koa.df.betneg2) 
B1.mod.RKoa.max<-lm(d15N~nfix.RK.max, data=RK.Koa.df.betneg2)

# models Median (lowest)
B1.mod.AKoa.med<-lm(d15N~nfix.AK.med, data=AK.Koa.df.betneg2)
B1.mod.RKoa.med<-lm(d15N~nfix.RK.med, data=RK.Koa.df.betneg2)

par(new=T)

#for AK
x <- seq(-40,100, 1) # sequence
ymax.AK <- x*(B1.mod.AKoa.max$coefficients[2])+B1.mod.AKoa.max$coefficients[1] # x*slope + intercept
ymed.AK <- x*(B1.mod.AKoa.med$coefficients[2])+B1.mod.AKoa.med$coefficients[1]
polygon(c(rev(x), x),c(rev(ymax.AK), ymed.AK),col=adjustcolor("salmon",alpha.f=0.2), border=NA)

#for RK
ymax.RK <- x*(B1.mod.RKoa.max$coefficients[2])+B1.mod.RKoa.max$coefficients[1] # x*slope + intercept
ymed.RK <- x*(B1.mod.RKoa.med$coefficients[2])+B1.mod.RKoa.med$coefficients[1]
polygon(c(rev(x), x),c(rev(ymax.RK), ymed.RK),col=adjustcolor("lightseagreen",alpha.f=0.2), border=NA)

# plot points and model fits
par(new=T)

# AK 
plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.max, col="salmon", xlab="Ndf-koa (%)",
    cex.axis=0.7, main= "Beta = -2", cex.main=0.7, ylab="",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)

#RK
par(new=T)
plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.max, col="lightseagreen", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)


# add model lines
abline(B1.mod.AKoa.max, col="coral", lty=1)
abline(B1.mod.RKoa.max, col="darkcyan", lty=1)

## Median
# AK 
par(new=T)
plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.med, col="salmon", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)

#RK
par(new=T)
plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.med, col="lightseagreen", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)


# model
B1.mod.AKoa.med<-lm(d15N~nfix.AK.med, data=AK.Koa.df.betneg2)
B1.mod.RKoa.med<-lm(d15N~nfix.RK.med, data=RK.Koa.df.betneg2)

# add model lines
abline(B1.mod.AKoa.med, col="coral", lty=4)
abline(B1.mod.RKoa.med, col="darkcyan", lty=4)


###
#### all sample median
B1.mod.allKoa.med<-lm(d15N~nfix.allKoa.med, data=All.Koa.neg2)

# add model lines
abline(B1.mod.allKoa.med, col="gray40", lty=5)

par(new=T)
#AK
plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.all.med, col="salmon", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")

par(new=T)
#RK
plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.all.med, col="lightseagreen", xlab="", ylab= "", xaxt="n", yaxt="n",
     ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")

# means, add to plot
AK.mean.nfix<-mean(AK.Koa.df.betneg2$nfix.AK.all.med) # mean 28.4
AK.mean.d15N<-mean(AK.Koa.df.betneg2$d15N) # mean 1.28
RK.mean.nfix<-mean(RK.Koa.df.betneg2$nfix.RK.all.med) # mean 9.64
RK.mean.d15N<-mean(RK.Koa.df.betneg2$d15N) # mean 2.25

points(x=AK.mean.nfix, y=AK.mean.d15N, pch=3, col="firebrick3", lwd=1.5, cex=0.9) # AK means
points(x=RK.mean.nfix, y=RK.mean.d15N, pch=3, col="darkgreen", lwd=1.5, cex=0.9) # RK means


dev.copy(pdf, "figures/Ndfa.pdf", width=8, height=5, encod="MacRoman")
dev.off()
