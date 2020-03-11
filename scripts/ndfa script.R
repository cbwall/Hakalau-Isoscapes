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

Ndfa.AK<-Hak.data[(Hak.data$Plot=="AK"),]
Ndfa.RK<-Hak.data[(Hak.data$Plot=="RK"),]

# AK Rubs
AK.R.min<-min(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 0.6
AK.R.max<-max(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 3.15
AK.R.med<-median(Ndfa.AK[(Ndfa.AK$Sample=="RUBHAW"),]$d15N) # 1.94

# RK Rubs
RK.R.min<-min(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 0.99
RK.R.max<-max(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 4.53
RK.R.med<-median(Ndfa.RK[(Ndfa.RK$Sample=="RUBARG"),]$d15N) # 3.03

# AK and RK Koa dataframes
AK.Koa.df<-Ndfa.AK[(Ndfa.AK$Sample=="Koa"),]
RK.Koa.df<-Ndfa.AK[(Ndfa.RK$Sample=="Koa"),]

# AK koa Ndfa
AK.Koa.df<- AK.Koa.df %>%
  mutate(nfix.AK.min = (AK.R.min - d15N)/(AK.R.min-Beta0.5)*100) %>%
  mutate(nfix.AK.max = (AK.R.min - d15N)/(AK.R.max-Beta0.5)*100)  %>%
  mutate(nfix.AK.med = (AK.R.min - d15N)/(AK.R.med-Beta0.5)*100)

# RK koa Ndfa
RK.Koa.df<- RK.Koa.df %>%
  mutate(nfix.RK.min = (RK.R.min - d15N)/(RK.R.min-Beta0.5)*100) %>%
  mutate(nfix.RK.max = (RK.R.min - d15N)/(RK.R.max-Beta0.5)*100)  %>%
  mutate(nfix.RK.med = (RK.R.min - d15N)/(RK.R.med-Beta0.5)*100)

