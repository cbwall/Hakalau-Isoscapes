
#### Degree of nitrogen fixation (Ndfa %)  
#the degree of nitrogen fixation takes into account the target nitrogen fixer (Koa) and a non-fixing reference plant (Rubus spp.) and Beta. Beta is the value when all nitrgen % in fixing plant comes from nitrogen fixation and is estimated, usually as -2 to 0. The formula is:  
#  $Ndfa (\%) = (δ^{15}N~ref~ - δ^{15}N~fixer~) / (δ^{15}N~ref~ - Beta)* 100$   
    
#Here we use Rubus spp. maximum and median d15N values (pooled across sites) are used as reference plants and a Beta of -1 and -2 to see the % Ndfa of Koa across its range of d15N values.   
  
#```{r Beta Ndfa, fig.cap="Figure 2b-c. The degree of fixed nitrogen from the atmospheric (Ndfa) contributing to Acacia koa percent nitrogen using two assumptions for Beta (δ15N value where Ndfa=100%)", fig.align='center'}
  # Beta and Ndfa
  # ability for Koa to fix
  
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
  
  ############## TO USE
  # all mean, all min, all max
  All.Rub<-Ndfa.Hak[(Ndfa.Hak$Sample=="RUBHAW" | Ndfa.Hak$Sample=="RUBARG"),]
  Rub.av<-mean(All.Rub$d15N) # 2.59
  Rub.med<-median(All.Rub$d15N) # 2.59
  Rub.min<-min(All.Rub$d15N) # 0.6
  Rub.max<-max(All.Rub$d15N) # 4.53
  
  # AK and RK Koa dataframes
  All.Koa<- Ndfa.Hak[(Ndfa.Hak$Sample=="Koa"),]
  
  AK.Koa.df<-Ndfa.AK[(Ndfa.AK$Sample=="Koa"),]
  RK.Koa.df<-Ndfa.RK[(Ndfa.RK$Sample=="Koa"),]
  
  ##### Beta -1 plot
  Beta.neg1<- -1
  Beta.neg2<- -2
  
  
  #####################
  # Beta negative 1
  # AK koa Ndfa
  All.Koa.betaneg1<- All.Koa %>%
    mutate(nfix.min = (Rub.min - d15N)/(Rub.min-Beta.neg1)*100) %>%
    mutate(nfix.max = (Rub.max - d15N)/(Rub.max-Beta.neg1)*100)  %>%
    mutate(nfix.med = (Rub.med - d15N)/(Rub.med-Beta.neg1)*100)
  
  ##### Site specific
  # Beta negative 1
  # AK koa Ndfa
  AK.Koa.df.betneg1<- AK.Koa.df %>%
    mutate(nfix.AK.min = (Rub.min - d15N)/(Rub.min-Beta.neg1)*100) %>%
    mutate(nfix.AK.max = (Rub.max - d15N)/(Rub.max-Beta.neg1)*100)  %>%
    mutate(nfix.AK.med = (Rub.med - d15N)/(Rub.med-Beta.neg1)*100)
  
  # RK koa Ndfa
  RK.Koa.df.betneg1<- RK.Koa.df %>%
    mutate(nfix.RK.min = (Rub.min - d15N)/(Rub.min-Beta.neg1)*100) %>%
    mutate(nfix.RK.max = (Rub.max - d15N)/(Rub.max-Beta.neg1)*100)  %>%
    mutate(nfix.RK.med = (Rub.med - d15N)/(Rub.med-Beta.neg1)*100)
  
  
  ## Max
  par(mfrow=c(1,2), mar=c(5,5,3,3))
  
  # build empty plot
  plot(data=All.Koa.betaneg1, d15N~nfix.max, yaxt="n", xaxt="n", ylim=c(-2,3.5), xlim=c(100, -30), 
       xlab="", ylab="", type="n")
  
  # add shaded areas
  # max and median are highest and lowest respectively
  
  # models Max
  B1.mod.Koa.max<-lm(d15N~nfix.max, data=All.Koa.betaneg1) 
  
  # models Median
  B1.mod.Koa.med<-lm(d15N~nfix.med, data=All.Koa.betaneg1)
  
  par(new=T)
  
  #for convex hull
  x <- seq(-40,100, 1) # sequence
  ymax.beta1 <- x*(B1.mod.Koa.max$coefficients[2])+B1.mod.Koa.max$coefficients[1] # x*slope + intercept
  ymed.beta1 <- x*(B1.mod.Koa.med$coefficients[2])+B1.mod.Koa.med$coefficients[1]
  polygon(c(rev(x), x),c(rev(ymax.beta1), ymed.beta1),col=adjustcolor("palegreen3",alpha.f=0.2), border=NA)
  
  # add model lines
  abline(B1.mod.Koa.max, col="black", lty=1)
  
  # plot points and model fits
  par(new=T)
  
  # AK 
  plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.max, col="coral",
       xlab=expression(paste("Ndfa-", italic("Acacia koa"), " (%)", sep="")),
       ylab= expression(paste(italic("Acacia koa "),delta^{15}, N, " (\u2030)")), cex.axis=0.7, 
       main= "Beta = -1", cex.main=0.7,
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.max, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  legend(x=135, y=3.7, x.intersp =0.15, cex=0.8, legend= c("AK", "RK","means"), bg="transparent", 
         col=c("coral", "dodgerblue", "black"), 
         pch=c(20, 20, 21), box.lty=0)
  
  
  ## Median
  # AK 
  par(new=T)
  # add model lines
  abline(B1.mod.Koa.med, col="black", lty=2)
  
  par(new=T)
  plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.med, col="coral", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.med, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  legend(x=95, y=-1, x.intersp =0.25, legend= c("max", "median"), bg="transparent",
         col="black", lty=c(1,2), seg.len=0.4, cex=0.7, box.lty=0, pch=21)
  
  
  # means, FOR max
  AK.max.mean.nfixB1<-mean(AK.Koa.df.betneg1$nfix.AK.max) # mean 58.6
  RK.max.mean.nfixB1<-mean(RK.Koa.df.betneg1$nfix.RK.max) # mean 43.0
  
  # means, FOR median
  AK.med.mean.nfixB1<-mean(AK.Koa.df.betneg1$nfix.AK.med) # mean 36.3
  RK.med.mean.nfixB1<-mean(RK.Koa.df.betneg1$nfix.RK.med) # mean 12.3
  
  # y d15N means
  AK.mean.d15NB1<-mean(AK.Koa.df.betneg1$d15N) # mean 1.28
  RK.mean.d15NB1<-mean(RK.Koa.df.betneg1$d15N) # mean 2.15
  
  ######
  # add to plot (max)
  points(x=AK.max.mean.nfixB1, y=AK.mean.d15NB1, pch=1, col="firebrick3", cex=1.2) # AK means
  points(x=RK.max.mean.nfixB1, y=RK.mean.d15NB1, pch=1, col="blue", cex=1.2) # RK means
  
  # add to plot (median)
  points(x=AK.med.mean.nfixB1, y=AK.mean.d15NB1, pch=1, col="firebrick3", cex=1.2) # AK means
  points(x=RK.med.mean.nfixB1, y=RK.mean.d15NB1, pch=1, col="blue", cex=1.2) # RK means
  
  
  
  ###################
  ###################
  # Beta negative 2
  
  ##### Beta -2 plot
  # Beta negative 2
  # AK koa Ndfa
  # Beta negative 1
  # AK koa Ndfa
  All.Koa.betaneg2<- All.Koa %>%
    mutate(nfix.min = (Rub.min - d15N)/(Rub.min-Beta.neg2)*100) %>%
    mutate(nfix.max = (Rub.max - d15N)/(Rub.max-Beta.neg2)*100)  %>%
    mutate(nfix.med = (Rub.med - d15N)/(Rub.med-Beta.neg2)*100)
  
  ##### Site specific
  # Beta negative 1
  # AK koa Ndfa
  AK.Koa.df.betneg2<- AK.Koa.df %>%
    mutate(nfix.AK.min = (Rub.min - d15N)/(Rub.min-Beta.neg2)*100) %>%
    mutate(nfix.AK.max = (Rub.max - d15N)/(Rub.max-Beta.neg2)*100)  %>%
    mutate(nfix.AK.med = (Rub.med - d15N)/(Rub.med-Beta.neg2)*100)
  
  # RK koa Ndfa
  RK.Koa.df.betneg2<- RK.Koa.df %>%
    mutate(nfix.RK.min = (Rub.min - d15N)/(Rub.min-Beta.neg2)*100) %>%
    mutate(nfix.RK.max = (Rub.max - d15N)/(Rub.max-Beta.neg2)*100)  %>%
    mutate(nfix.RK.med = (Rub.med - d15N)/(Rub.med-Beta.neg2)*100)
  
  
  ## Max
  # build empty plot
  plot(data=All.Koa.betaneg2, d15N~nfix.max, yaxt="n", xaxt="n", ylim=c(-2,3.5), xlim=c(100, -30), 
       xlab="", ylab="", type="n")
  
  # add shaded areas
  # max and median are highest and lowest respectively
  
  # models Max
  B2.mod.Koa.max<-lm(d15N~nfix.max, data=All.Koa.betaneg2) 
  
  # models Median
  B2.mod.Koa.med<-lm(d15N~nfix.med, data=All.Koa.betaneg2)
  
  par(new=T)
  
  #for convex hull
  x <- seq(-40,100, 1) # sequence
  ymax.beta1 <- x*(B2.mod.Koa.max$coefficients[2])+B2.mod.Koa.max$coefficients[1] # x*slope + intercept
  ymed.beta1 <- x*(B2.mod.Koa.med$coefficients[2])+B2.mod.Koa.med$coefficients[1]
  polygon(c(rev(x), x),c(rev(ymax.beta1), ymed.beta1),col=adjustcolor("palegreen3",alpha.f=0.2), border=NA)
  
  # add model lines
  abline(B2.mod.Koa.max, col="black", lty=1)
  
  # plot points and model fits
  par(new=T)
  
  # AK 
  plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.max, col="coral", yaxt="n" ,
       xlab=expression(paste("Ndfa-", italic("Acacia koa"), " (%)", sep="")),
       ylab= "", cex.axis=0.7, 
       main= "Beta = -2", cex.main=0.7,
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  axis(side=2, labels=F)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.max, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  
  ## Median
  # AK 
  par(new=T)
  # add model lines
  abline(B2.mod.Koa.med, col="black", lty=2)
  
  par(new=T)
  plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.med, col="coral", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.med, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  
  # means, FOR max
  AK.max.mean.nfixB2<-mean(AK.Koa.df.betneg2$nfix.AK.max) # mean 58.6
  RK.max.mean.nfixB2<-mean(RK.Koa.df.betneg2$nfix.RK.max) # mean 43.0
  
  # means, FOR median
  AK.med.mean.nfixB2<-mean(AK.Koa.df.betneg2$nfix.AK.med) # mean 36.3
  RK.med.mean.nfixB2<-mean(RK.Koa.df.betneg2$nfix.RK.med) # mean 12.3
  
  # y d15N means
  AK.mean.d15NB2<-mean(AK.Koa.df.betneg2$d15N) # mean 1.28
  RK.mean.d15NB2<-mean(RK.Koa.df.betneg2$d15N) # mean 2.15
  
  ######
  # add to plot (max)
  points(x=AK.max.mean.nfixB2, y=AK.mean.d15NB2, pch=1, col="firebrick3", cex=1.2) # AK means
  points(x=RK.max.mean.nfixB2, y=RK.mean.d15NB2, pch=1, col="blue", cex=1.2) # RK means
  
  # add to plot (median)
  points(x=AK.med.mean.nfixB2, y=AK.mean.d15NB2, pch=1, col="firebrick3", cex=1.2) # AK means
  points(x=RK.med.mean.nfixB2, y=RK.mean.d15NB2, pch=1, col="blue", cex=1.2) # RK means
  
  
  dev.copy(pdf, "figures/Ndfa.update.pdf", width=7, height=5, encod="MacRoman")
  dev.off()
  
  ```
  
  ```{r Ndfa with site different refs, eval=FALSE}
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
  par(mfrow=c(1,2), mar=c(5,5,3,3))
  
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
  polygon(c(rev(x), x),c(rev(ymax.AK), ymed.AK),col=adjustcolor("coral",alpha.f=0.2), border=NA)
  
  #for RK
  ymax.RK <- x*(B1.mod.RKoa.max$coefficients[2])+B1.mod.RKoa.max$coefficients[1] # x*slope + intercept
  ymed.RK <- x*(B1.mod.RKoa.med$coefficients[2])+B1.mod.RKoa.med$coefficients[1]
  polygon(c(rev(x), x),c(rev(ymax.RK), ymed.RK),col=adjustcolor("dodgerblue",alpha.f=0.2), border=NA)
  
  # plot points and model fits
  par(new=T)
  
  # AK 
  plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.max, col="coral",
       xlab=expression(paste("Ndfa-", italic("Acacia koa"), " (%)", sep="")),
       ylab= expression(paste(delta^{15}, N, " (\u2030)")), cex.axis=0.7, main= "Beta = -1", cex.main=0.7,
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.max, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  legend(x=135, y=3.7, x.intersp =0.15, cex=0.8, legend= c("AK", "RK"), bg="transparent", 
         col=c("coral", "dodgerblue"), 
         pch=20, box.lty=0)
  
  # add model lines
  abline(B1.mod.AKoa.max, col="coral", lty=1)
  abline(B1.mod.RKoa.max, col="royalblue3", lty=1)
  
  ## Median
  # AK 
  par(new=T)
  plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.med, col="coral", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.med, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  legend(x=95, y=-1, x.intersp =0.25, legend= c("max", "site median", "pooled  median"), bg="transparent",
         col="black", lty=c(1,4,5), seg.len=0.4, cex=0.7, box.lty=0, pch=c(16,16,1))
  
  # model
  B1.mod.AKoa.med<-lm(d15N~nfix.AK.med, data=AK.Koa.df.betneg1)
  B1.mod.RKoa.med<-lm(d15N~nfix.RK.med, data=RK.Koa.df.betneg1)
  
  # add model lines
  abline(B1.mod.AKoa.med, col="coral", lty=4)
  abline(B1.mod.RKoa.med, col="royalblue3", lty=4)
  
  
  ###
  #### all sample median
  B1.mod.allKoa.med<-lm(d15N~nfix.allKoa.med, data=All.Koa.neg1)
  # add model lines
  abline(B1.mod.allKoa.med, col="gray40", lty=5)
  
  par(new=T)
  #AK
  plot(data=AK.Koa.df.betneg1, d15N~nfix.AK.all.med, col="coral", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")
  
  par(new=T)
  #RK
  plot(data=RK.Koa.df.betneg1, d15N~nfix.RK.all.med, col="dodgerblue", xlab="", ylab= "", 
       bty="n", xaxt="n", yaxt="n", 
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")
  
  # means, add to plot
  AK.mean.nfix<-mean(AK.Koa.df.betneg1$nfix.AK.all.med) # mean 36.3
  AK.mean.d15N<-mean(AK.Koa.df.betneg1$d15N) # mean 1.28
  RK.mean.nfix<-mean(RK.Koa.df.betneg1$nfix.RK.all.med) # mean 12.3
  RK.mean.d15N<-mean(RK.Koa.df.betneg1$d15N) # mean 2.15
  
  points(x=AK.mean.nfix, y=AK.mean.d15N, pch=3, col="firebrick3", lwd=1.5, cex=0.9) # AK means
  points(x=RK.mean.nfix, y=RK.mean.d15N, pch=3, col="blue", lwd=1.5, cex=0.9) # RK means
  
  
  
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
  polygon(c(rev(x), x),c(rev(ymax.AK), ymed.AK),col=adjustcolor("coral",alpha.f=0.2), border=NA)
  
  #for RK
  ymax.RK <- x*(B1.mod.RKoa.max$coefficients[2])+B1.mod.RKoa.max$coefficients[1] # x*slope + intercept
  ymed.RK <- x*(B1.mod.RKoa.med$coefficients[2])+B1.mod.RKoa.med$coefficients[1]
  polygon(c(rev(x), x),c(rev(ymax.RK), ymed.RK),col=adjustcolor("dodgerblue",alpha.f=0.2), border=NA)
  
  # plot points and model fits
  par(new=T)
  
  # AK 
  plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.max, col="coral",
       xlab=expression(paste("Ndfa-", italic("Acacia koa"), " (%)", sep="")),
       cex.axis=0.7, main= "Beta = -2", cex.main=0.7, ylab="",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.max, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  
  # add model lines
  abline(B1.mod.AKoa.max, col="coral", lty=1)
  abline(B1.mod.RKoa.max, col="royalblue3", lty=1)
  
  ## Median
  # AK 
  par(new=T)
  plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.med, col="coral", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  #RK
  par(new=T)
  plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.med, col="dodgerblue", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=20)
  
  
  # model
  B1.mod.AKoa.med<-lm(d15N~nfix.AK.med, data=AK.Koa.df.betneg2)
  B1.mod.RKoa.med<-lm(d15N~nfix.RK.med, data=RK.Koa.df.betneg2)
  
  # add model lines
  abline(B1.mod.AKoa.med, col="coral", lty=4)
  abline(B1.mod.RKoa.med, col="royalblue3", lty=4)
  
  
  ###
  #### all sample median
  B1.mod.allKoa.med<-lm(d15N~nfix.allKoa.med, data=All.Koa.neg2)
  
  # add model lines
  abline(B1.mod.allKoa.med, col="gray40", lty=5)
  
  par(new=T)
  #AK
  plot(data=AK.Koa.df.betneg2, d15N~nfix.AK.all.med, col="coral", xlab="", ylab= "", xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")
  
  par(new=T)
  #RK
  plot(data=RK.Koa.df.betneg2, d15N~nfix.RK.all.med, col="dodgerblue", xlab="", ylab= "", 
       xaxt="n", yaxt="n", bty="n",
       ylim=c(-2,3.5), xlim=c(100, -30), cex=0.7, pch=21, bg="white")
  legend(x=110, y=-1.5, x.intersp =0.25, legend= c("AK-mean", "RK-mean"), bg="transparent",
         col=c("firebrick3", "blue"), cex=0.6, box.lty=0, pch=3)
  
  # means, add to plot
  AK.mean.nfix<-mean(AK.Koa.df.betneg2$nfix.AK.all.med) # mean 28.4
  AK.mean.d15N<-mean(AK.Koa.df.betneg2$d15N) # mean 1.28
  RK.mean.nfix<-mean(RK.Koa.df.betneg2$nfix.RK.all.med) # mean 9.64
  RK.mean.d15N<-mean(RK.Koa.df.betneg2$d15N) # mean 2.25
  
  points(x=AK.mean.nfix, y=AK.mean.d15N, pch=3, col="firebrick3", lwd=1.5, cex=0.9) # AK means
  points(x=RK.mean.nfix, y=RK.mean.d15N, pch=3, col="blue", lwd=1.5, cex=0.9) # RK means
  
  
  dev.copy(pdf, "figures/Ndfa.pdf", width=7, height=5, encod="MacRoman")
  dev.off()
  
  
#if comparing in linear model the % Ndfa, they are only different (higher in AK) if you use a single shared value, in this case the grouped median (i.e, pooled Rubus d15N value across the 2 sites). FYI the mean and median are essentially the same at 2.6 permil. This is the same results if using the offset Beat -1 or -2. If using the site-specifc median and maximum value the mean %Ndfa is the same for both sites. Why? Rubus is likely influenced by the Koa, therefore it is netting a + Ndfa% through Koa/s contribution to surrounding plants. However this d15N signal from Koa is not obvious in the soils of these sites.
  
  ```{r Ndfa stats, eval=FALSE}
  # tests
  ### Beta 1
  Beta1.AK<-AK.Koa.df.betneg1[,c(1:3,5,6,8)]
  names(Beta1.AK)<- c("Plot", "Sample", "d15N", "fix.max", "fix.med", "allfix.med")
  
  Beta1.RK<-RK.Koa.df.betneg1[,c(1:3,5,6,8)]
  names(Beta1.RK)<- c("Plot", "Sample", "d15N", "fix.max", "fix.med", "allfix.med")
  
  Beta1.df<-rbind(Beta1.AK, Beta1.RK)
  
  # test differences between the 2 forest types 
  anova(lm(fix.max~Plot, data=Beta1.df)) # max, NS
  anova(lm(fix.med~Plot, data=Beta1.df)) # med, NS
  anova(lm(allfix.med~Plot, data=Beta1.df)) # med *p=0.114
  
  ### Beta 2
  Beta2.AK<-AK.Koa.df.betneg2[,c(1:3,5,6,8)]
  names(Beta2.AK)<- c("Plot", "Sample", "d15N", "fix.max", "fix.med", "allfix.med")
  
  Beta2.RK<-RK.Koa.df.betneg2[,c(1:3,5,6,8)]
  names(Beta2.RK)<- c("Plot", "Sample", "d15N", "fix.max", "fix.med", "allfix.med")
  
  Beta2.df<-rbind(Beta2.AK, Beta2.RK)
  
  # test differences between the 2 forest types 
  anova(lm(fix.max~Plot, data=Beta2.df)) # max, NS
  anova(lm(fix.med~Plot, data=Beta2.df)) # med, NS
  anova(lm(allfix.med~Plot, data=Beta2.df)) # med *p=0.114
  
  