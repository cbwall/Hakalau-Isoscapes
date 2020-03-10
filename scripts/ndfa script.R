

# calculations
# delta values to R ratios
# First d15N standard is nitrogen air -- Rstd =15N/14N = 0.0036765  and   %15N = 0.3663
# d15N values to R.... d15N = (Rsample/Rstandard -1) *1000
# continued...  Rsample = (d15N/1000 +1)*Rstandard, where Rstd = 0.0036765

# from R standard and R sample calculate F, then atom %
# F-heavy = (delta +1000) / [(delta+1000 + (1000/Rstandard))]

# F to Atom %
# Heavy-AP = 100*F-heavy

Ndfa<-read.csv("data/Ndfa.Rub.csv")
Rstd<-0.0036765 

# calculate R sample for Koa
Ndfa<- Ndfa %>% 
  mutate(koa.Rsam = (mean.d15N.Ndfa/1000 +1)*Rstd)

# calcualte F-values for koa

Ndfa<- Ndfa %>% 
  mutate(koa.Fval = (mean.d15N.Ndfa+1000)/(mean.d15N.Ndfa+1000 +(1000/Rstd)))

# calcualte %15N for koa
Ndfa<- Ndfa %>% 
  mutate(koa.per.15N = koa.Fval*100)

Ndfa$d15N.Rub<-Ndfa$d15N
Ndfa$d15N.Koa<-Ndfa$mean.d15N.Ndfa
Beta0.5<-0.5
Beta0<-0
Beta.neg1<- -1.0
Beta.neg2<- -2.0

# Ndfa of rubus
Ndfa$Diff<-100*(Ndfa$d15N.Rub - Ndfa$d15N.Koa)/(Beta.neg1- Ndfa$d15N.Rub)

mod.AK<-lm(Ndfa[(Ndfa$Plot=="AK"),]$d15N.Rub~Ndfa[(Ndfa$Plot=="AK"),]$Diff)
mod.RK<-lm(Ndfa[(Ndfa$Plot=="RK"),]$d15N.Rub~Ndfa[(Ndfa$Plot=="RK"),]$Diff)

plot(Ndfa$d15N.Rub~Ndfa$Diff, col=Ndfa$Plot, xlab="Ndfa-from koa (%)", ylab= "Rubus d15N (permil)")
abline(mod.AK)
abline(mod.RK, col="red")

