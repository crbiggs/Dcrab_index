## GAM selection with Indicator variables and Annual landings. 



library(mgcv)

library(DHARMa)
library(tidyverse)
theme_set(theme_light())



#I5gam - Selecting the standardized indicators for GAM analysis that are lagged by 4 years
I5gam <- I5[,c(1,41,9,35,38,10,31,8,32,36,37,33)]
I5gam <- I5gam[37:59,]## Limiting to 2003:2025 years of data  


## Gamma distributions 

Gam1<- gam(Landkg~  s(sSST_lag4, k=4)+ s(sSTI48_lag4, k=4)+ s(scuti_lag4, k=4) + s(sbeuti_lag4, k=4)+
             s(sTumi_lag4, k=4) + s(sLusi_lag4, k=4) + s(sHCI_lag4, k=4) + s(sDO50_lag4, k=4) + s(sDO150_lag4, k=4) +
             s(sWind_lag4, k=4), data=I5gam,Gamma(link="log"), method="REML")
summary(Gam1)
AIC(Gam1)
plot(Gam1)
simulateResiduals(Gam1, plot=T)
gam.check(Gam1)

Gam1<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4), data=I5gam,Gamma(link="log"), method="REML")
summary(Gam1)
AIC(Gam1)
plot(Gam1)
simulateResiduals(Gam1, plot=T)
gam.check(Gam1)

Gam2<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sHCI_lag4, k=4), data=I5gam,Gamma(link="log"), method="REML")
summary(Gam2)
AIC(Gam2)
plot(Gam2)
simulateResiduals(Gam2, plot=T)
gam.check(Gam2)

Gam3<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4), data=I5gam,Gamma(link="log"), method="REML")
summary(Gam3)
AIC(Gam3)
plot(Gam3)
simulateResiduals(Gam3, plot=T)
gam.check(Gam3)


Gam4<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4)+s(sHCI_lag4, k=4), data=I5gam,Gamma(link="log"), method="REML")
summary(Gam4)
AIC(Gam4)
plot(Gam4)
simulateResiduals(Gam4, plot=T)
gam.check(Gam4)


Gam5<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4)+s(sSTI48_lag4, k=4), data=I5gam,Gamma(link="log"), method="REML")
summary(Gam5)
AIC(Gam5)
plot(Gam5)
simulateResiduals(Gam5, plot=T)
gam.check(Gam5)



## Tweedie distribution
Gam6<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4), data=I5gam,family=tw(link="log"), method="REML")
summary(Gam6)
AIC(Gam6)
plot(Gam6)
simulateResiduals(Gam6, plot=T)
gam.check(Gam6)

Gam7<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sHCI_lag4, k=4), data=I5gam,tw(link="log"), method="REML")
summary(Gam7)
AIC(Gam7)
plot(Gam7)
simulateResiduals(Gam7, plot=T)
gam.check(Gam7)

Gam8<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4), data=I5gam,tw(link="log"), method="REML")
summary(Gam8)
AIC(Gam8)
plot(Gam8)
simulateResiduals(Gam8, plot=T)
gam.check(Gam8)


Gam9<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4)+s(sHCI_lag4, k=4), data=I5gam,tw(link="log"), method="REML")
summary(Gam9)
AIC(Gam9)
plot(Gam9)
simulateResiduals(Gam9, plot=T)
gam.check(Gam9)


Gam10<- gam(Landkg~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4)+s(sSTI48_lag4, k=4), data=I5gam,tw(link="log"), method="REML")
summary(Gam10)
AIC(Gam10)
plot(Gam10)
simulateResiduals(Gam10, plot=T)
gam.check(Gam10)


