### SDM Manuscript SDM code 1_14_2026 

library(tidyverse)
library(sdmTMB)
load("Log100Ind.RData")
load("G16YDI.RData")
load("WA_coast_proj.RData")
load("I5.RData")
theme_set(theme_light())

## Running SDM model 
# Create  mesh/vertices 
mesh_all<- make_mesh(Log100Ind, xy_cols=c("X","Y"), cutoff=5)
xtime  <-sdmTMBxtime <- c(2024:2028)
  
## Running sdm with scaled predictors, except for Depth, so that the prediction grid is still appropriate
## since there are a larger range of depths on the prediction grid. 
SBLDdpth <- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4)+ s(DepthM1,k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)

SBLDdpth
SBLD16_parms <- tidy(SBLDdpth, "ran_pars", conf.int=T)


## Good residual tests

sfit <- simulate(SBLDdpth, nsim=500, type="mle-mvn")
fitD <- dharma_residuals(sfit, SBLDdpth, return_DHARMa = T)
DHARMa::plotResiduals(fitD, form=factor(Log100Ind$Cyear))
DHARMa::testUniformity(fitD)
DHARMa::testOutliers(fitD)
DHARMa::testDispersion(fitD)
DHARMa::testQuantiles(fitD)




## Make prediction grid for visualizing sdm output 
## G16YDI-  16 km^2 cells , points are 4 km apart. 

SBLD_pred16 <- predict(SBLDdpth, newdata=G16YDI, return_tmb_object = T)
SBLD_index16 <- get_index(SBLD_pred16, bias_correct = T, area=16)
SBLD_cog16 <- get_cog(SBLD_pred16, bias_correct = T, area=16, format="wide")


save(SBLDdpth, file="SBLDdpth.RData")
save(SBLD_pred16, file="SBLD_pred16.RData")
save(fitD, file="fitD.Rdata")
save(SBLD_index16, file="SBLD_index16.RData")



library(ggrepel)

ggplot(SBLD_cog16[1:14,])+geom_point( aes(est_x*1000, est_y*1000, ))+
  geom_text_repel( aes(est_x*1000, est_y*1000,label=Cyear), max.overlaps=19)+labs(x="", y="")+guides(x=guide_axis(angle=90))+
  geom_linerange(aes(y=est_y*1000, xmin = lwr_x*1000, xmax = upr_x*1000,color=Cyear)) +
  geom_linerange(aes(x=est_x*1000,ymin = lwr_y*1000, ymax = upr_y*1000,color=Cyear)) +scale_colour_gradient()+
  geom_sf(data=WA_coast_proj)+coord_sf(ylim=c(5150*1000,5188*1000), xlim=c(370*1000, 400*1000))+ggtitle("Center of Gravity")


ggplot() + geom_line(data=SBLD_index16, aes(Cyear,est*65, color="Model"))+ geom_line(data=I5[44:62,], aes(Cyear,Landkg, color="Landings"))+
  geom_ribbon(data=SBLD_index16, aes(Cyear, est*65, ymax=upr*65, ymin=lwr*65), alpha=0.3)+
  labs(x="Crab Year", y="Landings (kg)", color="") + scale_x_continuous(breaks=c(2010:2028))+
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 65, name = "Number of Crab") # Transformation
  ) 

ggplot()+geom_raster(data=SBLD_pred16$data, aes(x=X*1000, y=Y*1000, fill=exp(est)))+
  scale_fill_viridis_c()+facet_wrap(~Cyear, nrow=3)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("SBLD Model")+labs(fill="Crab per pot")

ggplot()+geom_raster(data=SBLD_pred16$data, aes(x=X*1000, y=Y*1000, fill=est_non_rf))+
  scale_fill_viridis_c()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("SBLD Model") 

ggplot()+geom_raster(data=SBLD_pred16$data, aes(x=X*1000, y=Y*1000, fill=exp(est_rf)))+
  scale_fill_viridis_c()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("SBLD Model")


ggplot()+geom_raster(data=SBLD_pred16$data, aes(x=X*1000, y=Y*1000, fill=epsilon_st))+
  scale_fill_gradient2()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("SBLD Model")




save(SBLDdpth, file="SBLDdpth.RData")
save(SBLD_index, file="SBLD_index16.RData")
save(SBLD_pred4, file="SBLD_pred16.RData")
save(SBLD4_cog, file="SBLD_cog16.RData")


#Conditional effects plots 
Dnd <- data.frame(
  DepthM1=seq(min(Log100Ind$DepthM1), max(Log100Ind$DepthM1), length.out=100),
  Cyear=2023, 
  sSST_lag4 = mean(Log100Ind$sHCI_lag4),
  sbeuti_lag4=mean(Log100Ind$sbeuti_lag4),
  sLusi_lag4 = mean(Log100Ind$sLusi_lag4))
MargPD <- predict(SBLDdpth, newdata=Dnd, se_fit=T, re_form=NA) 

ggplot(MargPD, aes(DepthM1, exp(est),
                   ymin=exp(est-1.96*est_se), 
                   ymax=exp(est+1.96*est_se)))+
  geom_line()+ geom_ribbon(alpha=0.3) + scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Depth", y= "Crab per Pot")


Snd <- data.frame(
  sSST_lag4=seq(min(Log100Ind$sSST_lag4), max(Log100Ind$sSST_lag4), length.out=100),
  Cyear=2023, 
  DepthM1= mean(Log100Ind$DepthM1),
  sbeuti_lag4=mean(Log100Ind$sbeuti_lag4),
  sLusi_lag4 = mean(Log100Ind$sLusi_lag4))

MargPS <- predict(SBLDdpth, newdata=Snd, se_fit=T, re_form=NA) 

ggplot(MargPS, aes(sSST_lag4, exp(est),
                   ymin=exp(est-1.96*est_se), 
                   ymax=exp(est+1.96*est_se)))+
  geom_line()+ geom_ribbon(alpha=0.3) + scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Scaled SST Lagged 4 years", y= "Crab per Pot")


Bnd <- data.frame(
  sbeuti_lag4=seq(min(Log100Ind$sbeuti_lag4), max(Log100Ind$sbeuti_lag4), length.out=100),
  Cyear=2023, 
  DepthM1= mean(Log100Ind$DepthM1),
  sSST_lag4=mean(Log100Ind$sHCI_lag4),
  sLusi_lag4 = mean(Log100Ind$sLusi_lag4))

MargPB <- predict(SBLDdpth, newdata=Bnd, se_fit=T, re_form=NA) 

ggplot(MargPB, aes(sbeuti_lag4, exp(est),
                   ymin=exp(est-1.96*est_se), 
                   ymax=exp(est+1.96*est_se)))+
  geom_line()+ geom_ribbon(alpha=0.3) + scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Scaled Betui Lagged 4 years", y= "Crab per Pot")

Lnd <- data.frame(
  sLusi_lag4=seq(min(Log100Ind$sLusi_lag4), max(Log100Ind$sLusi_lag4), length.out=100),
  Cyear=2023, 
  DepthM1= mean(Log100Ind$DepthM1),
  sSST_lag4=mean(Log100Ind$sHCI_lag4),
  sbeuti_lag4 = mean(Log100Ind$sLusi_lag4))

MargPL <- predict(SBLDdpth, newdata=Lnd, se_fit=T, re_form=NA) 

ggplot(MargPL, aes(sLusi_lag4, exp(est),
                   ymin=exp(est-1.96*est_se), 
                   ymax=exp(est+1.96*est_se)))+
  geom_line()+ geom_ribbon(alpha=0.3) + scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Scaled Lusi Lagged 4 years", y= "Crab per Pot")





## Other models configurations
SBL <- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


SBLHD <- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4) + s(sHCI_lag4, k=4) +
    s(DepthM1,k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


SBD <- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4) +
    s(DepthM1,k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


Beuti <- sdmTMB(
  CperPot ~   s(sbeuti_lag4, k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


BD <- sdmTMB(
  CperPot ~ s(sbeuti_lag4, k=4) +
    s(DepthM1,k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)

Sst<- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


Hci <- sdmTMB(
  CperPot ~  s(sHCI_lag4, k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)

Dpth <- sdmTMB(
  CperPot ~ 
    s(DepthM1,k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


Lusi <- sdmTMB(
  CperPot ~  s(sLusi_lag4, k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)


Cyear <- sdmTMB(
  CperPot ~  s(Cyear, k=4),
  data=Log100Ind,
  extra_time = xtime,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)