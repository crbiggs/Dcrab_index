## SDM_revised 
## predictions from sdm (DIS_range_pred16) are log(crab per pot per) per grid. Exp(est) gives crab per pot per grid area. 

library(sdmTMB)
library(tidyverse)
theme_set(theme_light())
library(DHARMa)
mesh_dis<- make_mesh(Log.dis, xy_cols=c("X","Y"), cutoff=5)
xtime  <-sdmTMBxtime <- c(2024:2028)


Dis_sdm <- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4) + s(DIS, k=4),
  data=Log.dis,
  extra_time= xtime,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F, 
)
Dis_sdm
save(Dis_sdm, file="Dis_sdm.Rdata")

DIS_sdm_parms <- tidy(Dis_sdm, "ran_pars", conf.int=T)
DIS_sdm_parms

sfit <- simulate(Dis_sdm, nsim=500, type="mle-mvn")
fitD <- dharma_residuals(sfit, Dis_sdm, return_DHARMa = T)
DHARMa::plotResiduals(fitD, form=factor(Log.dis$Cyear))
DHARMa::testUniformity(fitD)
DHARMa::testOutliers(fitD)
DHARMa::testDispersion(fitD)
DHARMa::testQuantiles(fitD)
fitD
save(fitD, file="fitD.RData")

Dis_rangeG16  <- expand_grid(G16YDI, DIS = c(0,15,30,45,60,75,90,105,120, 135, 150))

DIS_range_pred16 <- predict(Dis_sdm, newdata=Dis_rangeG16, return_tmb_object = T)

save(DIS_range_pred16, file="DIS_range_pred16.Rdata")

DIS_range_index16 <- get_index(DIS_range_pred16, bias_correct = T, area=16)
DIS_range_cog16 <- get_cog(DIS_range_pred16, bias_correct = F, area=16, format="wide")


ggplot() + geom_line(data=DIS_range_index16, aes(Cyear,est*6.1, color="Model"))+ geom_line(data=I5[44:62,], aes(Cyear,Landkg, color="Landings"))+
  geom_ribbon(data=DIS_range_index16, aes(Cyear, est*6.1, ymax=upr*6.1, ymin=lwr*6.1), alpha=0.3)+
  labs(x="Crab Year", y="Landings (kg)", color="") + scale_x_continuous(breaks=c(2010:2028))+
  scale_y_continuous(
    sec.axis = sec_axis(~ . / 6, name = "Number of Crab") # Transformation
  ) 

DIS_range_pred16$data |> filter(Cyear <2024) |> ggplot()+geom_raster(aes(x=X*1000, y=Y*1000, fill=exp(est)))+
  scale_fill_viridis_c(limits = c(0, 15), oob = scales::squish)+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("Spatiotemporal Model")+labs(fill="Crab per pot")

DIS_range_pred16$data |> filter(Cyear <2024) |> ggplot()+geom_raster(aes(x=X*1000, y=Y*1000, fill=est_non_rf))+
  scale_fill_viridis_c()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("Spatiotemporal Model")+labs(fill="fixed effects")

DIS_range_pred16$data |> filter(Cyear <2024) |> ggplot()+geom_raster(aes(x=X*1000, y=Y*1000, fill=est_rf))+
  scale_fill_viridis_c()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("Spatiotemporal Model")+labs(fill="random effects")


OS_fig <- ggplot()+geom_raster(data=DIS_range_pred16$data, aes(x=X*1000, y=Y*1000, fill=omega_s))+
  scale_fill_gradient2() +
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("")+labs(fill="Spatial \nrandom \neffects")
OS_fig

DIS_range_pred16$data |> filter(Cyear <2024) |> ggplot()+geom_raster( aes(x=X*1000, y=Y*1000, fill=epsilon_st))+
  scale_fill_gradient2()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  ggtitle("SBLD Model")+labs(fill="Random effects")


library(ggrepel)

COG <- ggplot(DIS_range_cog16[1:14,])+geom_point( aes(est_x*1000, est_y*1000, ))+
  geom_text_repel( aes(est_x*1000, est_y*1000,label=Cyear), max.overlaps=19)+labs(x="", y="")+guides(x=guide_axis(angle=90))+
  geom_linerange(aes(y=est_y*1000, xmin = lwr_x*1000, xmax = upr_x*1000,color=Cyear)) +
  geom_linerange(aes(x=est_x*1000,ymin = lwr_y*1000, ymax = upr_y*1000,color=Cyear)) +scale_colour_gradient()+
  geom_sf(data=WA_coast_proj)+coord_sf(ylim=c(5150*1000,5188*1000), xlim=c(370*1000, 400*1000))+ggtitle("Center of Gravity")


COG


## Conditional effects

Snd <- data.frame(
  sSST_lag4=seq(min(Log.dis$sSST_lag4), max(Log.dis$sSST_lag4), length.out=100),
  Cyear=2023, 
 DIS= mean(Log.dis$DIS),
  sbeuti_lag4=mean(Log.dis$sbeuti_lag4),
  sLusi_lag4 = mean(Log.dis$sLusi_lag4))

MargPS <- predict(Dis_sdm, newdata=Snd, se_fit=T, re_form=NA) 

S_CE <- ggplot()+
  geom_line(data=MargPS, aes(sSST_lag4, exp(est)))+ 
  geom_ribbon(data=MargPS, aes(sSST_lag4, exp(est), ymin=exp(est-1.96*est_se), 
                                        ymax=exp(est+1.96*est_se)),alpha=0.3) + 
                scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Scaled SST Lagged 4 years", y= "Crab per Pot")+
  geom_rug(data=Log.dis, aes(sSST_lag4))


Dnd <- data.frame(
  DIS=seq(min(Log.dis$DIS), max(Log.dis$DIS), length.out=100),
  Cyear=2023, 
  sSST_lag4= mean(Log.dis$sSST_lag4),
  sbeuti_lag4=mean(Log.dis$sbeuti_lag4),
  sLusi_lag4 = mean(Log.dis$sLusi_lag4))

MargPD <- predict(Dis_sdm, newdata=Dnd, se_fit=T, re_form=NA) 

D_CE <- ggplot()+
  geom_line(data=MargPD, aes(DIS, exp(est)))+ 
  geom_ribbon(data=MargPD, aes(DIS, exp(est), ymin=exp(est-1.96*est_se), 
                               ymax=exp(est+1.96*est_se)),alpha=0.3) + 
  scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Day in Season", y= "Crab per Pot")+
  geom_rug(data=Log.dis, aes(DIS))



Bnd <- data.frame(
  sbeuti_lag4=seq(min(Log.dis$sbeuti_lag4), max(Log.dis$sbeuti_lag4), length.out=100),
  Cyear=2023, 
  sSST_lag4= mean(Log.dis$sSST_lag4),
  DIS=mean(Log.dis$DIS),
  sLusi_lag4 = mean(Log.dis$sLusi_lag4))

MargPB <- predict(Dis_sdm, newdata=Bnd, se_fit=T, re_form=NA) 

B_CE <-ggplot()+
  geom_line(data=MargPB, aes(sbeuti_lag4, exp(est)))+ 
  geom_ribbon(data=MargPB, aes(sbeuti_lag4, exp(est), ymin=exp(est-1.96*est_se), 
                               ymax=exp(est+1.96*est_se)),alpha=0.3) + 
  scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Scaled Beuti Lagged 4 Years", y= "Crab per Pot")+
  geom_rug(data=Log.dis, aes(sbeuti_lag4))


Lnd <- data.frame(
  sLusi_lag4=seq(min(Log.dis$sLusi_lag4), max(Log.dis$sLusi_lag4), length.out=100),
  Cyear=2023, 
  sSST_lag4= mean(Log.dis$sSST_lag4),
  DIS=mean(Log.dis$DIS),
  sbeuti_lag4 = mean(Log.dis$sbeuti_lag4))

MargPL <- predict(Dis_sdm, newdata=Lnd, se_fit=T, re_form=NA) 

L_CE <-ggplot()+
  geom_line(data=MargPL, aes(sLusi_lag4, exp(est)))+ 
  geom_ribbon(data=MargPL, aes(sLusi_lag4, exp(est), ymin=exp(est-1.96*est_se), 
                               ymax=exp(est+1.96*est_se)),alpha=0.3) + 
  scale_x_continuous() +
  coord_cartesian(expand=F) + labs(x="Scaled Lusi Lagged 4 Years", y= "Crab per Pot")+
  geom_rug(data=Log.dis, aes(sLusi_lag4))

library(gridExtra)
CEplots <- grid.arrange(D_CE, S_CE,B_CE, L_CE,  ncol=2, nrow=2)
ggsave(file="figure 5_sdmCE.jpg",
       plot=CEplots,
       dpi=500)



Sdm_comp <- DIS_range_index16[1:16,]
Sdm_comp$Landkg <- I5$Landkg[44:59]

sdmIL <- cor.test(test.out_sdm$est, test.out_sdm$Landkg)
sdmIL$conf.int
sdmIL$statistic
sdmIL$estimate


test.out_sdm <-Sdm_comp[,c(2,9)]
library(boot)

se <- corelrho(test.out_sdm)
bootar2.delta <- boot( test.out_sdm, corelrho, R=1000 )
mean(bootar2.delta$t)

err_sdm <- test.out_sdm$est/164 - test.out_sdm$Landkg/1000
errdf <- as.data.frame(err_sdm)
errdf$abs <- abs(errdf$err)
mean(errdf$abs)
errdf$sq <- errdf$err^2
sqrt(mean(errdf$sq))




## mean absolute relative error
errdf$errYi <- errdf$abs/(test.out_sdm$Landkg/1000)
mean(errdf$errYi)


#Getting avg values or location/Cyear
Avg_Dis_pred16 <- DIS_range_pred16_Data |> 
  group_by(Cyear, X, Y) |> reframe(AvgEst = mean(est), SDest = sd(est), AvgEp = mean(epsilon_st), AvgO = mean(omega_s))

sd_dis_pred16 <- DIS_range_pred16_Data |> 
  group_by(Cyear, X, Y) |> reframe(SDest = sd(est), SdEp = sd(epsilon_st), SdO = sd(omega_s))



Est_fig <- Avg_Dis_pred16 |> filter(Cyear <2024) |> ggplot()+geom_raster(aes(x=X*1000, y=Y*1000, fill=exp(AvgEst)))+
  scale_fill_viridis_c()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  labs(fill= expression("Avg. Crab ("~pot^-1 ~km^-2~")"))
Est_fig

ggsave(filename="Est_fig.jpg", 
       plot=Est_fig,
       dpi=500)
EP_fig <- Avg_Dis_pred16|> filter(Cyear <2024) |> ggplot()+geom_raster(aes(x=X*1000, y=Y*1000, fill=AvgEp))+
  scale_fill_gradient2()+facet_wrap(~Cyear, nrow=2)+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  labs(fill=("Spatiotemporal \nrandom effects \nlog(Crab per pot)"))
EP_fig

ggsave(filename="EP_fig.jpg", 
       plot=EP_fig,
       dpi=500)
Avg_Dis_pred16 |> filter(Cyear <2024) |> ggplot()+geom_raster(aes(x=X*1000, y=Y*1000, fill=AvgO))+
  scale_fill_gradient2()+
  geom_sf(data=WA_coast_proj)+
  theme_light()+ guides(x=guide_axis(angle=90))+labs(x="", y="")+
  labs(fill="Crab per pot")


