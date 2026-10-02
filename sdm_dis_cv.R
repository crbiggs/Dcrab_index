## New CV analysis for revision : 

## Include DIS in each model- same as GAM- try each factor individually, 
## then look at combinations. 


library(sdmTMB)

library(future)
plan(multisession, workers=3)

library(tidyverse)

save(mesh_dis, file="mesh_dis.Rdata")
save(Log.dis, file="Log.dis.Rdata")
mesh_dis<- make_mesh(Log.dis, xy_cols=c("X","Y"), cutoff=5)

## current working model as of 8/28/26
Dis_sdm <- sdmTMB(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4) + s(DIS, k=4),
  data=Log.dis,
  extra_time= xtime,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  silent=F)



DIS_CV <- sdmTMB_cv(
  CperPot ~  s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
DIS_CV$fold_loglik
DIS_CV$sum_loglik
##RMSE
sqrt(mean((DIS_CV$data$CperPot- DIS_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(DIS_CV$data$CperPot- DIS_CV$data$cv_predicted))

save(DIS_CV, file="DIS_CV.RData")




SST_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+  s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SST_CV$fold_loglik
SST_CV$sum_loglik
##RMSE
sqrt(mean((SST_CV$data$CperPot- SST_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SST_CV$data$CperPot- SST_CV$data$cv_predicted))

save(SST_CV, file="SST_CV.RData")



beuti_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4)+  s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
beuti_CV$fold_loglik
beuti_CV$sum_loglik
##RMSE
sqrt(mean((beuti_CV$data$CperPot- beuti_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(beuti_CV$data$CperPot- beuti_CV$data$cv_predicted))

save(beuti_CV, file="beuti_CV.RData")



Lusi_CV <- sdmTMB_cv(
  CperPot ~  s(sLusi_lag4, k=4)+  s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
Lusi_CV$fold_loglik
Lusi_CV$sum_loglik
##RMSE
sqrt(mean((Lusi_CV$data$CperPot- Lusi_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(Lusi_CV$data$CperPot- Lusi_CV$data$cv_predicted))

save(Lusi_CV, file="Lusi_CV.RData")




STI48_CV <- sdmTMB_cv(
  CperPot ~  s(sSTI48_lag4, k=4)+  s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
STI48_CV$fold_loglik
STI48_CV$sum_loglik
##RMSE
sqrt(mean((STI48_CV$data$CperPot- STI48_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(STI48_CV$data$CperPot- STI48_CV$data$cv_predicted))

save(STI48_CV, file="STI48_CV.RData")




HCI_CV <- sdmTMB_cv(
  CperPot ~  s(sHCI_lag4, k=4)+  s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
HCI_CV$fold_loglik
HCI_CV$sum_loglik
##RMSE
sqrt(mean((HCI_CV$data$CperPot- HCI_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(HCI_CV$data$CperPot- HCI_CV$data$cv_predicted))

save(HCI_CV, file="HCI_CV.RData")


SB_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+ s(sbeuti_lag4, k=4) + s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SB_CV$fold_loglik
SB_CV$sum_loglik
##RMSE
sqrt(mean((SB_CV$data$CperPot- SB_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SB_CV$data$CperPot- SB_CV$data$cv_predicted))
save(SB_CV, file="SB_CV.RData")

SBS_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+ s(sbeuti_lag4, k=4) + s(sSTI48_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBS_CV$fold_loglik
SBS_CV$sum_loglik
##RMSE
sqrt(mean((SBS_CV$data$CperPot- SBS_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBS_CV$data$CperPot- SBS_CV$data$cv_predicted))
save(SBS_CV, file="SBS_CV.RData")


SBL_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+ s(sbeuti_lag4, k=4) + s(sLusi_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBL_CV$fold_loglik
SBL_CV$sum_loglik
##RMSE
sqrt(mean((SBL_CV$data$CperPot- SBL_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBL_CV$data$CperPot- SBL_CV$data$cv_predicted))
save(SBL_CV, file="SBL_CV.RData")


SBH_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+ s(sbeuti_lag4, k=4) + s(sHCI_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBH_CV$fold_loglik
SBH_CV$sum_loglik
##RMSE
sqrt(mean((SBH_CV$data$CperPot- SBH_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBH_CV$data$CperPot- SBH_CV$data$cv_predicted))
save(SBH_CV, file="SBH_CV.RData")



DO_CV <- sdmTMB_cv(
  CperPot ~  s(sDO50_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
DO_CV$fold_loglik
DO_CV$sum_loglik
##RMSE
sqrt(mean((DO_CV$data$CperPot- DO_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(DO_CV$data$CperPot- DO_CV$data$cv_predicted))
save(DO_CV, file="DO_CV.RData")



SS_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+ s(sSTI48_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SS_CV$fold_loglik
SS_CV$sum_loglik
##RMSE
sqrt(mean((SS_CV$data$CperPot- SS_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SS_CV$data$CperPot- SS_CV$data$cv_predicted))
save(SS_CV, file="SS_CV.RData") 

SBD_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4)+ s(sbeuti_lag4, k=4)+ s(sDO50_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBD_CV$fold_loglik
SBD_CV$sum_loglik
##RMSE
sqrt(mean((SBD_CV$data$CperPot- SBD_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBD_CV$data$CperPot- SBD_CV$data$cv_predicted))
save(SBD_CV, file="SBD_CV.RData") 


## need to fill in missing values for Wind 
Log.dis1 <- Log.dis |> mutate(sWind_lag4 = replace_na(sWind_lag4, 0.2334736))
Log.dis <- Log.dis1

Wind_CV <- sdmTMB_cv(
  CperPot ~  s(sWind_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
Wind_CV$fold_loglik
Wind_CV$sum_loglik
##RMSE
sqrt(mean((Wind_CV$data$CperPot- Wind_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(Wind_CV$data$CperPot- Wind_CV$data$cv_predicted))
save(Wind_CV, file="Wind_CV.RData")


SBW_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4) + s(sWind_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBW_CV$fold_loglik
SBW_CV$sum_loglik
##RMSE
sqrt(mean((SBW_CV$data$CperPot- SBW_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBW_CV$data$CperPot- SBW_CV$data$cv_predicted))
save(SBW_CV, file="SBW_CV.RData")



BL_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4) + s(sLusi_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
BL_CV$fold_loglik
BL_CV$sum_loglik
##RMSE
sqrt(mean((BL_CV$data$CperPot- BL_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(BL_CV$data$CperPot- BL_CV$data$cv_predicted))
save(BL_CV, file="BL_CV.RData")


BS_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4) + s(sSTI48_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
BS_CV$fold_loglik
BS_CV$sum_loglik
##RMSE
sqrt(mean((BS_CV$data$CperPot- BS_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(BS_CV$data$CperPot- BS_CV$data$cv_predicted))
save(BS_CV, file="BS_CV.RData")


SL_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sLusi_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SL_CV$fold_loglik
SL_CV$sum_loglik
##RMSE
sqrt(mean((SL_CV$data$CperPot- SL_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SL_CV$data$CperPot- SL_CV$data$cv_predicted))
save(SL_CV, file="SL_CV.RData")

BH_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4) + s(sHCI_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
BH_CV$fold_loglik
BH_CV$sum_loglik
##RMSE
sqrt(mean((BH_CV$data$CperPot- BH_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(BH_CV$data$CperPot- BH_CV$data$cv_predicted))
save(BH_CV, file="BH_CV.RData")



SH_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sHCI_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SH_CV$fold_loglik
SH_CV$sum_loglik
##RMSE
sqrt(mean((SH_CV$data$CperPot- SH_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SH_CV$data$CperPot- SH_CV$data$cv_predicted))
save(SH_CV, file="SH_CV.RData")


BW_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4) + s(sWind_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
BW_CV$fold_loglik
BW_CV$sum_loglik
##RMSE
sqrt(mean((BW_CV$data$CperPot- BW_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(BW_CV$data$CperPot- BW_CV$data$cv_predicted))
save(BW_CV, file="BW_CV.RData")

SW_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sWind_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SW_CV$fold_loglik
SW_CV$sum_loglik
##RMSE
sqrt(mean((SW_CV$data$CperPot- SW_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SW_CV$data$CperPot- SW_CV$data$cv_predicted))
save(SW_CV, file="SW_CV.RData")


BHS_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4) + s(sHCI_lag4, k=4) + s(sSTI48_lag4, k=4) +s(DIS, k=4),
  data=Log.dis,
  mesh=mesh_dis,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
BHS_CV$fold_loglik
BHS_CV$sum_loglik
##RMSE
sqrt(mean((BHS_CV$data$CperPot- BHS_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(BHS_CV$data$CperPot- BHS_CV$data$cv_predicted))
save(BHS_CV, file="BHS_CV.RData")
