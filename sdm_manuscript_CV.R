
## Cross validation of sdmTMB models. Leave future out, forecasting 1 year ahead across three validations

library(sdmTMB)

library(future)
plan(multisession)


Depth_CV <- sdmTMB_cv(
  CperPot ~  s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
Depth_CV$fold_loglik
Depth_CV$sum_loglik
##RMSE
sqrt(mean((Depth_CV$data$CperPot- Depth_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(Depth_CV$data$CperPot- Depth_CV$data$cv_predicted))


save(Depth_CV, file="Depth_CV.RData")




Year_CV <- sdmTMB_cv(
  CperPot ~ 0 + s(Cyear,k=14),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
Year_CV$fold_loglik
Year_CV$sum_loglik
##RMSE
sqrt(mean((Year_CV$data$CperPot- Year_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(Year_CV$data$CperPot- Year_CV$data$cv_predicted))

save(Year_CV, file="Year_CV.RData")




HCI_CV <- sdmTMB_cv(
  CperPot ~  s(sHCI_lag4, k=4),
  data=Log100Ind,
  mesh=mesh_all,
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


Beuti_CV <- sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
Beuti_CV$fold_loglik
Beuti_CV$sum_loglik
##RMSE
sqrt(mean((Beuti_CV$data$CperPot- Beuti_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(Beuti_CV$data$CperPot- Beuti_CV$data$cv_predicted))
save(Beuti_CV, file="Beuti_CV.RData")


SST_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4),
  data=Log100Ind,
  mesh=mesh_all,
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
AIC(SST_CV)
save(SST_CV, file="SST_CV.RData")


Lusi_CV <- sdmTMB_cv(
  CperPot ~  s(sLusi_lag4, k=4),
  data=Log100Ind,
  mesh=mesh_all,
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

SBL_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4) + s(sLusi_lag4, k=4),
  data=Log100Ind,
  mesh=mesh_all,
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

SBLD_CV <- sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sLusi_lag4, k=4)+ s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBLD_CV$fold_loglik
SBLD_CV$sum_loglik
##RMSE
sqrt(mean((SBLD_CV$data$CperPot- SBLD_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBLD_CV$data$CperPot- SBLD_CV$data$cv_predicted))
save(SBLD_CV, file="SBLD_CV.RData")


SBHD_CV <- sdmTMB::sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+s(sHCI_lag4, k=4)+ s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBHD_CV$fold_loglik
SBHD_CV$sum_loglik
##RMSE
sqrt(mean((SBHD_CV$data$CperPot- SBHD_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBHD_CV$data$CperPot- SBHD_CV$data$cv_predicted))
save(SBHD_CV, file="SBHD_CV.RData")

## May not have converged
SBHLD_CV <- sdmTMB::sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4)+ s(sHCI_lag4, k=4) + s(sLusi_lag4, k=4) + s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
SBHLD_CV$fold_loglik
SBHLD_CV$sum_loglik
##RMSE
sqrt(mean((SBHLD_CV$data$CperPot- SBHLD_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(SBHLD_CV$data$CperPot- SBHLD_CV$data$cv_predicted))
save(SBHLD_CV, file="SBHLD_CV.RData")

SBD_CV <- sdmTMB::sdmTMB_cv(
  CperPot ~  s(sSST_lag4, k=4) + s(sbeuti_lag4, k=4) + s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
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


BD_CV <- sdmTMB::sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4)+s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
BD_CV$fold_loglik
BD_CV$sum_loglik
##RMSE
sqrt(mean((BD_CV$data$CperPot- BD_CV$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(BD_CV$data$CperPot- BD_CV$data$cv_predicted))
save(BD_CV, file="BD_CV.RData")



CV_SHBD <- sdmTMB::sdmTMB_cv(
  CperPot ~ s(sSST_lag4, k=4) + s(sHCI_lag4, k=4)+ s(sbeuti_lag4, k=4)+ s(ScaleDepth, k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast=1,
  lfo_validation=3
)

CV_test <- sdmTMB::sdmTMB_cv(
  CperPot ~  s(sbeuti_lag4, k=4)+s(sSST_lag4, k=4)+ s(sLusi_lag4, k=4)+s(ScaleDepth,k=4),
  data=Log100Ind,
  mesh=mesh_all,
  family=tweedie(link="log"),
  time="Cyear",
  spatial="on",
  spatiotemporal="AR1",
  lfo=T,
  lfo_forecast = 1,
  lfo_validations = 3
)
CV_test$fold_loglik
CV_test$sum_loglik
##RMSE
sqrt(mean((CV_test$data$CperPot- CV_test$data$cv_predicted)^2)) 
#> [1] 194.1784
# MAE across entire dataset:
mean(abs(CV_test$data$CperPot- CV_test$data$cv_predicted))
