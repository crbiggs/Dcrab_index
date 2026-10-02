



suppressMessages(library(mgcv))


preds <- preds <- c("sSST_lag4","sSTI48_lag4","sbeuti_lag4","sLusi_lag4",
                    "sHCI_lag4","sDO150_lag4","sDO50_lag4","sWind_lag4")
df <- I5gamcv

n <- nrow(df)
cat("Using complete-case dataset: n =", n, "\n\n")
cat("Landkg range:", range(df$Landkg), " (all positive?", all(df$Landkg > 0), ")\n\n")

fit_full <- function(vars, k=4) {
  smooth_terms <- paste0("s(", vars, ", k=", k, ")")
  form <- as.formula(paste("Landkg ~", paste(smooth_terms, collapse = " + ")))
  gam(form, data = df, family = Gamma(link = "log"), method = "REML")
}

loocv_rmse <- function(vars, k = 4) {
  smooth_terms <- paste0("s(", vars, ", k=", k, ")")
  form <- as.formula(paste("Landkg ~", paste(smooth_terms, collapse = " + ")))
  preds_out <- rep(NA_real_, n)
  for (i in 1:n) {
    train <- df[-i, ]
    test  <- df[i, , drop = FALSE]
    fit <- tryCatch(gam(form, data = train, family = Gamma(link = "log"), method = "REML"),
                     error = function(e) NULL)
    if (is.null(fit)) return(NA_real_)
    preds_out[i] <- tryCatch(predict(fit, newdata = test, type = "response"),
                              error = function(e) NA_real_)
  }
  sqrt(mean((df$Landkg - preds_out)^2, na.rm = TRUE))
}

results <- data.frame(vars = character(), n_terms = integer(),
                       AIC = numeric(), LOOCV_RMSE = numeric(),
                       adjR2 = numeric(), dev_expl = numeric(),
                       stringsAsFactors = FALSE)

for (size in 1:4) {
  combos <- combn(preds, size, simplify = FALSE)
  for (cb in combos) {
    full_fit <- tryCatch(fit_full(cb, k=4), error=function(e) NULL)
    aic_val <- if (!is.null(full_fit)) AIC(full_fit) else NA_real_
    r2_val  <- if (!is.null(full_fit)) summary(full_fit)$r.sq else NA_real_
    dev_val <- if (!is.null(full_fit)) summary(full_fit)$dev.expl else NA_real_
    rmse_val <- loocv_rmse(cb, k=4)
    results <- rbind(results, data.frame(
      vars = paste(cb, collapse = " + "),
      n_terms = size, AIC = aic_val, LOOCV_RMSE = rmse_val,
      adjR2 = r2_val, dev_expl = dev_val
    ))
  }
  cat("Done size", size, "-", length(combos), "models\n")
}

# Null model
null_fit <- gam(Landkg ~ 1, data = df, family = Gamma(link="log"), method="REML")
null_aic <- AIC(null_fit)
preds_out <- rep(NA_real_, n)
for (i in 1:n) {
  train <- df[-i,]
  m <- gam(Landkg ~ 1, data=train, family=Gamma(link="log"), method="REML")
  preds_out[i] <- predict(m, newdata=df[i,,drop=FALSE], type="response")
}
null_rmse <- sqrt(mean((df$Landkg - preds_out)^2))
results <- rbind(results, data.frame(vars="(intercept only)", n_terms=0,
                                      AIC=null_aic, LOOCV_RMSE=null_rmse, adjR2=0, dev_expl=0))

results_gamma <- results[order(results$LOOCV_RMSE), ]


cat("\nNull model: AIC =", round(null_aic,1), " LOOCV RMSE =", round(null_rmse,1), "\n\n")
cat("=== Top 20 models by LOOCV RMSE (Gamma, log link) ===\n")
print(head(results, 20), row.names = FALSE, digits=6)

cat("\n=== Top 10 models by AIC ===\n")
res_aic <- results[order(results$AIC), ]
print(head(res_aic, 10), row.names = FALSE, digits=6)

cat("\nTotal models evaluated:", nrow(results), "\n")
cat("Spearman cor(AIC, LOOCV_RMSE):",
    round(cor(results$AIC, results$LOOCV_RMSE, method="spearman", use="complete.obs"), 3), "\n")
