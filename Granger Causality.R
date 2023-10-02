df=EuStockMarkets
ggplot2::autoplot(df)
forecast:Acf(df)
ggplot2::autoplot(diff(df))
forecast::Acf(diff(df))
# loop over columns and perform PP test
for (i in 1:ncol(df)) {
  pp_result <- tseries::pp.test(diff(df[, i]))
  print(pp_result$p.value)
}
lagselect <- vars::VARselect(diff(df), lag.max = 10, type = "const")
lagselect$selection
Model1 <- vars::VAR(diff(df), p = 9, type = "const",season = NULL)
summary(Model1)
Serial1 <- vars::serial.test(Model1, lags.pt = 10, type = "PT.asymptotic")
Serial1
Arch1 <- vars::arch.test(Model1, lags.multi = 10, multivariate.only = TRUE)
Arch1
Norm1 <- vars::normality.test(Model1, multivariate.only = TRUE)
Norm1
Stability1 <- vars::stability(Model1, type = "OLS-CUSUM")
plot(Stability1)
GrangerDAX<- vars::causality(Model1, cause = "DAX")
GrangerDAX
GrangerSMI <- vars::causality(Model1, cause = "SMI")
GrangerSMI
GrangerCAC <- vars::causality(Model1, cause = "CAC")
GrangerCAC
GrangerFTSE <- vars::causality(Model1, cause = "FTSE")
GrangerFTSE


# Model with lag 1
Model2 <- vars::VAR(diff(df), p = 1, type = "const",season = NULL)
summary(Model1)
Serial2 <- vars::serial.test(Model2, lags.pt = 10, type = "PT.asymptotic")
Serial2
Arch2 <- vars::arch.test(Model2, lags.multi = 10, multivariate.only = TRUE)
Arch2
Norm2 <- vars::normality.test(Model2, multivariate.only = TRUE)
Norm2
Stability2 <- vars::stability(Model2, type = "OLS-CUSUM")
plot(Stability2)
GrangerDAX<- vars::causality(Model2, cause = "DAX")
GrangerDAX
GrangerSMI <- vars::causality(Model2, cause = "SMI")
GrangerSMI
GrangerCAC <- vars::causality(Model2, cause = "CAC")
GrangerCAC
GrangerFTSE <- vars::causality(Model2, cause = "FTSE")
GrangerFTSE
