library(panelvar)
library(knitr)
paneleols <- pvarfeols(var_names, lags = laglen, data = panel, panel_identifier = c("Country", "yqtr"))

summary(paneleols)
colnames(paneleols$Set_Vars[,18:22])
femodel <- VAR(pan[,8:12], p = laglen, type = "const", season = NULL)

summary(femodel)

panirf <- oirf(paneleols, n.ahead = 4)
plot(panirf)
bootpan <- bootstrap_irf(model = paneleols,
              "OIRF",
              n.ahead = ahead,
              nof_Nstar_draws = 1000,
              confidence.band = .95)
fevdpan <- fevd_orthogonal(paneleols,
                n.ahead = 12)
stability(paneleols)
pvarhk()
class(paneleols)
bootstrap_irf.pvarfeols <- panelvar:::bootstrap_irf.pvargmm
names(paneleols)
plot(panirf)

lgdp