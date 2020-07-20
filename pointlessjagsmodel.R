#Ireland
lm1 <- list(N = nrow(irevar$datamat), const =irevar$datamat[,"const"],
            lgdp = irevar$datamat[,"lgdp"], lhou = irevar$datamat[,"lhou"], lres = irevar$datamat[,"lres"], ldef = irevar$datamat[,"ldef"], int = irevar$datamat[,"int"],
            lgdp.l1 = irevar$datamat[,"lgdp.l1"], lhou.l1 =irevar$datamat[,"lhou.l1"], lres.l1  =irevar$datamat[,"lres.l1"],  ldef.l1 =irevar$datamat[,"ldef.l1"], int.l1  =irevar$datamat[,"int.l1"], 
            lgdp.l2 =irevar$datamat[,"lgdp.l2"], lhou.l2  =irevar$datamat[,"lhou.l2"], lres.l2=irevar$datamat[,"lres.l2"], ldef.l2 =irevar$datamat[,"ldef.l2"], int.l2 =irevar$datamat[,"int.l2"])

#Random effects
paneleols$Set_Vars <- paneleols$Set_Vars[order(paneleols$Set_Vars[,1]),]
lm2 <- list(N = paneleols$nof_groups, Ts = (paneleols$obs_per_group_avg-2),
            lgdp = paneleols$Set_Vars[,"lgdp"], lhou = paneleols$Set_Vars[,"lhou"], lres = paneleols$Set_Vars[,"lres"], ldef = paneleols$Set_Vars[,"ldef"], int = paneleols$Set_Vars[,"int"],
            lgdp.l1 = paneleols$Set_Vars[,"lag1_lgdp"], lhou.l1 =paneleols$Set_Vars[,"lag1_lhou"], lres.l1  =paneleols$Set_Vars[,"lag1_lres"],  ldef.l1 =paneleols$Set_Vars[,"lag1_ldef"], int.l1  =paneleols$Set_Vars[,"lag1_int"], 
            lgdp.l2 =paneleols$Set_Vars[,"lag2_lgdp"], lhou.l2  =paneleols$Set_Vars[,"lag2_lhou"], lres.l2=paneleols$Set_Vars[,"lag2_lres"], ldef.l2 =paneleols$Set_Vars[,"lag2_ldef"], int.l2 =paneleols$Set_Vars[,"lag2_int"])
library(tidyr)
namecol <- names(paneleols$Set_Vars)
for(i in 3:17) {
  lm2[[i]] = paneleols$Set_Vars[,c(namecol[i+15], "category", "period")]
  lm2[[i]] <- pivot_wider(lm2[[i]],names_from = category, values_from = namecol[(i+15)])
  lm2[[i]] <- dplyr::select(lm2[[i]], -period)
}

#Fixed Effects
lm3 <- list(N = nrow(paneleols$Set_Vars),
            lgdp = paneleols$Set_Vars[,"demeaned_lgdp"], lhou = paneleols$Set_Vars[,"demeaned_lhou"], lres = paneleols$Set_Vars[,"demeaned_lres"], ldef = paneleols$Set_Vars[,"demeaned_ldef"], int = paneleols$Set_Vars[,"demeaned_int"],
            lgdp.l1 = paneleols$Set_Vars[,"demeaned_lag1_lgdp"], lhou.l1 =paneleols$Set_Vars[,"demeaned_lag1_lhou"], lres.l1  =paneleols$Set_Vars[,"demeaned_lag1_lres"],  ldef.l1 =paneleols$Set_Vars[,"demeaned_lag1_ldef"], int.l1  =paneleols$Set_Vars[,"demeaned_lag1_int"], 
            lgdp.l2 =paneleols$Set_Vars[,"demeaned_lag2_lgdp"], lhou.l2  =paneleols$Set_Vars[,"demeaned_lag2_lhou"], lres.l2=paneleols$Set_Vars[,"demeaned_lag2_lres"], ldef.l2 =paneleols$Set_Vars[,"demeaned_lag2_ldef"], int.l2 =paneleols$Set_Vars[,"demeaned_lag2_int"])

library(coda)
library(rjags)

var.model <- jags.model("1.model", lm1,)
varre.model <- jags.model("2.model", lm2, n.adapt = 5000)
var.samps <- coda.samples(varre.model,c("alpha10","beta11","beta12","beta13","beta14","beta15","beta16","beta17","beta18","beta19","beta10",
                                      "alpha20","beta21","beta22","beta23","beta24","beta25","beta26","beta27","beta28","beta29","beta20",
                                      "alpha30","beta31","beta32","beta33","beta34","beta35","beta36","beta37","beta38","beta39","beta30",
                                      "alpha40","beta41","beta42","beta43","beta44","beta45","beta46","beta47","beta48","beta49","beta40",
                                      "alpha50","beta51","beta52","beta53","beta54","beta55","beta56","beta57","beta58","beta59","beta50",
                                      "tau1", "tau2", "tau3", "tau4", "tau5", "phi1", "phi2", "phi3", "phi4", "phi5"), n.iter = 5000)

varfe.model <- jags.model("3.model", lm3, n.adapt = 5000)
varfe.samps <- coda.samples(varfe.model,c("alpha10","beta11","beta12","beta13","beta14","beta15","beta16","beta17","beta18","beta19","beta10",
                                        "alpha20","beta21","beta22","beta23","beta24","beta25","beta26","beta27","beta28","beta29","beta20",
                                        "alpha30","beta31","beta32","beta33","beta34","beta35","beta36","beta37","beta38","beta39","beta30",
                                        "alpha40","beta41","beta42","beta43","beta44","beta45","beta46","beta47","beta48","beta49","beta40",
                                        "alpha50","beta51","beta52","beta53","beta54","beta55","beta56","beta57","beta58","beta59","beta50",
                                        "tau1", "tau2", "tau3", "tau4", "tau5", "phi1", "phi2", "phi3", "phi4", "phi5"), n.iter = 50000, thin = 50)

plot(varfe.samps)
save(varfe.samps, varfe.model, file = "pointlessfixedeffects.RData")