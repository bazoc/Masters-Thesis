#Using the BVAR package
for(i in 1:length(countries)) {
  model2[[countries[i]]] <- bvar(data = data[[countries[i]]], lags = laglen, bv_irf = c(identification = FALSE))
}
names(model2$Austria$beta)
model2$Austria$variables

summary(model2$Austria)
apply(model2$Austria$beta, FUN = mean, MARGIN = 2)
plot(model2$Austria)
plot(model2$Austria, type = "dens", vars_response = "lgdp", vars_impulse = "lgdp-lag1")

#Make it a coda object
austria_mcmc <- as.mcmc(model2$Austria)

#Multiple chains
n_cores <- 2
cl <- makeCluster(n_cores)
bvarire <- bvar(data = data$Ireland, lags = laglen, n_draw = 60000, n_burn = 10000, n_thin = 20, bv_irf = c(identification = TRUE))

#Run the model
#for(i in 1:length(countries)) {
#  model2[[countries[i]]] <- par_bvar(cl = cl, data = data[[countries[i]]], lags = k, bv_irf = c(identification = FALSE))
#}
yup <- par_bvar(cl = cl, data = data$Ireland, lags = laglen, n_draw = 60000, n_burn = 10000, n_thin = 20)
stopCluster(cl)

#Type of impulse response
irfparams <- bv_irf(horizon = 40,
                    fevd = F,
                    identification = T)

birfire <- irf(bvarire, irfparams, conf_bands = c(.05, .95), vars_impulse = "int", vars_response = "lhou, lgdo")
plot(birfire)
subset.ts(ireland,
       year <= 2007)
window(ireland, start = 2004, end = 2007)
#Make it a coda object
austria_mcmc <- as.mcmc(yup)
gelman.plot(austria_mcmc)
effectiveSize(austria_mcmc)
acf(austria_mcmc$x)