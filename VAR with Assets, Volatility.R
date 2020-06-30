#VAR with CB balance, volatility, real GDP, GDP Deflator, Real House Prices, Residential Investment
rm(list = ls())
setwd("~/Thesis")
Cleaned.Data <- read.csv("Data/CleanedData.csv")
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(BVAR)
library(plm)
library(panelvar)
library(coda)
library(parallel)
#Year we are using
sta <- 2008
fin <- 2019 

#Set whether VAR is in log or levels terms
ln = T

#House prices go up to 2020, unconventional monetary policy starts at 2008
Cleaned.Data <- subset(Cleaned.Data,
                       year >= sta & year <= fin)
#No residential investment for belgium

miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data$Real.House.Prices))
#No Nominal house prices forlithuania latvia
#No residential investment for belgium
Cleaned.Data <- subset(Cleaned.Data,
                       Country != "Lithuania" & Country != "Latvia" & Country != "Belgium" )
miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data))
#Now no missing observations lets go


#Make individual variable with columns for every country 
gdp <- dplyr::select(Cleaned.Data,
              Country,
              yqtr,
              `Real.GDP..s.a`)

hou <- dplyr::select(Cleaned.Data,
              Country,
              yqtr,
              `Real.House.Prices`)

def <- dplyr::select(Cleaned.Data,
              Country,
              yqtr,
              `GDP.Deflator..s.a`)

res <- dplyr::select(Cleaned.Data,
              Country,
              yqtr,
              `Residential.Investment..Real.s.a`)

vol <- dplyr::select(Cleaned.Data,
              Country,
              yqtr,
              `Volatility`)

ass <- dplyr::select(Cleaned.Data,
              Country,
              yqtr,
              `ECB.Assets`)

#Rename Volatility for convenience
vol$vol <- vol$Volatility

#Want logs of most of these
if(ln == T) {
gdp$lgdp <- log(gdp$Real.GDP..s.a)
hou$lhou <- log(hou$Real.House.Prices)
def$ldef <- log(def$GDP.Deflator..s.a)
res$lres <- log(res$Residential.Investment..Real.s.a)
ass$lass <- log(ass$ECB.Assets)
}
if(ln == F) {
gdp$lgdp <- (gdp$Real.GDP..s.a)
hou$lhou <- (hou$Real.House.Prices)
def$ldef <- (def$GDP.Deflator..s.a)
res$lres <- (res$Residential.Investment..Real.s.a)
ass$lass <- (ass$ECB.Assets)
}

#Drop the not logged variables
lgdp <- dplyr::select(gdp, -Real.GDP..s.a)
lhou <- dplyr::select(hou, -Real.House.Prices)
ldef <- dplyr::select(def, -GDP.Deflator..s.a)
lres <- dplyr::select(res, -Residential.Investment..Real)
lass <- dplyr::select(ass, -ECB.Assets)
lvol <- dplyr::select(vol, -Volatility)

#Merge all the variables together
pan <- merge(lgdp, lhou)
pan <- merge(pan, lhou)
pan <- merge(pan, lres)
pan <- merge(pan, ldef)
pan <- merge(pan, lass)
pan <- merge(pan, lvol)




#Don't need this

#Make each country its own column
#lgdp <- pivot_wider(lgdp, names_from = Country, values_from = lgdp)
#lhou <- pivot_wider(lhou, names_from = Country, values_from = lhou)
#ldef <- pivot_wider(ldef, names_from = Country, values_from = ldef)
#lres <- pivot_wider(lres, names_from = Country, values_from = lres)
#vol <- pivot_wider(vol, names_from = Country, values_from = Volatility)
#lass <- pivot_wider(lass, names_from = Country, values_from = lass)

#Variable with all the country names
countries <- unique(gdp$Country)
countries <- sort(countries)


#Variable names
var_names <- c("lgdp", "lhou", "ldef", "lres", "vol", "lass")
var_names_fancy <- c("Log Real GDP ", "Log of Real House Prices","Log of GDP Deflator", "Log of Residential Investment", "Stock Market Volatility", "log of ECB Total Assets")
#List with a list for all countries
data <- list(NULL)
for(i in countries) {
  data[[i]] <- list(NULL,NULL,NULL,NULL,NULL,NULL)
  names(data[[i]]) <- var_names
}
data <- data[-1]

#Making a list with each country as its own dataframe
for(i in 1:length(countries)) {
  v1 <- with(gdp, lgdp[`Country` == Country[i]])
  v2 <- with(hou, lhou[`Country` == Country[i]])
  v3 <- with(def, ldef[`Country` == Country[i]])
  v4 <- with(res, lres[`Country` == Country[i]])
  v5 <- with(vol,  vol[`Country` == Country[i]])
  v6 <- with(ass, lass[`Country` == Country[i]])
  
  #Making it a time series
  v1 <- ts(v1, start = sta, end = fin, frequency = 4)
  v2 <- ts(v2, start = sta, end = fin, frequency = 4)
  v3 <- ts(v3, start = sta, end = fin, frequency = 4)
  v4 <- ts(v4, start = sta, end = fin, frequency = 4)
  v5 <- ts(v5, start = sta, end = fin, frequency = 4)
  v6 <- ts(v6, start = sta, end = fin, frequency = 4)
  
  
  #Bind them all together
  data[[countries[i]]] <- cbind(v1,v2,v3,v4,v5,v6, deparse.level = 0)
  colnames(data[[countries[i]]]) <- var_names
  
}



Aus <- data$Ireland

Aus[1,]

lagselectaus <- VARselect(Aus, lag.max = 9, type = "const")
lagselectaus


library(foreign)
write.csv(Aus, "mydata.csv")







#Generic object to store results by country
lagselect <- list(NULL)
for(i in countries) {
  lagselect[[i]] <- list(NULL)
}
lagselect <- lagselect[-1]
model1 <- lagselect
model2 <- model1
model3 <- model1 
model4 <- model1


#lagselect
lags <- NULL
for(i in 1:length(countries)) {
  lagselect[[countries[i]]] <- VARselect(data[[countries[i]]], lag.max = 10, type = "const")
  lags <- c(lags, lagselect[[countries[i]]][1])
}

k = 2


#The actual VAR
for(i in 1:length(countries)) {
  model1[[countries[i]]] <- VAR(data[[countries[i]]], p = k, type = "const", season = NULL, exogen = NULL)
}
summary(model1[[1]])
yup <- model1[[1]]
unclass(yup$varresult)
names(yup$varresult)
names(yup[[1]][[1]])
yup[[1]][[1]][[1]]
#First gives the country, second index is varresults, third chooses the variable, fourth gives the coefficients

model1[[1]][[1]][[1]][[1]]
model1[[2]][[1]][[1]][[1]]
(model1[[1]][[1]][[1]][[1]] + model1[[2]][[1]][[1]][[1]]) / 2
#For every variable
bvar()

#Using the PVAR package
#gmm.model1 <- pvargmm(dependent_vars = var_names, data = pan, lags = k, panel_identifier = c("Country", "yqtr"))

#Using the BVAR package
for(i in 1:length(countries)) {
  model2[[countries[i]]] <- bvar(data = data[[countries[i]]], lags = k, bv_irf = c(identification = FALSE))
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

#Run the model
for(i in 1:length(countries)) {
  model2[[countries[i]]] <- par_bvar(cl = cl, data = data[[countries[i]]], lags = k, bv_irf = c(identification = FALSE))
}
yup <- par_bvar(cl = cl, data = data$Austria, lags = k, n_draw = 60000, n_burn = 10000, n_thin = 20)
stopCluster(cl)
#Make it a coda object
austria_mcmc <- as.mcmc(yup)
gelman.plot(austria_mcmc)
effectiveSize(austria_mcmc)
acf(austria_mcmc$x)