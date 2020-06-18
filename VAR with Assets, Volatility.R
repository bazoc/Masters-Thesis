#VAR with CB balance, volatility, real GDP, GDP Deflator, Real House Prices, Residential Investment
rm(list = ls())
VAR2 <- read.csv("Data/VAR2.csv")
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(BVAR)
library(plm)

#Year we are using
sta <- 2008
fin <- 2019 

#House prices go up to 2020, unconventional monetary policy starts at 2008
VAR2 <- subset(VAR2,
               year >= sta & year <= fin)
#No residential investment for belgium

miss <- subset(VAR2,
               is.na(VAR2$Real.House.Prices))
#No Nominal house prices forlithuania latvia
#No residential investment for belgium
VAR2 <- subset(VAR2,
               Country != "Lithuania" & Country != "Latvia" & Country != "Belgium" )
miss <- subset(VAR2,
               is.na(VAR2))
#Now no missing observations lets go


#Make individual variable with columns for every country 
gdp <- dplyr::select(VAR2,
              Country,
              yqtr,
              `Real.GDP..s.a`)

hou <- dplyr::select(VAR2,
              Country,
              yqtr,
              `Real.House.Prices`)

def <- dplyr::select(VAR2,
              Country,
              yqtr,
              `GDP.Deflator..s.a`)

res <- dplyr::select(VAR2,
              Country,
              yqtr,
              `Residential.Investment..Real`)

vol <- dplyr::select(VAR2,
              Country,
              yqtr,
              `Volatility`)

ass <- dplyr::select(VAR2,
              Country,
              yqtr,
              `ECB.Assets`)

#Rename Volatility for convenience
vol$vol <- vol$Volatility

#Want logs of most of these
gdp$lgdp <- log(gdp$Real.GDP..s.a)
hou$lhou <- log(hou$Real.House.Prices)
def$ldef <- log(def$GDP.Deflator..s.a)
res$lres <- log(res$Residential.Investment..Real)
ass$lass <- log(ass$ECB.Assets)

#Don't need this went a different way

#Drop the not logged variables
#lgdp <- select(gdp, -Real.GDP..s.a)
#lhou <- select(hou, -Real.House.Prices)
#ldef <- select(def, -GDP.Deflator..s.a)
#lres <- select(res, -Residential.Investment..Real)
#lass <- select(ass, -ECB.Assets)


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


#Generic object to store results by country
lagselect <- list(NULL)
for(i in countries) {
  lagselect[[i]] <- list(NULL)
}
lagselect <- lagselect[-1]
model1 <- lagselect


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