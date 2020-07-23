#VAR with Shadow Policy Rate, real GDP, GDP Deflator, Real House Prices, Residential Investment
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

#Full years we are using
sta.full<- 2000.5
fin.full <- 2019.75

#Pre recession
pre.fin <- 2008

#Post recession
post.sta <- 2012

#Set whether VAR is in log or levels terms
ln = T
#House prices go up to 2020, unconventional monetary policy starts at 2008
Cleaned.Data <- subset(Cleaned.Data,
                       yqtr >= sta.full& yqtr <= fin.full)
Cleaned.Data <- dplyr::select(Cleaned.Data, - X)
rownames(Cleaned.Data) <- NULL

miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data$Real.GDP..s.a))
unique(miss$Country)
#Real GDP there

miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data$GDP.Deflator..s.a))
unique(miss$Country)
#Deflator there

miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data$Residential.Investment..Real.s.a))
unique(miss$Country)
#No res investment for Belgium

miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data$Real.GFCF..s.a))
unique(miss$Country)
#Capital Formation there

miss <- subset(Cleaned.Data,
               is.na(Cleaned.Data$Real.House.Prices))
unique(miss$Country)
miss <- filter(Cleaned.Data, !is.na(Cleaned.Data$Real.House.Prices))
stayr <- by(miss, miss$Country, function(m) min(m[,1]))
endyr <- by(miss, miss$Country, function(m) max(m[,1]))
any(endyr != 2019.75)
#missing Nominal house prices for lithuania (None), latvia (None), slovak R (2005), luxembourg (2007), Estonia (2005), slovenia (2007), all end at 2019.75

#No residential investment for belgium
missing.countries <- c("Belgium", "Lithuania", "Latvia", "Slovak Republic", "Luxembourg", "Estonia", "Slovenia")

Cleaned.Data <- subset(Cleaned.Data,
                       !(Cleaned.Data$Country %in% missing.countries))

#Drop the years for the countries that are missing values
Cleaned.Data <- filter(Cleaned.Data, !is.na(Cleaned.Data$Real.House.Prices))

unique(Cleaned.Data$Country)
econnames <- colnames(Cleaned.Data)
Cleaned.Data$crashdum <- 0
#Make a dummy variable for  year and qtr 2007-2009
for(i in 1:nrow(Cleaned.Data)) {
  if(Cleaned.Data$year[i] %in% 2007:2009) {
    Cleaned.Data$crashdum[i] <- 1
  }
}

experiment <- pivot_wider(Cleaned.Data,names_from = yqtr, values_from = crashdum)

#Have to fix this if you add more columns or dates!!!!!!!!!!
experiment[is.na(experiment)] <- 0
experiment <- experiment[,48:59]
Cleaned.Data <- cbind(Cleaned.Data, experiment)
crashyrs <-colnames(experiment)

#Number of proper variables
num_varmain <- 1:5

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

int <- dplyr::select(Cleaned.Data,
                     Country,
                     yqtr,
                     `Shadow.Policy.Rate`)

vol <- dplyr::select(Cleaned.Data,
                     Country,
                     yqtr,
                     `Volatility`)

ass <- dplyr::select(Cleaned.Data,
                     Country,
                     yqtr,
                     `ECB.Assets`)


crashdummies <- Cleaned.Data[, c("Country", "yqtr", crashyrs)]

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

#Give shadow rates better name
int$int <- int$Shadow.Policy.Rate

#Drop the not logged variables
lgdp <- dplyr::select(gdp, -Real.GDP..s.a)
lhou <- dplyr::select(hou, -Real.House.Prices)
ldef <- dplyr::select(def, -GDP.Deflator..s.a)
lres <- dplyr::select(res, -Residential.Investment..Real.s.a)
lint <- dplyr::select(int, -Shadow.Policy.Rate)
lass <- dplyr::select(ass, -ECB.Assets)
lvol <- dplyr::select(vol, -Volatility)

#Merge all the variables together
pan1 <- merge(lgdp, lhou)
pan1 <- merge(pan1, lhou)
pan1 <- merge(pan1, lres)
pan1 <- merge(pan1, ldef)
pan <- merge(pan1, lint)
pan <- pan[,c(1,2,4,3,5,6,7)]
full.panel <- pan
dum.panel <- merge(pan, crashdummies)
pre.panel <- filter(full.panel,
                    yqtr < pre.fin)
post.panel<- filter(full.panel,
                    yqtr >= post.sta)

#With CB Assets
assets.panel <- merge(pan1, lass)
assets.panel <- merge(assets.panel, lvol)

#Only want post recession years
assets.panel <- filter(assets.panel,
                       yqtr >= post.sta)

#Countries
countries <- unique(gdp$Country)
countries <- sort(countries)


#Variable names
var.names.main<- c("lgdp", "lhou", "lres", "ldef", "int")
var.names.dummy <- c(var.names.main, crashyrs)
var.names.fancy.main <- c("Log of Real GDP ", "Log of Real House Prices", "Log of Residential Investment", "Log of GDP Deflator", "Shadow Policy Rate")
var.names.assets <- c("lgdp", "lhou", "ldef", "lres", "vol", "lass")
var.names.fancy.assets <- c("Log Real GDP ", "Log of Real House Prices","Log of GDP Deflator", "Log of Residential Investment", "Stock Market Volatility", "log of ECB Total Assets")

#Choleski decomposition
#Set up the structural matrix
amat <- diag(5)
amat[2:5,1] <- NA
amat[3:5,2] <- NA
amat[4:5,3] <- NA
amat[5,4]   <- NA
amat



#Making a list with each country as its own dataframe
var.names.dummy.full <- c(var.names.main, var.names.assets[5:6], crashyrs)

data <- list()
for(i in 1:length(countries)) {
  sta.full<- stayr[countries[i]]
  end <- endyr[countries[i]]
  v1 <- with(gdp, lgdp[`Country` == countries[i]])
  v2 <- with(hou, lhou[`Country` == countries[i]])
  v3 <- with(res, lres[`Country` == countries[i]])
  v4 <- with(def, ldef[`Country` == countries[i]])
  v5 <- with(int, int[`Country` == countries[i]])
  v6 <- with(vol,  vol[`Country` == Country[i]])
  v7 <- with(ass, lass[`Country` == Country[i]])
  
  
  #Making it a time series
  v1 <- ts(v1, start = sta.full, end = fin.full, frequency = 4)
  v2 <- ts(v2, start = sta.full, end = fin.full, frequency = 4)
  v3 <- ts(v3, start = sta.full, end = fin.full, frequency = 4)
  v4 <- ts(v4, start = sta.full, end = fin.full, frequency = 4)
  v5 <- ts(v5, start = sta.full, end = fin.full, frequency = 4)
  v6 <- ts(v5, start = sta.full, end = fin.full, frequency = 4)
  v7 <- ts(v6, start = sta.full, end = fin.full, frequency = 4)
  
  
  #Dummy Variables for years
  v8 <- with(Cleaned.Data, `2007`[`Country` == countries[i]])
  v9 <- with(Cleaned.Data, `2007.25`[`Country` == countries[i]])
  v10 <- with(Cleaned.Data, `2007.5`[`Country` == countries[i]])
  v11 <- with(Cleaned.Data, `2007.75`[`Country` == countries[i]])
  v12<- with(Cleaned.Data, `2008`[`Country` == countries[i]])
  v13<- with(Cleaned.Data, `2008.25`[`Country` == countries[i]])
  v14<- with(Cleaned.Data, `2008.5`[`Country` == countries[i]])
  v15<- with(Cleaned.Data, `2008.75`[`Country` == countries[i]])
  v16<- with(Cleaned.Data, `2009`[`Country` == countries[i]])
  v17<- with(Cleaned.Data, `2009.25`[`Country` == countries[i]])
  v18<- with(Cleaned.Data, `2009.5`[`Country` == countries[i]])
  v19<- with(Cleaned.Data, `2009.75`[`Country` == countries[i]])
  
  #Bind them all together
  data[[countries[i]]] <- cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19, deparse.level = 0)
  colnames(data[[countries[i]]]) <- var.names.dummy.full
}

#Bringing in all my functions
source("~/Thesis/R Code/VARselect.R")
source("~/Thesis/R Code/grangertestcode.R")
source("~/Thesis/R Code/fixing irf.R")