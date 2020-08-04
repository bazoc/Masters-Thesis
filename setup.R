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
library(broom)

#Full years we are using
sta.full<- 2000.25
fin.full <- 2020

#Pre recession
pre.fin <- 2008

#Post recession
post.sta <- 2012

#Set whether VAR is in log or levels terms
ln = T
#House prices go up to 2020, unconventional monetary policy starts at 2008
Cleaned.Data <- filter(Cleaned.Data,
                       yqtr >= sta.full& yqtr <= fin.full)
Cleaned.Data <- dplyr::select(Cleaned.Data, - X)
rownames(Cleaned.Data) <- NULL

miss <- filter(Cleaned.Data,
               is.na(Cleaned.Data$Real.GDP..s.a))
unique(miss$Country)
#Real GDP there

miss <- filter(Cleaned.Data,
               is.na(Cleaned.Data$GDP.Deflator..s.a))
unique(miss$Country)
#Deflator there

miss <- filter(Cleaned.Data,
               is.na(Cleaned.Data$Residential.Investment..Real.s.a))
unique(miss$Country)
#No res investment for Belgium

miss <- filter(Cleaned.Data,
               is.na(Cleaned.Data$Real.GFCF..s.a))
unique(miss$Country)
#Capital Formation there

#miss <- filter(Cleaned.Data,
#               is.na(Cleaned.Data$`Real GNI`))
#unique(miss$Country)
#No missing values

miss <- filter(Cleaned.Data,
               is.na(Cleaned.Data$Real.House.Prices))
unique(miss$Country)
miss <- filter(Cleaned.Data, !is.na(Cleaned.Data$Real.House.Prices))
stayr <- by(miss, miss$Country, function(m) min(m[,1]))
endyr <- by(miss, miss$Country, function(m) max(m[,1]))
any(endyr != 2019.75)
#missing Nominal house prices for lithuania (None), latvia (None), slovak R (2005), luxembourg (2007), Estonia (2005), slovenia (2007), all end at 2019.75

#No residential investment for belgium
missing.countries <- c("Belgium", "Lithuania", "Latvia", "Slovak Republic", "Luxembourg", "Estonia", "Slovenia")

Cleaned.Data <- filter(Cleaned.Data,
                       !(Cleaned.Data$Country %in% missing.countries))

#Drop the years for the countries that are missing values
Cleaned.Data <- filter(Cleaned.Data, !is.na(Cleaned.Data$Real.House.Prices))

unique(Cleaned.Data$Country)
econnames <- colnames(Cleaned.Data)
Cleaned.Data$crashdum <- 0
#Make a dummy variable for  year and qtr 2007-2009
firstcrashqtrr = 2007
lastcrashyqtr = 2010
crashyrs <-seq(from = firstcrashqtrr, lastcrashyqtr, by = .25)
for(i in 1:nrow(Cleaned.Data)) {
  if(Cleaned.Data$yqtr[i] %in% crashyrs) {
    Cleaned.Data$crashdum[i] <- 1
  }
}

experiment <- pivot_wider(Cleaned.Data,names_from = yqtr, values_from = crashdum)

#Have to fix this if you add more columns or dates!!!!!!!!!!
experiment[is.na(experiment)] <- 0
experiment <- experiment[,as.character(crashyrs)]
Cleaned.Data <- cbind(Cleaned.Data, experiment)

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
gfcf <- dplyr::select(Cleaned.Data,
                     Country,
                     yqtr,
                     'Real.GFCF..s.a')
#gni <- dplyr::select(Cleaned.Data,
#                      Country,
#                      yqtr,
#                      'Real.GFCF..s.a')#


crashdummies <- Cleaned.Data[, c("Country", "yqtr", crashyrs)]

#Rename Volatility for convenience
vol$vol <- vol$Volatility

#Want logs of most of these

gdp$lgdp <- log(gdp$Real.GDP..s.a)
hou$lhou <- log(hou$Real.House.Prices)
def$ldef <- log(def$GDP.Deflator..s.a)
res$lres <- log(res$Residential.Investment..Real.s.a)
ass$lass <- log(ass$ECB.Assets)
gfcf$lgfcf <- log(gfcf$Real.GFCF..s.a)
#gni$lgni <- log(gni$`Real GNI`)
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
lgfcf <-dplyr::select(gfcf, -`Real.GFCF..s.a`)

#For the graphs want not logged variables
nlgdp <- dplyr::select(gdp, Country, yqtr,Real.GDP..s.a)
nlhou <- dplyr::select(hou, Country, yqtr,Real.House.Prices)
nldef <- dplyr::select(def, Country, yqtr, GDP.Deflator..s.a, yqtr)
nlres <- dplyr::select(res, Country, yqtr, Residential.Investment..Real.s.a, yqtr)
nlass <- dplyr::select(ass, Country, yqtr, ECB.Assets)
colnames(nlgdp) <- c("Country", "yqtr", "gdp")
colnames(nlhou) <- c("Country", "yqtr", "hou")
colnames(nldef) <- c("Country", "yqtr", "def")
colnames(nlres) <- c("Country", "yqtr", "res")
colnames(nlass) <- c("Country", "yqtr", "ass")


#Merge all the variables together
pan1 <- merge(lgdp, lhou)
pan1 <- merge(pan1, lhou)
pan1 <- merge(pan1, lres)
pan1 <- merge(pan1, ldef)
pan1 <- merge(pan1, lgfcf)
pan <- merge(pan1, lint)
pan <- pan[,c("Country", "yqtr", "lgdp", "lres", "ldef", "int", "lhou")]
main.panel <- pan
exog.panel <- merge(pan, crashdummies)
pre.panel <- filter(main.panel,
                    yqtr < pre.fin)
post.panel<- filter(main.panel,
                    yqtr >= post.sta)
gfcf.panel<- merge(main.panel, lgfcf)
gfcf.panel <- gfcf.panel[,c("Country", "yqtr", "lgdp", "lgfcf", "lhou", "lres", "ldef", "int")]

dum.panel <- main.panel
dum.panel$dumcrash <- 0
for(i in 1:nrow(dum.panel)) {
  if(dum.panel$yqtr[i] %in% seq(from = pre.fin, to = fin.full, by = .25)) {
    dum.panel$dumcrash[i] <- 1
  }
}

complete.panel <- main.panel
#With CB Assets
assets.panel <- merge(pan1, lass)
complete.panel <- merge(complete.panel, lass)
assets.panel <- merge(assets.panel, lvol)
complete.panel <- merge(complete.panel, lvol)
#Only want post recession years
assets.panel <- filter(assets.panel,
                       yqtr >= post.sta)

#For the graphs
pan2 <- merge(nlgdp, nlhou)
pan2 <- merge(pan2, nlhou)
pan2 <- merge(pan2, nlres)
pan2 <- merge(pan2, nldef)
pan2 <- merge(pan2, lint)
pan2 <- pan2[,c(1,2,4,3,5,6,7)]
pan2 <- merge(pan2, nlass)
notlogged.panel <- merge(pan2, lvol)

#Countries
countries <- unique(gdp$Country)
countries <- sort(countries)


#Variable names
var.names.main<- c("lgdp", "lres", "ldef", "int", "lhou")
var.names.dummy <- c(var.names.main, crashyrs)
var.names.fancy.main <- c("Log of Real GDP ", "Log of Residential Investment", "Log of GDP Deflator", "Shadow Policy Rate", "Log of Real House Prices")
var.names.assets <- c("lgdp", "ldef", "lres", "vol", "lass", "lhou")
var.names.fancy.assets <- c("Log Real GDP ", "Log of Real House Prices","Log of GDP Deflator", "Log of Residential Investment", "Stock Market Volatility", "log of ECB Total Assets")
var.names.full <- c("lgdp", "ldef", "lres", "int", "vol", "lass", "lhou")
var.names.gfcf<- c("lgdp", "lgfcf", "lres", "ldef", "int", "lhou")

#Choleski decomposition
#Set up the structural matrix
amat <- diag(5)
amat[2:5,1] <- NA
amat[3:5,2] <- NA
amat[4:5,3] <- NA
amat[5,4]   <- NA
amat

north <- c("Germany", "France", "Ireland", "Austria", "Finland", "Netherlands")
south <- c("Greece", "Italy", "Portugal", "Spain")

#Making a list with each country as its own dataframe
var.names.dummy.full <- c(var.names.main, var.names.assets[5:6], crashyrs)

data <- list()
for(i in 1:length(countries)) {
  sta<- stayr[countries[i]]
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
  #v8 <- with(Cleaned.Data, `2007`[`Country` == countries[i]])
  #v9 <- with(Cleaned.Data, `2007.25`[`Country` == countries[i]])
  #v10 <- with(Cleaned.Data, `2007.5`[`Country` == countries[i]])
  #v11 <- with(Cleaned.Data, `2007.75`[`Country` == countries[i]])
  #v12<- with(Cleaned.Data, `2008`[`Country` == countries[i]])
  #v13<- with(Cleaned.Data, `2008.25`[`Country` == countries[i]])
  #v14<- with(Cleaned.Data, `2008.5`[`Country` == countries[i]])
  #v15<- with(Cleaned.Data, `2008.75`[`Country` == countries[i]])
  #v16<- with(Cleaned.Data, `2009`[`Country` == countries[i]])
  #v17<- with(Cleaned.Data, `2009.25`[`Country` == countries[i]])
  #v18<- with(Cleaned.Data, `2009.5`[`Country` == countries[i]])
  #v19<- with(Cleaned.Data, `2009.75`[`Country` == countries[i]])
  
  #Bind them all together
  data[[countries[i]]] <- cbind(v1,v2,v3,v4,v5,v6,v7,#v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19, 
                                deparse.level = 0)
  colnames(data[[countries[i]]]) <- var.names.full
}
rm(list = c("pan", "pan1", "miss", "hou", "int", "lass", "ldef", "lgdp", "lhou", "lint", "lres", "lvol", "res",
            "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17",
            "v18", "v19", "num_varmain", "endyr", "sta", "end", "stayr", "econnames", "i", "missing.countries"))
#Bringing in all my functions
source("~/Thesis/R Code/VARselect.R")
source("~/Thesis/R Code/grangertestcode.R")
source("~/Thesis/R Code/fixing irf.R")
source("~/Thesis/R Code/fixing plot(irf).R")