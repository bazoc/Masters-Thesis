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
#Year we are using
sta <- 2000.5
fin <- 2019.75

#Set whether VAR is in log or levels terms
ln = T

#House prices go up to 2020, unconventional monetary policy starts at 2008
Cleaned.Data <- subset(Cleaned.Data,
                       yqtr >= sta & yqtr <= fin)
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
#No Nominal house prices for lithuania, latvia, slovak R, luxembourg, Estonia, slovenia

#Not all missing forever, can go back later and add them to start at different time periods, jsut dropping for now
#No residential investment for belgium
missing.countries <- c("Belgium", "Lithuania", "Latvia", "Slovak Republic", "Luxembourg", "Estonia", "Slovenia")

Cleaned.Data <- subset(Cleaned.Data,
                       !(Cleaned.Data$Country %in% missing.countries))
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

#Number of proper variables
num_var <- 1:5

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


#Want logs of most of these
if(ln == T) {
  gdp$lgdp <- log(gdp$Real.GDP..s.a)
  hou$lhou <- log(hou$Real.House.Prices)
  def$ldef <- log(def$GDP.Deflator..s.a)
  res$lres <- log(res$Residential.Investment..Real.s.a)
}
if(ln == F) {
  gdp$lgdp <- (gdp$Real.GDP..s.a)
  hou$lhou <- (hou$Real.House.Prices)
  def$ldef <- (def$GDP.Deflator..s.a)
  res$lres <- (res$Residential.Investment..Real.s.a)
}

#Give shadow rates better name
int$int <- int$Shadow.Policy.Rate

#Drop the not logged variables
lgdp <- dplyr::select(gdp, -Real.GDP..s.a)
lhou <- dplyr::select(hou, -Real.House.Prices)
ldef <- dplyr::select(def, -GDP.Deflator..s.a)
lres <- dplyr::select(res, -Residential.Investment..Real.s.a)
lint <- dplyr::select(int, -Shadow.Policy.Rate)


#Merge all the variables together
pan <- merge(lgdp, lhou)
pan <- merge(pan, lhou)
pan <- merge(pan, lres)
pan <- merge(pan, ldef)
pan <- merge(pan, lint)


#Don't need this

#Make each country its own column
#lgdp <- pivot_wider(lgdp, names_from = Country, values_from = lgdp)
#lhou <- pivot_wider(lhou, names_from = Country, values_from = lhou)
#ldef <- pivot_wider(ldef, names_from = Country, values_from = ldef)
#lres <- pivot_wider(lres, names_from = Country, values_from = lres)
#int <- pivot_wider(int, names_from = Country, values_from = Volatility)


#Variable with all the country names
countries <- unique(gdp$Country)
countries <- sort(countries)



#Variable names
crashyrs <- as.character(seq(from = 2007, to = 2009.75, by = .25))
var_names<- c("lgdp", "lhou", "lres", "ldef", "int")
var_names_full <- c(var_names, crashyrs)
var_names_fancy <- c("Log Real GDP ", "Log of Real House Prices", "Log of Residential Investment", "Log of GDP Deflator", "Stock Market Volatility", "log of ECB Total Assets")



#List with a list for all countries
data <- list(NULL)
for(i in countries) {
  data[[i]] <- list(NULL,NULL,NULL,NULL,NULL)
  names(data[[i]]) <- var_names
}
data <- data[-1]


#Making a list with each country as its own dataframe
for(i in 1:length(countries)) {
  v1 <- with(gdp, lgdp[`Country` == Country[i]])
  v2 <- with(hou, lhou[`Country` == Country[i]])
  v3 <- with(res, lres[`Country` == Country[i]])
  v4 <- with(def, ldef[`Country` == Country[i]])
  v5 <- with(int, int[`Country` == Country[i]])

  
  #Making it a time series
  v1 <- ts(v1, start = sta, end = fin, frequency = 4)
  v2 <- ts(v2, start = sta, end = fin, frequency = 4)
  v3 <- ts(v3, start = sta, end = fin, frequency = 4)
  v4 <- ts(v4, start = sta, end = fin, frequency = 4)
  v5 <- ts(v5, start = sta, end = fin, frequency = 4)
  
  #Dummy Variables for years
  v6 <- with(Cleaned.Data, `2007`[`Country` == Country[i]])
  v7 <- with(Cleaned.Data, `2007.25`[`Country` == Country[i]])
  v8 <- with(Cleaned.Data, `2007.5`[`Country` == Country[i]])
  v9 <- with(Cleaned.Data, `2007.75`[`Country` == Country[i]])
  v10<- with(Cleaned.Data, `2008`[`Country` == Country[i]])
  v11<- with(Cleaned.Data, `2008.25`[`Country` == Country[i]])
  v12<- with(Cleaned.Data, `2008.5`[`Country` == Country[i]])
  v13<- with(Cleaned.Data, `2008.75`[`Country` == Country[i]])
  v14<- with(Cleaned.Data, `2009`[`Country` == Country[i]])
  v15<- with(Cleaned.Data, `2009.25`[`Country` == Country[i]])
  v16<- with(Cleaned.Data, `2009.5`[`Country` == Country[i]])
  v17<- with(Cleaned.Data, `2009.75`[`Country` == Country[i]])

  
  #Bind them all together
  data[[countries[i]]] <- cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17, deparse.level = 0)
  colnames(data[[countries[i]]]) <- var_names_full
  
}

temp1 <- filter(int, `Country` == "Austria")
temp <- with(int, int[`Country` == "Austria"])
colnames(data[["Austria"]])
temp <- ts(temp, start = sta, end = fin, frequency = 4)
data$Austria[,5]
Aus <- data$Ireland

Aus[1,]
Aus$lgdp
colnames(Aus)
lagselectaus <- VARselect(Aus[,num_var], lag.max = 9, type = "const")

colnames(Aus[,16])


library(foreign)
write.csv(Aus, "mydata.csv")


plot(Aus[,5])
colnames(Aus)



#Generic object to store results by country
lagselect <- list(NULL)
for(i in countries) {
  lagselect[[i]] <- list(NULL)
}
lagselect <- lagselect[-1]
model1 <- lagselect
coint  <- model1
model2 <- model1
model3 <- model1 
model4 <- model1



#lagselect
lags <- NULL
for(i in 1:length(countries)) {
  lagselect[[countries[i]]] <- VARselect(data[[countries[i]]][,num_var], lag.max = 9, type = "const")
  lags <- c(lags, lagselect[[countries[i]]][1])
}
lags
#SQ and SC mostly around 1 or 2, we will go for 2

laglen = 2
ahead = 40
irevar <- VAR(Aus[,num_var], p = laglen, type = "const", season = NULL, exogen = Aus[,6:17])
summary(irevar)
iregrange <- bazgrangertest(Aus[,num_var], p = laglen, cause = c("lgdp", "int"), type = "const", season = NULL, exogen = Aus[,6:17])
summary(iregrange)
SVARire <- SVAR(irevar, Amat = amat, Bmat = NULL)
irfire1 <- vars:::irf(SVARire, n.ahead = ahead, impulse = "int", response = c("lgdp", "lhou"), boot = T, runs = 1000, ci = .95)
plot(irfire1)
irfire2 <- vars:::irf(irevar, n.ahead = ahead, impulse = "int", response = c("lgdp", "lhou"), boot = T, runs = 1000, ci = .95)
plot(irfire2)
sumire <- summary(irevar)
t(chol(sumire$covres))
sumire2 <- summary(SVARire)
#The actual VAR
for(i in 1:length(countries)) {
  model1[[countries[i]]] <- VAR(data[[countries[i]]][,num_var], p = laglen, type = "const", season = NULL, exogen = NULL)
}
summary(model1$Ireland)

#checking that all the roots are inside the unit circle
roots <- NULL
temp <- NULL
for(i in 1:length(countries)) {
  temp <- vars:::roots(model1[[countries[i]]])
  roots <- c(roots, temp)
}
any(roots >= 1)
#France has roots outside the unit circle

#Cointegration
#cointire <- ca.jo(data$Ireland, type = "trace", ecdet = "const", K = laglen)
#cointire@teststat
#cointire@cval
#cointindex <- NULL
#Cointegration test for all the variables, trace statistic, this will break if more variables are added
#for(i in 1:length(countries)) {
#  coint[[countries[i]]] <- ca.jo(data[[countries[i]]], type = "trace", ecdet = "const", K = laglen)
#  #Check each in descending order
#  for(j in length(var_names)) {
#    if(coint[[countries[i]]]@cvaL[j,2] < coint[[countries[i]]]@teststat[j]) {
#      cointindex[countries[i]] <- length(var_names) - j
#      break
#    }
#  }
#}

#Breusch Godfrey test for serially correlated errors
serialire <- serial.test(model1$Ireland, lags.bg = 3, type = "BG")
serialire
#There is serial correlation

#Choleski decomposition
#Set up the structural matrix
amat <- diag(5)
amat[2:5,1] <- NA
amat[3:5,2] <- NA
amat[4:5,3] <- NA
amat[5,4]   <- NA
amat


ahead <- 40
irfire <- vars:::irf(model1$Ireland, n.ahead = ahead, impulse = "int", response = c("lgdp", "lhou"), boot = T, runs = 1000, ci = .95)
plot(irfire)


fevdire <- vars:::fevd(model1$Ireland, n.ahead = ahead)
plot(y = fevdire$lgdp[,1], ))
fevdire$lgdp