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
source("~/Thesis/R Code/grangertestcode.R")

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
pan <- pan[,c(1,2,4,3,5,6,7)]
panel <- pan
pan <- filter(pan, Country == "Austria" | Country == "France")

#Variable with all the country names
countries <- unique(gdp$Country)
countries <- sort(countries)



#Variable names
crashyrs <- as.character(seq(from = 2007, to = 2009.75, by = .25))
var_names<- c("lgdp", "lhou", "lres", "ldef", "int")
var_names_full <- c(var_names, crashyrs)
var_names_fancy <- c("Log Real GDP ", "Log of Real House Prices", "Log of Residential Investment", "Log of GDP Deflator", "Stock Market Volatility", "log of ECB Total Assets")



#A list for all countries
data <- list()

#Making a list with each country as its own dataframe
for(i in 1:length(countries)) {
  sta <- stayr[countries[i]]
  end <- endyr[countries[i]]
  v1 <- with(gdp, lgdp[`Country` == countries[i]])
  v2 <- with(hou, lhou[`Country` == countries[i]])
  v3 <- with(res, lres[`Country` == countries[i]])
  v4 <- with(def, ldef[`Country` == countries[i]])
  v5 <- with(int, int[`Country` == countries[i]])

  
  #Making it a time series
  v1 <- ts(v1, start = sta, end = fin, frequency = 4)
  v2 <- ts(v2, start = sta, end = fin, frequency = 4)
  v3 <- ts(v3, start = sta, end = fin, frequency = 4)
  v4 <- ts(v4, start = sta, end = fin, frequency = 4)
  v5 <- ts(v5, start = sta, end = fin, frequency = 4)
  
  #Dummy Variables for years
  v6 <- with(Cleaned.Data, `2007`[`Country` == countries[i]])
  v7 <- with(Cleaned.Data, `2007.25`[`Country` == countries[i]])
  v8 <- with(Cleaned.Data, `2007.5`[`Country` == countries[i]])
  v9 <- with(Cleaned.Data, `2007.75`[`Country` == countries[i]])
  v10<- with(Cleaned.Data, `2008`[`Country` == countries[i]])
  v11<- with(Cleaned.Data, `2008.25`[`Country` == countries[i]])
  v12<- with(Cleaned.Data, `2008.5`[`Country` == countries[i]])
  v13<- with(Cleaned.Data, `2008.75`[`Country` == countries[i]])
  v14<- with(Cleaned.Data, `2009`[`Country` == countries[i]])
  v15<- with(Cleaned.Data, `2009.25`[`Country` == countries[i]])
  v16<- with(Cleaned.Data, `2009.5`[`Country` == countries[i]])
  v17<- with(Cleaned.Data, `2009.75`[`Country` == countries[i]])

  
  #Bind them all together
  data[[countries[i]]] <- cbind(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17, deparse.level = 0)
  colnames(data[[countries[i]]]) <- var_names_full
  
}

temp1 <- filter(Cleaned.Data, `Country` == "Austria")
temp <- with(int, int[`Country` == "Austria"])
colnames(data[["Austria"]])
temp <- ts(temp, start = sta, end = fin, frequency = 4)
data$Austria[,5]
Aus <- data$Estonia


lagselectaus <- VARselect(Aus[,num_var], lag.max = 9, type = "const")

colnames(Aus[,16])

pan$id <- NA
library(foreign)
for(i in 1:nrow(pan)) {
  country <- pan$Country[i]
  pan$id[i] <- which(countries == country)
}
pan$yqtr <- pan$yqtr*4
write.csv(pan, "mydata.csv")


plot(Aus[,5])
colnames(Aus)



#Generic object to store results by country
lagselect <- list()
model1 <- list()
model2 <- list()
irfmodel1 <- list()
irfmodel2 <- list()


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

#Choleski decomposition
#Set up the structural matrix
amat <- diag(5)
amat[2:5,1] <- NA
amat[3:5,2] <- NA
amat[4:5,3] <- NA
amat[5,4]   <- NA
amat

irevar <- VAR(Aus[,num_var], p = laglen, type = "const", season = NULL, exogen = Aus[,6:17])
summary(irevar)
iregrange <- bazgrangertest(Aus[,num_var], p = laglen, type = "const", season = NULL, exogen = Aus[,6:17])
summary(iregrange)
SVARire <- SVAR(irevar, Amat = amat, Bmat = NULL, exogen = Aus[,6:17])
irfire1 <- vars:::irf(SVARire, n.ahead = ahead, impulse = "int", response = c("lgdp", "lhou"), boot = T, runs = 1000, ci = .95)
plot(irfire1)
irfire2 <- vars:::irf(irevar, n.ahead = ahead, impulse = "int", response = c("lgdp", "lhou"), boot = T, runs = 1000, ci = .95)
plot(irfire2)
sumire <- summary(irevar)
t(chol(sumire$covres))
sumire2 <- summary(SVARire)

#The actual VAR
model1 <- list()
for(i in 1:length(countries)) {
  temp <- data[[countries[i]]][,num_var]
  temp1<- data[[countries[i]]][,6:17]
  model1[[countries[i]]] <- VAR(temp, p = laglen, type = "const", season = NULL, exogen = temp1)
}
temp <- data[[countries[2]]]
#Structural VAR
for(i in 1:length(countries)) {
  model2[[countries[i]]] <- SVAR(model1[[countries[i]]], Amat = amat, Bmat = NULL)
}

#Drop France because roots outside the unit circle
model1$France <- NULL
model2$France <- NULL
countries <- countries[-3]

#irfs
irfmodel1 <- list()
for(i in 1:length(countries)) {
  est <- model1[[countries[i]]]
  irfmodel1[[countries[i]]] <- vars::irf(est, n.ahead = ahead, impulse = "int", response = c("lhou", "lgdp", "ldef", "lres"), boot = T, runs = 1000, ci = .95)
}
countries
summary(est)

names(irfmodel1)
for(i in 1:length(countries)) {
  irfmodel2[[countries[i]]] <- vars::irf(model2[[countries[i]]], n.ahead = ahead, impulse = "int", response = c("lhou"), boot = T, runs = 1000, ci = .95)
}
trump <- irfmodel1$Ireland
for(i in 1:length(countries)) {
  plot(irfmodel1[[countries[i]]], main = countries[i])
}
#Impulse response coefficients
impmain <- matrix(NA, nrow = ahead + 1, ncol = length(countries))
implow <- matrix(NA, nrow = ahead + 1, ncol = length(countries))
imphigh <- matrix(NA, nrow = ahead + 1, ncol = length(countries))
for(i in 1:length(countries)) {
  impmain[,i] <- irfmodel1[[i]][[1]][[1]]
  implow[,i] <- irfmodel1[[i]][[2]][[1]]
  imphigh[,i] <- irfmodel1[[i]][[3]][[1]]
}
get
impma <- as.matrix(apply(impmain, 1, mean))
implo <- as.matrix(apply(implow, 1, mean))
imphi <- as.matrix(apply(imphigh, 1, mean))

imp <- cbind(impma,implo,imphi)
colnames(imp) <- rep("lhou", 3)
irfave <- irfmodel1$Austria

for(i in 1:3) {
  irfave[[i]][[1]][,1] <- imp[,i]
}

plot(irfave)

summary(model1$Ireland)
plot(irfmodel1$Estonia)
irfmodel1[[1]][[1]][[1]]
class(irfave[[1]][[1]])

#checking that all the roots are inside the unit circle
roots <- NULL
temp <- NULL
templist <- list()
for(i in 1:length(countries)) {
  temp <- vars:::roots(model1[[countries[i]]])
  tempcols <- c(tempcols, temp)
}

any(roots >= 1)
#France, Estonia has roots outside the unit circle
roots >= 1
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
