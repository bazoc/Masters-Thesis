setwd("~/Thesis")
library(dplyr)
library(tidyverse)
library(tidyr)
library(rio)
library(stringr)
library(ggplot2)
#Reading in two of the data sets
QNA <- read.csv("Original data/Quarterly national accounts original.csv", na.strings=c("","NA"))
CPI <- read.csv("Original data/OECD Prices original.csv", na.strings=c("","NA"))
BOI <- read.csv("Original data/OECD OG Big Boi.csv", na.strings=c("","NA"))
BIS <- read.csv("Original data/BIS Property Prices Nominal.csv", na.strings=c("","NA"))
BAL <- read.csv("Original data/ECB Total Assets FRED.csv", na.strings=c("","NA"))

#Variable with eurozone countries
eurozone <- c("Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovak Republic","Slovenia","Spain")

#Only keep Eurozone countries
QNA <- subset(QNA,
              Country %in% eurozone)
CPI <- subset(CPI,
              Country %in% eurozone)


#Measures were keeping
meas <- c("National currency, current prices, quarterly levels, seasonally adjusted", "National currency, current prices, quarterly levels",
          "Deflator, OECD reference year, seasonally adjusted")


QNA <- subset(QNA,
              Measure %in% meas)

#Check what flags there are
unique(QNA$Flags)
subset(QNA,
       Flags == "Break")

#Break in how statistics are collected in lithuania in 2010, Might have to look into this more

unique(CPI$Flags)
#No flags

#MERGE
econ <- merge.data.frame(CPI, QNA, all = T)
unique(econ$Subject)
unique(econ$Measure)
#Seems to have merged

#Make a single variable for measure and subject
econ$var <- NA


for(i in 1:nrow(econ)) {
  #CPI
  if(econ$Subject[i] == "CPI: 01-12 - All items" & econ$Measure[i] == "Index") econ$var[i] <- "CPI: All items"
  if(econ$Subject[i] == "CPI: 01-12 - All items" & econ$Measure[i] == "Index, s.a") econ$var[i] <- "CPI: All items, s.a"
  if(econ$Subject[i] == "CPI: 04.1 - CPI Actual rentals for housing" & econ$Measure[i] == "Index") econ$var[i] <- "CPI Actual rentals for housing"
  if(econ$Subject[i] == "CPI: 04.1 - CPI Actual rentals for housing" & econ$Measure[i] == "Index, s.a") econ$var[i] <- "CPI Actual rentals for housing, s.a"
  if(econ$Subject[i] == "CPI: 04.2 - CPI Imputed rentals for housing" & econ$Measure[i] == "Index") econ$var[i] <- "CPI Imputed rentals for housing"
  if(econ$Subject[i] == "CPI: 04.2 - CPI Imputed rentals for housing" & econ$Measure[i] == "Index, s.a") econ$var[i] <- "CPI Imputed rentals for housing, s.a"
  if(econ$Subject[i] == "CPI: Housing" & econ$Measure[i] == "Index") econ$var[i] <- "CPI: Housing"
  if(econ$Subject[i] == "CPI: Housing" & econ$Measure[i] == "Index, s.a") econ$var[i] <- "CPI: Housing, s.a"
  if(econ$Subject[i] == "CPI: Housing excluding imputed rentals for housing" & econ$Measure[i] == "Index") econ$var[i] <- "CPI: Housing excluding imputed rentals for housing"
  if(econ$Subject[i] == "CPI: Housing excluding imputed rentals for housing" & econ$Measure[i] == "Index, s.a") econ$var[i] <- "CPI: Housing excluding imputed rentals for housing, s.a"
  
  #QNA
  if(econ$Subject[i] == "Dwellings" & econ$Measure[i] == "National currency, current prices, quarterly levels, seasonally adjusted") econ$var[i] <- "Residential Investment, Nominal s,a"
  if(econ$Subject[i] == "Dwellings" & econ$Measure[i] == "National currency, current prices, quarterly levels") econ$var[i] <- "Residential Investment, Nominal"
  
  if(econ$Subject[i] == "Gross domestic product - expenditure approach" & econ$Measure[i] == "National currency, current prices, quarterly levels") econ$var[i] <- "GDP, Nominal"
  if(econ$Subject[i] == "Gross domestic product - expenditure approach" & econ$Measure[i] == "National currency, current prices, quarterly levels, seasonally adjusted") econ$var[i] <- "GDP, Nominal s.a"
  if(econ$Subject[i] == "Gross domestic product - expenditure approach" & econ$Measure[i] == "Deflator, OECD reference year, seasonally adjusted") econ$var[i] <- "GDP, Real"
  
  if(econ$Subject[i] == "Gross fixed capital formation" & econ$Measure[i] == "National currency, current prices, quarterly levels") econ$var[i] <- "GFCF, Nominal"
  if(econ$Subject[i] == "Gross fixed capital formation" & econ$Measure[i] == "National currency, current prices, quarterly levels, seasonally adjusted") econ$var[i] <- "GFCF, Nominal s.a"
  if(econ$Subject[i] == "Gross fixed capital formation" & econ$Measure[i] == "Deflator, OECD reference year, seasonally adjusted") econ$var[i] <- "GFCF, Real"
}

sum(is.na(econ$Value))
#Works

colnames(econ)
econ <- select(econ, ï..LOCATION, Country, TIME, Value, var)
#Make it wider
econ <- pivot_wider(econ,
                    names_from = var,
                    values_from = Value)

econ <- arrange(econ, Country, TIME)


#Make a year and quarter variable
econ$year = as.numeric(str_sub(econ$TIME, 1, 4))
econ$qtr  = as.numeric(str_sub(econ$TIME, 7, 7))
econ$yqtr = econ$year + (econ$qtr-1) / 4

#OECD REAL HOUSE PRICES - They are seasonally adjusted
RHO <- read.csv("Original data/OECD Real House prices.csv", na.strings=c("","NA"))
NHO <- read.csv("Original data/OECD Nominal House prices.csv", na.strings=c("","NA"))

#Check for flags
unique(RHO$Flag.Codes)
unique(NHO$Flag.Codes)
#None
colnames(RHO) <- c("ï..LOCATION", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Real House Prices", "Flag.Codes")
colnames(NHO) <- c("ï..LOCATION", "INDICATOR", "SUBJECT", "MEASURE", "FREQUENCY", "TIME", "Nominal House Prices", "Flag.Codes")

#Only keep columns we car about
RHO <- select(RHO, "ï..LOCATION", "TIME", "Real House Prices")
NHO <- select(NHO, "ï..LOCATION", "TIME", "Nominal House Prices")

#Merge them
house <- merge.data.frame(RHO, NHO, all = T)

#Merge them with the econ one

econ <- merge.data.frame(econ, house, all = T)


#Probably gonna be between 2000 and 2018, check how many variables missing in this timeframe
#count <- rep(NA, ncol(econ))
#miss <- subset(econ,
#               year >= 2000 & year <= 2018)
#names <- colnames(miss)
for(i in 1:ncol(miss)) {
  count[i] = sum(is.na(miss[[i]]))
}
cbind(names,count) 

#GDP is good
#Drop imputed rentals for housing
#Base CPI good
#CPI seasonally adjusted has to go
#Gross fixed capital formation good
#y <- subset(miss,
#            is.na(miss$`Residential Investment, Nominal`))
#y <- select(y, Country, TIME)


#Residential investment is missing Belgium
#Gonna drop all seasonals, probably for the best
#CPI Housing missing alot of stuff
#y <- subset(miss,
#            is.na(miss$`CPI: Housing excluding imputed rentals for housing`))
#y <- select(y, Country, TIME)
#Definitely can't use this for housing

#Other housing missing 228
#y <- subset(miss,
#            is.na(miss$`Real House Prices`))
#y <- select(y, Country, TIME)
#unique(y$Country)
#Missing estonia lithuania luxembours latvia slovenia
#Not major economies so really not the biggest deal
#
#y <- subset(miss,
#is.na(miss$`Nominal House Prices`))
#y <- select(y, Country, TIME)
#unique(y$Country)
#Same as the other one


#The columns we are getting rid of
#econ <- select(econ, -`CPI Imputed rentals for housing`, -`CPI: All items, s.a`, -`GFCF, Nominal s.a`, -`Residential Investment, Nominal s,a`, -`GDP, Nominal s.a`, -`CPI: Housing`, -`CPI: Housing excluding imputed rentals for housing`)


#Reading in the dataset from the Economic indicators database
BOI <- read.csv("Original data/OECD Main Economic Indicators Original.csv", na.strings=c("","NA"))

BOI <- subset(BOI,
              Country %in% eurozone)

unique(BOI$Subject)
unique(BOI$Measure)

BOI <- subset(BOI,
              Subject != "Interest Rates > Other long term rates and yields > Housing > Mortgage rates" &
                Subject != "Interest Rates > 3-month or 90-day rates and yields > Interbank rates > Total")


meas1 <- c("Level, rate or national currency", "Level, rate or national currency, s.a.",
           "Index 2015=100", "Index 2015=100, s.a.", "Index source base")

BOI <- subset(BOI,
              Measure %in% meas1)

#Dropping some variables
#Only need an indexed GDP
BOI <- subset(BOI,
              Subject != "National Accounts > GDP by Expenditure > Constant Prices > Gross Domestic Product - Total"
              | Measure != "Level, rate or national currency, s.a.")
BOI <- subset(BOI,
              Subject != "National Accounts > GDP by Expenditure > Constant Prices > Gross Fixed Capital Formation"
              | Measure != "Level, rate or national currency, s.a.")

#Dont need individual bases for CPI
BOI <- subset(BOI,
              Subject != "Consumer Price Index > All items > Total > Total"
              | Measure != "Index source base")

unique(BOI$Subject)
sm <- subset(BOI,
             Subject == "Consumer Price Index > Housing, water, electricity, gas and other fuels (COICOP 04) > Imputed rentals for housing > Total")
unique(sm$Measure)
BOI$var <- NULL 
for(i in 1:nrow(BOI)) {
  if(BOI$Subject[i] == "Share Prices > All shares/broad > Total > Total" & BOI$Measure[i] == "Index 2015=100") BOI$var[i] <- "Share Prices"
  if(BOI$Subject[i] == "National Accounts > GDP by Expenditure > Constant Prices > Gross Domestic Product - Total" & BOI$Measure[i] == "Index 2015=100, s.a.") BOI$var[i] <- "Real GDP, s.a"
  if(BOI$Subject[i] == "Consumer Price Index > All items > Total > Total" & BOI$Measure[i] == "Index 2015=100") BOI$var[i] <- "CPI"
  if(BOI$Subject[i] == "Consumer Price Index > All items > Total > Total" & BOI$Measure[i] == "Index 2015=100, s.a.") BOI$var[i] <- "CPI, s.a"
  if(BOI$Subject[i] == "National Accounts > GDP by Expenditure > Current Prices > Gross Fixed Capital Formation" & BOI$Measure[i] == "Level, rate or national currency, s.a.") BOI$var[i] <- "Nominal GFCF, s.a"
  if(BOI$Subject[i] == "National Accounts > GDP by Expenditure > Current Prices > Gross Domestic Product - Total" & BOI$Measure[i] ==  "Level, rate or national currency, s.a.") BOI$var[i] <- "Nominal GDP, s.a"
  if(BOI$Subject[i] == "National Accounts > National Accounts Deflators > Gross Domestic Product > GDP Deflator" & BOI$Measure[i] == "Index 2015=100, s.a.") BOI$var[i] <- "GDP Deflator, s.a"
  if(BOI$Subject[i] == "National Accounts > GDP by Expenditure > Constant Prices > Gross Fixed Capital Formation" & BOI$Measure[i] == "Index 2015=100, s.a.") BOI$var[i] <- "Real GFCF, s.a"
  if(BOI$Subject[i] == "Consumer Price Index > Housing, water, electricity, gas and other fuels (COICOP 04) > Actual rentals for housing > Total" & BOI$Measure[i] == "Index 2015=100") BOI$var[i] <- "CPI: Actual rentals for housing"
  if(BOI$Subject[i] == "Consumer Price Index > Housing, water, electricity, gas and other fuels (COICOP 04) > Imputed rentals for housing > Total" & BOI$Measure[i] == "Index 2015=100") BOI$var[i] <- "CPI: Imputed rentals for housing"
}
sum(is.na(BOI$var))
#Works

#Theres a few breaks in the series, may have to further investigate later, again mostly in lithuani

#Widen this shit
colnames(BOI)
BOI <- select(BOI, ï..LOCATION, Country, TIME, Value, var)
BOI <- pivot_wider(BOI,
                   names_from = var,
                   values_from = Value)

BOI <- arrange(BOI, Country, TIME)

BOI$year = as.numeric(str_sub(BOI$TIME, 1, 4))
BOI$qtr  = as.numeric(str_sub(BOI$TIME, 7, 7))
BOI$yqtr = BOI$year + (BOI$qtr-1) / 4

#Check how it works
count <- rep(NA, ncol(BOI))
miss <- subset(BOI,
               year >= 2000 & year <= 2018)
names <- colnames(miss)
for(i in 1:ncol(miss)) {
  count[i] = sum(is.na(miss[[i]]))
}
cbind(names,count) 
#More data in this one for whatever reason, bizarre stuff altogether

#Get rid of CPI, s.a and CPI Imputed rentals for housing and Actual rentals, missing data and don't need for the time being
BOI <- select(BOI, -`CPI, s.a`, -`CPI: Actual rentals for housing`, -`CPI: Imputed rentals for housing`)

#Take the values out of the econ that I want
useful <- select(econ,
                 `ï..LOCATION`, `TIME`, `Country`, `Residential Investment, Nominal`, `Real House Prices`, `Nominal House Prices`)
#No res investment for belgium
BOI <- merge.data.frame(BOI, useful, all = T)

#Generate a t for every quarter
BOI$t <- NA
for(i in unique(BOI$Country)) {
    tem <- which(BOI$Country == i)
    nah <- 1:length(tem)
    lis <- matrix(c(tem,nah),nrow = length(tem), ncol = 2)
    BOI$t[lis[,1]] <- lis[,2]
}
library(plm)
pboi <- pdata.frame(BOI, index = c("t","Country"))

#Reorder the columns
BOI <- BOI[,c(1,2,3,11,12,13,17,4,5,6,7,8,9,10,14,15,16)]

#Normalise everything so its in 100 = 2015
ind2015 <- subset(BOI,
                  yqtr == 2015.00)
BOI2015 <- BOI
country <- NULL
BOI2015[,8:ncol(BOI)] <- NA
for(i in 1:nrow(BOI)) {
  country <- subset(ind2015,
                    Country == BOI2015[i,2])
  BOI2015[i,8:16] <- (BOI[i,8:16]*100) / country[,8:16]
}

#BIS Nominal small excel file
BIS <- subset(BIS,
              Reference.area %in% eurozone)
rownames(BIS) <- NULL
BIS <- select(BIS, -1, -2, -5, -6, -7, -8, -9)
BIS <- pivot_longer(BIS, 3:119)
colnames(BIS) <- c("ï..LOCATION", "Country", "yearqtr","BIS Nominal House Price")

BIS$year = as.numeric(str_sub(BIS$yearqtr, 2, 5))
BIS$qtr  = as.numeric(str_sub(BIS$yearqtr, 8, 8))
BIS$yqtr = BIS$year + (BIS$qtr-1) / 4

miss <- subset(BIS,
               year >= 1999 &
               year <= 2018)
miss <- subset(miss,
               is.na(miss$`BIS Nominal House Price`))

unique(miss$Country)
#Missing Slovenia Estonia Greece Luxembourg Malta Portgual Slavak Republic Austria Latvia
miss1 <- subset(BOI,
                year >= 1999 &
               year <= 2018)
miss1 <- subset(miss1,
                is.na(miss1$`Nominal House Prices`))
unique(miss1$Country)


#ECB Balance
BAL$DATE <- as.Date(BAL$DATE)

#Getting the quarterly levels of each one, just the value every 3 months
ECB <- matrix(rep(NA,560),nrow = 280, 2)
for (i in 1:nrow(BAL)) {
  if(i %% 3 == 1) {
    num <- (i%/%3)+1
    ECB[(num,2] <- BAL[i,2]
  }
}