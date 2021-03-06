rm(list = ls())
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
BOI <- read.csv("Original data/OECD Main Economic Indicators Original.csv", na.strings=c("","NA"))
BIS <- read.csv("Original data/BIS Property Prices Nominal.csv", na.strings=c("","NA"))
BAL <- read.csv("Original data/ECB Total assets - Internal Liquidity Management.csv", na.strings=c("","NA"), header = F, skip = 4)
VOL <- read.csv("Original data/Bloomberg VSTOXX index.csv", na.strings=c("-","NA"), header = T)
SRT <- read.csv("Original data/Bloomberg Shadow policy rate Original.csv")
GNI <- read.csv("Original data/OECD GNI Quarterly national accounts original.csv", na.strings = c("", "NA"))

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
econ <- dplyr::select(econ, ï..LOCATION, Country, TIME, Value, var)
#Make it wider
econ <- pivot_wider(econ,
                    names_from = var,
                    values_from = Value)

econ <- arrange(econ, Country, Time)


#Make a year and quarter variable
econ$year = as.numeric(str_sub(econ$TIME, 1, 4))
econ$qtr  = as.numeric(str_sub(econ$TIME, 7, 7))
econ$yqtr = econ$year + (econ$qtr) / 4

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
RHO <- dplyr::select(RHO, "ï..LOCATION", "TIME", "Real House Prices")
NHO <- dplyr::select(NHO, "ï..LOCATION", "TIME", "Nominal House Prices")

#Merge them
house <- merge.data.frame(RHO, NHO, all = T)

#Merge them with the econ one

econ <- merge.data.frame(econ, house, all = T)


#Probably gonna be between 2000 and 2018, check how many variables missing in this timeframe
#count <- rep(NA, ncol(econ))
miss <- subset(econ,
               year >= 2000 & year <= 2018)
names <- colnames(miss)
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
#            is.na(miss$`Residential Investment, Nominal s,a`))
#y <- dplyr::select(y, Country, TIME)


#Residential investment is missing Belgium

#CPI Housing missing alot of stuff
#y <- subset(miss,
#            is.na(miss$`CPI: Housing excluding imputed rentals for housing`))
#y <- dplyr::select(y, Country, TIME)
#Definitely can't use this for housing

#Other housing missing 228
#y <- subset(miss,
#            is.na(miss$`Real House Prices`))
#y <- dplyr::select(y, Country, TIME)
#unique(y$Country)
#Missing estonia lithuania luxembours latvia slovenia
#Not major economies so really not the biggest deal
#
#y <- subset(miss,
#is.na(miss$`Nominal House Prices`))
#y <- dplyr::select(y, Country, TIME)
#unique(y$Country)
#Same as the other one


#The columns we are getting rid of
#econ <- dplyr::select(econ, -`CPI Imputed rentals for housing`, -`CPI: All items, s.a`, -`GFCF, Nominal s.a`, -`Residential Investment, Nominal s,a`, -`GDP, Nominal s.a`, -`CPI: Housing`, -`CPI: Housing excluding imputed rentals for housing`)


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
BOI <- dplyr::select(BOI, ï..LOCATION, Country, TIME, Value, var)
BOI <- pivot_wider(BOI,
                   names_from = var,
                   values_from = Value)

BOI <- arrange(BOI, Country, TIME)

BOI$year = as.numeric(str_sub(BOI$TIME, 1, 4))
BOI$qtr  = as.numeric(str_sub(BOI$TIME, 7, 7))
BOI$yqtr = BOI$year + (BOI$qtr) / 4

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
BOI <- dplyr::select(BOI, -`CPI, s.a`, -`CPI: Actual rentals for housing`, -`CPI: Imputed rentals for housing`)

#Take the values out of the econ that I want
useful <- dplyr::select(econ,
                 `ï..LOCATION`, `TIME`, `Country`, `Residential Investment, Nominal s,a`, `Real House Prices`, `Nominal House Prices`)
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

#Make Res investment  real
BOI$`Residential Investment, Real s.a` <- BOI$`Residential Investment, Nominal s,a` / BOI$`GDP Deflator, s.a`


#Reorder the columns
BOI <- BOI[,c(1,2,3,11,12,13,17,4,5,6,7,8,9,10,14,15,16,18)]

#Normalise everything so its in 100 = 2015
ind2015 <- subset(BOI,
                  yqtr == 2015.25)
BOI2015 <- BOI
country <- NULL
BOI2015[,8:ncol(BOI)] <- NA
for(i in 1:nrow(BOI)) {
  country <- subset(ind2015,
                    Country == BOI2015[i,2])
  BOI2015[i,8:18] <- (BOI[i,8:18]*100) / country[,8:18]
}

BOI$`Residential Investment, Real s.a` <- BOI2015$`Residential Investment, Real s.a`

#BIS Nominal small excel file
BIS <- subset(BIS,
              Reference.area %in% eurozone)
rownames(BIS) <- NULL
BIS <- dplyr::select(BIS, -1, -2, -5, -6, -7, -8, -9)
BIS <- pivot_longer(BIS, 3:119)
colnames(BIS) <- c("ï..LOCATION", "Country", "yearqtr","BIS Nominal House Price")

BIS$year = as.numeric(str_sub(BIS$yearqtr, 2, 5))
BIS$qtr  = as.numeric(str_sub(BIS$yearqtr, 8, 8))
BIS$yqtr = BIS$year + (BIS$qtr) / 4

miss <- subset(BIS,
               year >= 2000 &
               year <= 2018)
miss <- subset(miss,
               is.na(miss$`BIS Nominal House Price`))

unique(miss$Country)
#Missing Slovenia Estonia Greece Luxembourg Malta Portgual Slavak Republic Austria Latvia
miss1 <- subset(BOI,
                year >= 2000 &
                year <= 2018)
miss1 <- subset(miss1,
                is.na(miss1$`Nominal House Prices`))
unique(miss1$Country)

miss2 <- subset(BOI,
                year >= 2000 &
                year <= 2018)
miss2 <- subset(miss2,
                is.na(miss2$`Real House Prices`))
unique(miss2$Country)


#ECB Balance sheet
#Give Column names
colnames(BAL) <- c("date","Month","ECB Assets")
#Drop the monthly observations so we are left with quarterly
BAL <- subset(BAL,
              is.na(BAL$`ECB Assets`) == F)

#Make time like the others
BAL$yqtr <- seq(from = 2020.5, to = 1999, by = -.25)

#drop not needed columns
BAL <- dplyr::select(BAL, -Month, -date)
BAL <-BAL[order(BAL$yqtr),]

#Volatility index
#Only care about the price
rownames(VOL) <- NULL


#Make the yqtr 
VOL$yqtr <- seq(from = 1999.75, to = 2020.25, by = .25)

#select the columns i want and change the names
VOL <- dplyr::select(VOL, VSTOXX.Index, yqtr)
colnames(VOL) <- c("Volatility", "yqtr")

MON <- merge.data.frame(BAL,VOL, ALL = TRUE)

#Add the Shadow rates
SRT$yqtr <-  seq(from = 2000.25, to = 2020.25, by = .25)
SRT <- dplyr::select(SRT, Shadow.Policy.Rate, yqtr)
#Drop all the ones i don't need

#Merge them
MON <- merge.data.frame(MON,SRT, ALL = TRUE)


#Merge Monetary policy and the main one
Cleaned.Data <- merge.data.frame(BOI, MON, All = T)

#GNI
#GNI <- read.csv("Original data/OECD GNI Quarterly national accounts original.csv", na.strings = c("", "NA"))

#GNI <- filter(GNI,
#              Country %in% eurozone)
#unique(GNI$Country)
#unique(GNI$Measure)
#
#GNI <- filter(GNI,
#              Measure == "National currency, current prices, quarterly levels")
#miss <- filter(GNI,
#               is.na(GNI$Value))
#unique(miss$Country)
#namesgni <- colnames(GNI) 
#namesgni[17] <- "Nominal GNI"
#colnames(GNI) <- namesgni
#GNI <- dplyr::select(GNI, Country, TIME, `Nominal GNI`)
#GNI$year = as.numeric(str_sub(GNI$TIME, 1, 4))
#GNI$qtr  = as.numeric(str_sub(GNI$TIME, 7, 7))
#GNI$yqtr = GNI$year + (GNI$qtr) / 4
#
#Cleaned.Data <- merge(GNI, Cleaned.Data, ALL = T)
#Cleaned.Data$`Real GNI`  <- Cleaned.Data$`Nominal GNI`/Cleaned.Data$`GDP Deflator, s.a`
#for(i in 1:nrow(Cleaned.Data)) {
#  country <- filter(Cleaned.Data,
#                    `Country` == Cleaned.Data[i,"Country"],
#                    `yqtr` == 2015.25)
#  Cleaned.Data[i,"Real GNI"] <- (Cleaned.Data[i,"Real GNI"]*100) / country[,"Real GNI"]
#}


rm(list = c("BAL","country","miss","miss1","RHO","NHO","sm","meas","meas1","CPI","QNA","names",
            "count","useful","VOL","house","ind2015", "GNI"))

#Saving for orhter r things
write.csv(Cleaned.Data, file = "Data/CleanedData.csv")