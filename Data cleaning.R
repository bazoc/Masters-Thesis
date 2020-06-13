setwd("~/Thesis")
library(dplyr)
library(tidyr)
library(zoo)
#Reading in two of the data sets
QNA <- read.csv("Original data/Quarterly national accounts original.csv", na.strings=c("","NA"))
CPI <- read.csv("Original data/OECD Prices original.csv", na.strings=c("","NA"))

#Variable with eurozone countries
eurozone <- c("Austria","Belgium","Cyprus","Estonia","Finland","France","Germany","Greece","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Portugal","Slovakia","Slovenia","Spain")

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
econ$TIME
time <- econ$TIME
time <- time[1:100]
time.ts <- ts(time, start = 1990, frequency = 4)
time <- zoo(time)
class(time)
#Probably gonna be between 2000 and 2018, check how many variables missing in this timeframe
subset(econ,
       TIME )

#OECD REAL HOUSE PRICES
RHO <- read.csv("Original data/OECD Real House prices.csv", na.strings=c("","NA"))
NHO <- read.csv("Original data/OECD Nominal House prices.csv", na.strings=c("","NA"))
ts_info(time)
class(econ$Subject)
