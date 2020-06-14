#packages for var in r
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(BVAR)
library(plm)

#Just practicing an Ireland VAR
tempint <- read.csv("Original data/Placeholder interest rate.csv", na.strings=c("","NA"))
sta = 1999
fin = 2010
IRE <- subset(BOI,
              Country == "Ireland" & 
              year >= sta &
              year <= fin)

tempint$year <- as.numeric(str_sub(tempint$DATE, 1, 4))
tempint <- subset(tempint,
                  year >= sta &
                  year <= fin)

IRE <- cbind(IRE, tempint$INTDSREZQ193N)

GDP <- ts(IRE$`Real GDP, s.a`, start = sta, end = fin, frequency = 4)
def <- ts(IRE$`GDP Deflator, s.a`, start = sta, end = fin, frequency = 4)
hou <- ts(IRE$`Real House Prices`, start = sta, end = fin, frequency = 4)
int <- ts(IRE$`tempint$INTDSREZQ193N`, start = sta, end = fin, frequency = 4)

#Make them logs
GDP <- log(GDP)
def <- log(def)
hou <- log(hou)

IRE <- cbind(GDP, def, hou, int)
#We'll do 4 sure why not

#
IreVar <- bvar(IRE, lags = 4)
summary(IreVar)

irf <- irf(IreVar)

plot(irf)