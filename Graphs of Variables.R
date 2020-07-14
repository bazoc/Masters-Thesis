rm(list = ls())
setwd("~/Thesis")
VAR2 <- read.csv("Data/VAR2.csv")

library(tidyr)
library(ggplot2)

Aus <- data$Austria
for(i in 1:length(countries)) {
  plot()
}
for(i in 1:ncol(data[[1]])) {
  plot(data[[countries[1]]][,i],
       ylab = var_names_fancy[i])
}
plot.ts(Aus)
plot(Cleaned.Data$Shadow.Policy.Rate)
plot(Aus[,2])
data[[1]]
colnames(Aus[,1])
data[[countries[1]]