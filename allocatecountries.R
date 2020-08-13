library(plyr)
library(ggplot2)
library(panelvar)
library(PEIP)
library(tolerance)

temp <- bazirf.varest(fevar.main, impulse = "demeaned_int", n.ahead = 50, ortho = T, ci = .95, runs = 1000, seed = 253)
bazplotirf(temp, plot.type = "multiple", legendbot = "Impulse Response", lwd = 1.5)

#Numbet of periods ahead to assess at
q = 12

allqs <- list()
for(i in c(4,8,12,16,20,24,28)) {
q = 24
minncountries <- 3 #The minimum number of countries in a group
#10 countries, possibly up to ~14
#Minimum 3 countries in each group
#(3,7) (4,6) (5,5)
enum.choose <- function(x, k) {
  if(k > length(x)) stop('k > length(x)')
  if(choose(length(x), k)==1){
    list(as.vector(combn(x, k)))
  } else {
    cbn <- combn(x, k)
    lapply(seq(ncol(cbn)), function(i) cbn[,i])
  }
}

nocount <- length(countries)
no3 <- 1:choose(nocount,3)
no4 <- (max(no3) + 1): (max(no3) + choose(nocount,4))
no5 <- (max(no4) + 1): (max(no4) + choose(nocount,5)/2)

totcombos34 <- c(enum.choose(countries, 3), enum.choose(countries, 4))
totcombos5 <- enum.choose(countries, 5)
allreadyused <- list()
count1 = 0
repeatindex <- vector("logical", length = choose(nocount, 5))
#Get rid of all the inverse groups in 10 choose 5
for(i in 1:choose(nocount,5)) {
  othercountries <- countries[!(countries %in% totcombos5[[i]])]
  if(totcombos5[i] %in% allreadyused) {
    repeatindex[i] <- 1
  }
  otherlist <- list(othercountries)
  allreadyused <- c(allreadyused, otherlist)
} 
#Starts repeating after half way
totcombos <- c(totcombos34, totcombos5[1:(252/2)])

dtrad <- vector()
d <- vector()
N1T <- vector()
N2T <- vector()
distancemeasure <- matrix(NA, nrow = length(totcombos), ncol = 8)


for(i in 1:length(totcombos)) {
  current.countries1 <- totcombos[[i]]
  index <- !(countries %in% totcombos[[i]])
  current.countries2 <- countries[index]
  
  current.data1 <- dplyr::filter(main.panel,
                            `Country` %in% current.countries1)
  current.data2 <- dplyr::filter(main.panel,
                                 `Country` %in% current.countries2)
  
  current.model1 <- pvarfeols(var.names.main, lags = laglen, data = current.data1, panel_identifier = c("Country", "yqtr"))
  current.model2 <- pvarfeols(var.names.main, lags = laglen, data = current.data2, panel_identifier = c("Country", "yqtr"))
  
  current.irf1 <- oirf(current.model1, n.ahead = q)
  current.irf2 <- oirf(current.model2, n.ahead = q)
  
  current.irf1n <- current.irf1
  current.irf2n <- current.irf2
  
  current.irf1n$int <- current.irf1$int / current.irf1$int[1,"int"]
  current.irf2n$int <- current.irf2$int / current.irf2$int[1,"int"]
  
  s1 <- current.irf1n[["int"]][,"lhou"]
  s2 <- current.irf2n[["int"]][,"lhou"]
  
  S1 <- sum(s1)
  S2 <- sum(s2)
  
  distancemeasure[i,1] <- abs(sum((s1-s2)))
  distancemeasure[i,2] <- i
  distancemeasure[i,3] <- S1
  distancemeasure[i,4] <- S2

  
  #Number of variables in each model times
  N1T[i] <- nrow(current.model1$Set_Vars)
  N2T[i] <- nrow(current.model2$Set_Vars)
  
  if(abs(S1) > abs(S2)) distancemeasure[i,5] = 0
  else distancemeasure[i,5] = 1
  distancemeasure[i,6] <- length(current.countries1)
  d[i] <- abs(S1 - S2)
  
}

dist_variance <-var(distancemeasure[,1])
for(i in 1:length(totcombos)) {
  distancemeasure[i,7] <- dist_variance/((1/distancemeasure[i,6])+1/(length(countries) - distancemeasure[i,6]))
  distancemeasure[i,8] <- distancemeasure[i,1] / (sqrt(distancemeasure[i,7]/distancemeasure[i,6] + distancemeasure[i,7]/(length(countries) - distancemeasure[i,6])))
}

distancemeasure_sorted <- distancemeasure[order(distancemeasure[,8]),]

critical_value <- tinv(.95, length(countries)- 2) #Inverse t distribution, lose 2 degrees of freedom due to no. oif groups
ngrouping <- sum(distancemeasure_sorted[,8] > critical_value)

max_countries <- vector()
for(i in (nrow(distancemeasure)-ngrouping+1):nrow(distancemeasure)) {
  if(distancemeasure_sorted[i,5] == 0) { #If the country group are ther bigger effect
    max_countries <- c(max_countries, totcombos[distancemeasure_sorted[i,2]][[1]])
  }
  else { #If the country group is the smaller effects
    notmaxcountries <- totcombos[distancemeasure_sorted[i,2]][[1]]
    index <- !(countries %in% notmaxcountries)
    max_countries <- c(max_countries, countries[index])
  }
}

sorttable <-table(max_countries)

M= 0
for(N1 in minncountries:(length(countries)-minncountries)) {
  M <- M + choose(length(countries),N1)
}
K <- M/2
quants <- qhyper(p = c(.05, .95), m = K, n = K, k = sum(ngrouping))

countfreq <- as.matrix(sorttable)
countorder <- rownames(countfreq)
countorder <- countorder[order(countfreq)]
small.reaction.group <- countorder[1:5]
large.reaction.group <- countorder[6:10]
carq <- paste(q)
allqs[[carq]] <- list(quants = quants, sorttable = sorttable, countfreq = countfreq, small.reaction.group = small.reaction.group, large.reaction.group = large.reaction.group)
}


sort(allqs$`4`$large.reaction.group)
sort(allqs$`8`$large.reaction.group)
sort(allqs$`12`$large.reaction.group)
sort(allqs$`16`$large.reaction.group)
sort(allqs$`20`$large.reaction.group)
sort(allqs$`24`$large.reaction.group)
sort(allqs$`28`$large.reaction.group)

save(allqs , file = "~/Thesis/Data/Sorting result.Rdata")