library(plyr)
library(ggplot2)
#Numbet of periods ahead to assess at
q = 4

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
no4 <- (max(no3)+1):(max(no3) + choose(nocount,4))
no5 <- (max(no4) + 1): (max(no4) + choose(nocount,5))


totcombos <- c(enum.choose(countries, 3), enum.choose(countries, 4), enum.choose(countries, 5))
dtrad <- vector()
d <- vector()
N1T <- vector()
N2T <- vector()
distancemeasure <- matrix(NA, nrow = length(totcombos), ncol = 3)


for(i in 1:length(totcombos)) {
  current.countries1 <- totcombos[[i]]
  index <- !(countries %in% totcombos[[i]])
  current.countries2 <- countries[index]
  
  current.data1 <- dplyr::filter(pan,
                            `Country` %in% current.countries1)
  current.data2 <- dplyr::filter(pan,
                                 `Country` %in% current.countries2)
  
  current.model1 <- pvarfeols(var_names, lags = laglen, data = current.data1, panel_identifier = c("Country", "yqtr"))
  current.model2 <- pvarfeols(var_names, lags = laglen, data = current.data2, panel_identifier = c("Country", "yqtr"))
  
  current.irf1 <- oirf(current.model1, n.ahead = q)
  current.irf2 <- oirf(current.model2, n.ahead = q)
  
  s1 <- current.irf1[["int"]][,"lhou"]
  s2 <- current.irf2[["int"]][,"lhou"]
  
  S1 <- sum(s1)
  S2 <- sum(s2)
  
  dtrad[i] <- sum(abs(s1-s2))
  d[i] <- abs(S1 - S2)
  
  
  #Number of variables in each model times
  N1T[i] <- nrow(current.model1$Set_Vars)
  N2T[i] <- nrow(current.model2$Set_Vars)
  
}

variance.d <-var(d)
var3 <- var(d[no3])
var4 <- var(d[no4])
var5 <- var(d[no5])

var31 <- var(dtrad[no3])
var41 <- var(dtrad[no4])
var51 <- var(dtrad[no5])

sigma3 <- var3*(((1/N1T[min(no3)]) + (1/N2T[min(no3)]))^-1)
sigma4 <- var4*(((1/N1T[min(no4)]) + (1/N2T[min(no4)]))^-1)
sigma5 <- var5*(((1/N1T[min(no5)]) + (1/N2T[min(no5)]))^-1)

sigma31 <- var31*(((1/N1T[min(no3)]) + (1/N2T[min(no3)]))^-1)
sigma41 <- var41*(((1/N1T[min(no4)]) + (1/N2T[min(no4)]))^-1)
sigma51 <- var51*(((1/N1T[min(no5)]) + (1/N2T[min(no5)]))^-1)

sigma <- (sigma3 + sigma4 + sigma5) /3
sigma1 <- (sigma31 + sigma41 + sigma51) /3
stat <- qnorm(c(.975) , mean = 0 , sd = sqrt(sigma))
outliers <- dtrad >= stat

totcombos[c(which(d == min(d)))]
distscore
for(i in length(countries)) {
  temp <- countries[i]
  count.countries <- temp %in% totcombos
  distscore
}