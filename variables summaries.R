tmp <- seq(from = 2006.5, to = 2017, by = .25)
recessionyears <- filter(notlogged.panel, 
                         yqtr %in% tmp)
notgreece <- filter(notlogged.panel, Country != "Greece")
onecountry <- filter(notlogged.panel, Country == "Austria")


#GDP
summary(notlogged.panel$gdp)
by(notlogged.panel$gdp, notlogged.panel$Country, summary)

rangegdp <- by(notlogged.panel$gdp, notlogged.panel$Country, range)
vargdp <- by(notlogged.panel$gdp, notlogged.panel$Country, var)
min(var)
distgdp <- list()
for(i in 1:10) {
  distgdp[[countries[i]]] <- range1[[countries[i]]][[2]] - range1[[countries[i]]][[1]]
}

max(gdp)

avegdp <- by(notlogged.panel$gdp, notlogged.panel$yqtr, ave)
(avegdp[[80]][[1]] - avegdp[[1]][[1]])/ave[[1]][[1]] 

#House Prices
summary(notlogged.panel$hou)
by(notlogged.panel$hou, notlogged.panel$Country, summary)

rangehou <- by(notlogged.panel$hou, notlogged.panel$Country, range)
varhou <- by(notlogged.panel$hou, notlogged.panel$Country, var)
min(varhou)
max(varhou)
disthou <- list()
for(i in 1:10) {
  disthou[[countries[i]]] <- range1[[countries[i]]][[2]] - range1[[countries[i]]][[1]]
}

max(disthou)

avehou <- by(notlogged.panel$hou, notlogged.panel$yqtr, ave)
(avehou[[80]][[1]] - avehou[[1]][[1]])/avehou[[1]][[1]] 
rechou <- dplyr::select(recessionyears, Country, yqtr, hou) 
rechou <- pivot_wider(rechou, names_from = Country, values_from = hou)

apply(rechou, 2, min)


#Residential Investment
summary(notlogged.panel$res)
by(notlogged.panel$res, notlogged.panel$Country, summary)

rangeres <- by(notlogged.panel$res, notlogged.panel$Country, range)
varres <- by(notlogged.panel$res, notlogged.panel$Country, var)
min(varres)
max(varres)
distres <- list()
for(i in 1:10) {
  distres[[countries[i]]] <- range1[[countries[i]]][[2]] - range1[[countries[i]]][[1]]
}

max(distres)

averes <- by(notlogged.panel$res, notlogged.panel$yqtr, ave)
(averes[[80]][[1]] - averes[[1]][[1]])/averes[[1]][[1]] 

averesnog <- by(notgreece$res, notgreece$yqtr, ave)

recres <- dplyr::select(recessionyears, Country, yqtr, res) 
recres <- pivot_wider(recres, names_from = Country, values_from = res)


#Deflator
summary(notlogged.panel$def)
by(notlogged.panel$def, notlogged.panel$Country, summary)

rangedef <- by(notlogged.panel$def, notlogged.panel$Country, range)
vardef <- by(notlogged.panel$def, notlogged.panel$Country, var)
min(vardef)
max(vardef)
distdef <- list()
for(i in 1:10) {
  distdef[[countries[i]]] <- range1[[countries[i]]][[2]] - range1[[countries[i]]][[1]]
}

max(distdef)

avedef <- by(notlogged.panel$def, notlogged.panel$yqtr, ave)
(avedef[[80]][[1]] - avedef[[1]][[1]])/avedef[[1]][[1]] 

avedefnog <- by(notgreece$def, notgreece$yqtr, ave)

recdef <- dplyr::select(recessionyears, Country, yqtr, def) 
recdef <- pivot_wider(recdef, names_from = Country, values_from = def)

#Shadow Rate
summary(onecountry$int)
onecountry[,c("yqtr", "int")]

#CB Assets
onecountry[,c("yqtr", "ass")]


#Volatility
