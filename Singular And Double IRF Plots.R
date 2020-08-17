load(file = "~/Thesis/Data/All IRFs.Rdata")
#Options
source("~/Thesis/R Code/Steps, Conf, Runs.R")

mainfolder <- "~/Thesis/FiguresandGraphs/"
subfolders <- c(main = "main", exog = "exog", pre =  "pre", post =  "post", 
                maxreact = "maxreact", minreact = "minreact", north = "north", 
                south = "south", nogreece = "nogreece", noireland = "noireland", 
                assets = "assets")
all(names(subfolders) == names(allirfs))
folders <- paste(mainfolder, subfolders, sep = "")
novars <- length(folders)
savelocation <- list()
for(i in 1:(novars-1)) {
  savelocation[[i]] <- paste(folders[i], "/", var.names.main, ".eps", sep = "")
}
savelocation[[novars]] <- paste(folders[novars], "/", var.names.assets, ".eps", sep = "")

for(j in 1:(novars-1)) {
  for(i in 1:length(var.names.main)) {
    setEPS()
    postscript(savelocation[[j]][i])
    #png(savelocation[[j]][i])
    bazplotirf(allirfs[[j]][[i]], plot.type = "multiple", ylab = var.names.main.graph, legendbot = "Impulse Response", lwd = 1.5)
    dev.off()
  }
}
for(i in 1:length(var.names.assets)) {
  #png(savelocation[[novars]][[i]])
  setEPS()
  postscript(savelocation[[novars]][i])
  bazplotirf(allirfs[[novars]][[i]], plot.type = "multiple", ylab = var.names.assets.graph, legendbot = "Impulse Response", lwd = 1.5)
  dev.off()
}

#####################DOUBLE GRAPHS############################################

#Pre and post
maintitleprepost <- paste("Orthogonal Impulse Response for Pre and Post Crisis VARs from", var.names.main.graph, sep = " ")
savelocation.prepost <- paste("~/Thesis/FiguresandGraphs/Double/PreandPost/", var.names.main, ".eps", sep = "")
for(i in 1:length(var.names.main)) {
  # png(savelocation.prepost[i])
  setEPS()
  postscript(savelocation.prepost[i])
  bazplotirf.double(irf.withci =  allirfs[["pre"]][[i]], irf.noci = allirfs[["post"]][[i]], plot.type = "multiple", 
                    ylab = var.names.main.graph, main = maintitleprepost[i],
                    legendbot = c("Impulse Response for Pre Crisis VAR", "Impulse Response for Post Crisis VAR"))
  dev.off()
}

#Max and min
maintitlemaxmin <-   paste("Orthogonal Impulse Response for Strong and Weak Reaction Groups from", var.names.main.graph, sep = " ")
savelocation.maxmin <- paste("~/Thesis/FiguresandGraphs/Double/MaxandMin/m", var.names.main, ".eps", sep = "")
for(i in 1:length(var.names.main)) {
  #png(savelocation.maxmin[i])
  setEPS()
  postscript(savelocation.maxmin[i])
  bazplotirf.double(irf.withci = allirfs[["maxreact"]][[i]], irf.noci = allirfs[["minreact"]][[i]], plot.type = "multiple", 
                    ylab = var.names.main.graph, 
                    legendbot = c("Impulse Response for Strong Reaction Group", "Impulse Response for Weak Reaction Group"), 
                    main = maintitlemaxmin[i])
  dev.off()
}

#North and South
savelocation.northsouth <- paste("~/Thesis/FiguresandGraphs/Double/NorthandSouth/n", var.names.main, ".eps", sep = "")
maintitlenorthsouth <-  paste("Orthogonal Impulse Response for Northern and Southern Groups from", var.names.main.graph, sep = " ")

for(i in 1:length(var.names.main)) {
  #png(savelocation.northsouth[i])
  setEPS()
  postscript(savelocation.northsouth[i])
  bazplotirf.double(irf.withci = allirfs[["north"]][[i]], irf.noci = allirfs[["south"]][[i]], plot.type = "multiple", 
                    ylab = var.names.main.graph, 
                    legendbot = c("Impulse Response for Northern Group", "Impulse Response for Southern Group"), 
                    main = maintitlenorthsouth[i])
  dev.off()
}

min(allirfs$maxreact$int$irf$demeaned_int[,1])
min(allirfs$minreact$int$irf$demeaned_int[,1])
##################################################################################
#load(file = "~/Thesis/Data/All IRFs.Rdata")

#tmp <- bazirf.varest(fevar.large.reaction, n.ahead = steps, impulse = "demeaned_int", ortho = T, ci = conf, runs = runs, seed = 253)
#temp <- bazirf.varest(fevar.small.reaction, n.ahead = steps, impulse = "demeaned_int", ortho = T, ci = conf, runs = runs, seed = 253)
#bazplotirf(tmp, plot.type = "multiple", ylab = var.names.fancy.main)
#bazplotirf(temp, plot.type = "multiple", ylab = var.names.fancy.main)