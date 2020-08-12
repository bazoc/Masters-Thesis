source("~/Thesis/R Code/setup.R")
source("~/Thesis/R Code/FEVARs.R")
load(file = "~/Thesis/Data/All IRFs.Rdata")

#Options
source("~/Thesis/R Code/Steps, Conf, Runs.R")


#Have to save each graph individually
imp1 <- paste("demeaned_", var.names.main, sep = "")
imp2 <- paste("demeaned_", var.names.assets, sep = "")

irf.main.ortho.1 <- list()
irf.exog.ortho.1 <- list()
irf.pre.ortho <- list()
irf.post.ortho <- list()
irf.assets.ortho <- list()
irf.maxreaction.ortho <- list()
irf.minreaction.ortho <- list()
irf.north.ortho <- list()
irf.south.ortho <- list()
irf.nogreece.ortho <- list()
irf.noireland.ortho <- list()


for(i in 1:5) {
  irf.main.ortho.1[[var.names.main[i]]] <- bazirf.varest(fevar.main, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 253)
  irf.exog.ortho.1[[var.names.main[i]]] <- bazirf.varest(fevar.exog, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 7084)
  irf.pre.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.pre, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 405)
  irf.post.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.post, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 195)
  irf.maxreaction.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.large.reaction, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 7540)
  irf.minreaction.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.small.reaction, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 3047)
  irf.north.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.north, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 129)
  irf.south.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.south, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 3524)
  irf.nogreece.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.nogreece, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 4749)
  irf.noireland.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.noireland, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 2015)
}
for(i in 1:length(assets.panel)) {
  irf.assets.ortho[[var.names.assets[i]]] <- bazirf.varest(fevar.assets, impulse = imp2[i] ,n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 678)
}


allirfs <- list(main = irf.main.ortho.1, exog = irf.exog.ortho.1, pre = irf.pre.ortho,post = irf.post.ortho, 
                maxreact = irf.maxreaction.ortho, minreact = irf.minreaction.ortho, north = irf.north.ortho,
                south = irf.south.ortho, nogreece = irf.nogreece.ortho, noireland = irf.noireland.ortho, 
                assets = irf.assets.ortho)

mainfolder <- "~/Thesis/Figures and Graphs/"
subfolders <- c(main = "main irf", exog = "exog irf", pre =  "pre irf", post =  "post irf", 
                maxreact = "max react", minreact = "min react", north = "north", 
                south = "south", nogreece = "no greece", noireland = "no ireland", 
                assets = "assets irf")
all(names(subfolders) == names(allirfs))
folders <- paste(mainfolder, subfolders, ".png", sep = "")
novars <- length(folders)
savelocation <- list()
for(i in 1:(novars-1)) {
  savelocation[[i]] <- paste(folders[i], "/", var.names.main, sep = "")
}
savelocation[[novars]] <- paste(folders[novars], "/", var.names.assets, ".png", sep = "")

for(j in 1:(novars-1)) {
  for(i in 1:length(var.names.main)) {
    png(savelocation[[j]][i])
    bazplotirf(allirfs[[j]][[i]], plot.type = "multiple", ylab = var.names.main.graph, legendbot = "Impulse Response")
    dev.off()
  }
}
for(i in 1:length(var.names.assets)) {
  png(savelocation[[novars]][[i]])
  bazplotirf(allirfs[[novars]][[i]], plot.type = "multiple", ylab = var.names.assets.graph, legendbot = "Impulse Response")
  dev.off()
}

#Save it so we can use it in double
save(allirfs, file = "~/Thesis/Data/All IRFs.Rdata")

#####################DOUBLE GRAPHS############################################

#Pre and post
maintitleprepost <- paste("Orthogonal Impulse Response for Pre and Post Crisis VARs from", var.names.main.graph, sep = " ")
savelocation.prepost <- paste("~/Thesis/Figures and Graphs/Double/Pre and Post/", var.names.main, ".png", sep = "")
for(i in 1:length(var.names.main)) {
  png(savelocation.prepost[i])
  bazplotirf.double(irf.withci =  allirfs[["pre"]][[i]], irf.noci = allirfs[["post"]][[i]], plot.type = "multiple", 
                    ylab = var.names.main.graph, main = maintitleprepost[i],
                    legendbot = c("Impulse Response for Pre Crisis VAR", "Impulse Response for Post Crisis VAR",
                                  "95% Boorstrapped C.I. - 1000 Runs for Pre Crisis VAR"))
  dev.off()
}

#Max and min
maintitlemaxmin <-   paste("Orthogonal Impulse Response for Strong and Weak Reaction Groups from", var.names.main.graph, sep = " ")
savelocation.maxmin <- paste("~/Thesis/Figures and Graphs/Double/Max and Min/", var.names.main, ".png", sep = "")
for(i in 1:length(var.names.main)) {
  png(savelocation.maxmin[i])
  bazplotirf.double(irf.withci = allirfs[["maxreact"]][[i]], irf.noci = allirfs[["minreact"]][[i]], plot.type = "multiple", 
                    ylab = var.names.main.graph, 
                    legendbot = c("Impulse Response for Strong Reaction Group", "Impulse Response for Weak Reaction Group",
                                  "95% Boorstrapped C.I. - 1000 Runs for Strong Reaction Group"), 
                    main = maintitlemaxmin[i])
  dev.off()
}

#North and South
savelocation.northsouth <- paste("~/Thesis/Figures and Graphs/Double/North and South/", var.names.main, ".png", sep = "")
maintitlenorthsouth <-  paste("Orthogonal Impulse Response for Northern and Southern Groups from", var.names.main.graph, sep = " ")

for(i in 1:length(var.names.main)) {
  png(savelocation.northsouth[i])
  bazplotirf.double(irf.withci = allirfs[["north"]][[i]], irf.noci = allirfs[["south"]][[i]], plot.type = "multiple", 
                    ylab = var.names.main.graph, 
                    legendbot = c("Impulse Response for Northern Group", "Impulse Response for Southern Group",
                                  "95% Boorstrapped C.I. - 1000 Runs for Northern Group"), 
                    main = maintitlenorthsouth[i])
  dev.off()
}


##################################################################################
#load(file = "~/Thesis/Data/All IRFs.Rdata")

#tmp <- bazirf.varest(fevar.large.reaction, n.ahead = steps, impulse = "demeaned_int", ortho = T, ci = conf, runs = runs, seed = 253)
#temp <- bazirf.varest(fevar.small.reaction, n.ahead = steps, impulse = "demeaned_int", ortho = T, ci = conf, runs = runs, seed = 253)
#bazplotirf(tmp, plot.type = "multiple", ylab = var.names.fancy.main)
#bazplotirf(temp, plot.type = "multiple", ylab = var.names.fancy.main)