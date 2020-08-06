source("~/Thesis/R Code/setup.R")
source("~/Thesis/R Code/FEVARs.R")


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
for(i in 1:6) {
  irf.assets.ortho[[var.names.assets[i]]] <- bazirf.varest(fevar.assets, impulse = imp2[i] ,n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 678)
}


allirfs <- list(main = irf.main.ortho.1, exog = irf.exog.ortho.1, pre = irf.pre.ortho,post = irf.post.ortho, 
                maxreact = irf.maxreaction.ortho, minreact = irf.minreaction.ortho, north <- irf.north.ortho,
                south <- irf.south.ortho, nogreece <- irf.nogreece.ortho, noireland = irf.noireland.ortho, 
                assets = irf.assets.ortho)

mainfolder <- "~/Thesis/Figures and Graphs/"
subfolders <- c("main irf", "exog irf", "pre irf", "post irf", "max react", "min react", "north", "south", "no greece", "no ireland", "assets irf")
folders <- paste(mainfolder, subfolders, sep = "")
novars <- length(folders)
savelocation <- list()
for(i in 1:(novars-1)) {
  savelocation[[i]] <- paste(folders[i], "/", var.names.main, ".png", sep = "")
}
savelocation[[novars]] <- paste(folders[novars], "/", var.names.assets, ".png", sep = "")

for(j in 1:(novars-1)) {
  for(i in 1:length(var.names.main)) {
    png(savelocation[[j]][i])
    bazplotirf(allirfs[[j]][[i]], plot.type = "multiple", ylab = var.names.fancy.main)
    dev.off()
  }
}
for(i in 1:length(var.names.assets)) {
  png(savelocation[[novars]][[i]])
  bazplotirf(allirfs[[novars]][[i]], plot.type = "multiple", ylab = var.names.fancy.main)
  dev.off()
}

tmp <- bazirf.varest(fevar.large.reaction, n.ahead = steps, impulse = "demeaned_int", ortho = T, ci = conf, runs = runs, seed = 253)
temp <- bazirf.varest(fevar.small.reaction, n.ahead = steps, impulse = "demeaned_int", ortho = T, ci = conf, runs = runs, seed = 253)
bazplotirf(tmp, plot.type = "multiple", ylab = var.names.fancy.main)
bazplotirf(temp, plot.type = "multiple", ylab = var.names.fancy.main)