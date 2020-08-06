source("~/Thesis/R Code/setup.R")
source("~/Thesis/R Code/FEVARs.R")

#Options
source("~/Thesis/R Code/Steps, Conf, Runs.R")

#Options
irf.main.ortho.1 <- bazirf.varest(fevar.main, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 253)
irf.exog.ortho.1 <- bazirf.varest(fevar.exog, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 7084)
irf.pre.ortho <- bazirf.varest(fevar.pre, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 405)
irf.post.ortho <- bazirf.varest(fevar.post, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 195)
irf.maxreaction.ortho <- bazirf.varest(fevar.large.reaction, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 7540)
irf.minreaction.ortho <- bazirf.varest(fevar.small.reaction, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 3047)
irf.north.ortho <- bazirf.varest(fevar.north, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 129)
irf.south.ortho <- bazirf.varest(fevar.south, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 3524)
irf.nogreece.ortho <- bazirf.varest(fevar.nogreece, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 4749)
irf.noireland.ortho <- bazirf.varest(fevar.noireland, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 2015)
irf.assets.ortho <- bazirf.varest(fevar.assets, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 678)



allirfs <- list(main = irf.main.ortho.1, exog = irf.exog.ortho.1, pre = irf.pre.ortho,post = irf.post.ortho, 
                maxreact = irf.maxreaction.ortho, minreact = irf.minreaction.ortho, north <- irf.north.ortho,
                south <- irf.south.ortho, nogreece <- irf.nogreece.ortho, noireland = irf.noireland.ortho, 
                assets = irf.assets.ortho)

mainfolder <- "~/Thesis/Figures and Graphs/"
subfolders <- c("main irf", "exog irf", "pre irf", "post irf", "max react", "min react", "north", "south", "no greece", "no ireland", "assets irf")
savelocation <- paste(mainfolder, subfolders, "/Full", sep = "")
novars <- length(folders)
for(j in 1:(novars)) {
  png(savelocation[[j]])
  bazplotirf.allinone(allirfs[[j]], plot.type = "multiple", ylab = var.names.fancy.main)
  dev.off()
}