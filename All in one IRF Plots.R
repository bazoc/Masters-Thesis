source("~/Thesis/R Code/FEVARs.R")

#Options
source("~/Thesis/R Code/Steps, Conf, Runs.R")
width = 800
height = 600

#IRFs
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



allirfs.multiple <- list(main = irf.main.ortho.1, exog = irf.exog.ortho.1, pre = irf.pre.ortho,post = irf.post.ortho, 
                maxreact = irf.maxreaction.ortho, minreact = irf.minreaction.ortho, north = irf.north.ortho,
                south = irf.south.ortho, nogreece = irf.nogreece.ortho, noireland = irf.noireland.ortho, 
                assets = irf.assets.ortho)
save(allirfs.multiple, file = "~/Thesis/Data/All Multiple IRFs.Rdata")



######Single plots######
mainfolder <- "~/Thesis/Figures and Graphs/"
subfolders <- c(main = "main irf", exog = "exog irf", pre =  "pre irf", post =  "post irf", 
                maxreact = "max react", minreact = "min react", north = "north", 
                south = "south", nogreece = "no greece", noireland = "no ireland", 
                assets = "assets irf")
all(names(allirfs.multiple) == names(subfolders))

savelocation <- paste(mainfolder, subfolders, "/Full.png", sep = "")
novars <- length(subfolders)
for(j in 1:(novars-1)) {
  png(savelocation[[j]], width = width, height = height)
  bazplotirf.allinone(allirfs.multiple[[j]], plot.type = "multiple", ylab = var.names.main)
  dev.off()
}
#Assets
png(savelocation[[novars]])
bazplotirf.allinone(allirfs.multiple[[novars]], plot.type = "multiple", ylab = var.names.assets)
dev.off()





#######2 in 1 plots###########
#No Greece and Main
png("~/Thesis/Figures and Graphs/no greece/2in1.png", width = width, height = height)
bazplotirf.allinone.double(irf.withci = allirfs.multiple$nogreece, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.main)
dev.off()

#No Ireland and Main
png("~/Thesis/Figures and Graphs/no ireland/2in1.png", width = width, height = height)
bazplotirf.allinone.double(irf.withci = allirfs.multiple$noireland, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.main)
dev.off()

#Assets and Main
png("~/Thesis/Figures and Graphs/assets irf/2in1.png", width = width, height = height)
bazplotirf.allinone.double(irf.withci = allirfs.multiple$assets, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.assets)
dev.off()

#2 identification schemes
#png("~/Thesis/Figures and Graphs/ortho 2/2in1.png", width = width, height = height)
#bazplotirf.allinone.double(irf.withci = allirfs.multiple$ortho2, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.main)
#dev.off()

###########testing#############
#load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")
#temp = allirfs.multiple$main