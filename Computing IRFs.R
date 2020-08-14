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
irf.ident2.ortho <- list()
irf.ident3.ortho <- list()

for(i in 1:5) {
  irf.main.ortho.1[[var.names.main[i]]] <- bazirf.varest(fevar.main, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 253)
  irf.exog.ortho.1[[var.names.main[i]]] <- bazirf.varest(fevar.exog, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 7084)
  irf.pre.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.pre, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 405)
  irf.post.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.post, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 195)
  irf.north.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.north, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 129)
  irf.south.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.south, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 3524)
  irf.nogreece.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.nogreece, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 4749)
  irf.noireland.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.noireland, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 2015)
}

for(i in 1:5) {
  irf.maxreaction.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.large.reaction, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 7540)
  irf.minreaction.ortho[[var.names.main[i]]] <- bazirf.varest(fevar.small.reaction, impulse = imp1[i], n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 3047)
}

for(i in 1:length(var.names.assets)) {
  irf.assets.ortho[[var.names.assets[i]]] <- bazirf.varest(fevar.assets, impulse = imp2[i] ,n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 678)
}

allirfs.notnormal <- list(main = irf.main.ortho.1, exog = irf.exog.ortho.1, pre = irf.pre.ortho,post = irf.post.ortho, 
                maxreact = irf.maxreaction.ortho, minreact = irf.minreaction.ortho, north = irf.north.ortho,
                south = irf.south.ortho, nogreece = irf.nogreece.ortho, noireland = irf.noireland.ortho, 
                assets = irf.assets.ortho)
save(allirfs.notnormal, file = "~/Thesis/Data/All IRFs not normal.Rdata")


#################Multiple###################
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
irf.ident2.ortho <- bazirf.varest(fevar.ident2, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 8662)
irf.ident3.ortho <- bazirf.varest(fevar.ident3, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 1923)
irf.assets.ortho <- bazirf.varest(fevar.assets, n.ahead = steps, ortho = T, ci = conf, runs = runs, seed = 678)

#allirfs.multiple.notnormal$ident2 <- irf.ident2.ortho
#allirfs.multiple.notnormal$ident3 <- irf.ident3.ortho

allirfs.multiple.notnormal <- list(main = irf.main.ortho.1, exog = irf.exog.ortho.1, pre = irf.pre.ortho,post = irf.post.ortho, 
                         maxreact = irf.maxreaction.ortho, minreact = irf.minreaction.ortho, north = irf.north.ortho,
                         south = irf.south.ortho, nogreece = irf.nogreece.ortho, noireland = irf.noireland.ortho, 
                         assets = irf.assets.ortho, ident2 = irf.ident2.ortho, ident3 = irf.ident3.ortho)
save(allirfs.multiple.notnormal, file = "~/Thesis/Data/All Multiple IRFs not normal.Rdata")



##########Normalise the IRFs################
load(file = "~/Thesis/Data/All IRFs not normal.Rdata")
load(file = "~/Thesis/Data/All Multiple IRFs not normal.Rdata")

source("~/Thesis/R Code/normaliseirf.R")
allirfs <- list()
allirfs.multiple <- list()
ainm <- names(allirfs.notnormal)
for(i in 1:length(ainm)) {
  allirfs[[ainm[i]]] <- normaliseirf(allirfs.notnormal[[ainm[i]]])
}
namen <- names(allirfs.multiple.notnormal)
for(i in 1:length(namen)) {
  allirfs.multiple[[namen[i]]] <- normaliseirf(allirfs.multiple.notnormal[[namen[i]]])
}

save(allirfs, file = "~/Thesis/Data/All IRFs.Rdata")
save(allirfs.multiple, file = "~/Thesis/Data/All Multiple IRFs.Rdata")
