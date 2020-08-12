source("~/Thesis/R Code/FEVARs.R")

#Options
source("~/Thesis/R Code/Steps, Conf, Runs.R")
width = 800
height = 600
load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")

######Single plots######
mainfolder <- "~/Thesis/Figures and Graphs/"
subfolders <- c(main = "main irf", exog = "exog irf", pre =  "pre irf", post =  "post irf", 
                maxreact = "max react", minreact = "min react", north = "north", 
                south = "south", nogreece = "no greece", noireland = "no ireland", 
                assets = "assets irf")
all(names(allirfs.multiple) == names(subfolders))
savelocation <- paste(mainfolder, subfolders, "/Full.eps", sep = "")

maintitles <- c(main = "Orthogonal Impulse Responses for the Primary VAR",
                exog = "LOL",
                pre = "Orthogonal Impulse Responses for Pre-Crisis VAR",
                post = "Orthogonal Impulse Responses for Post-Crisis VAR",
                maxreact = "Orthogonal Impulse Responses for the Strong Reaction Group VAR",
                minreact = "Orthogonal Impulse Responses for the Weak Reaction Group VAR",
                north = "Orthogonal Impulse Responses for Northern Country Group VAR",
                south = "Orthogonal Impulse Responses for Southern Country Group VAR",
                nogreece = "Orthogonal Impulse Responses for Panel without Greece",
                noireland = "Orthogonal Impulse Responses for Panel without Ireland",
                assets = "Orthogonal Impulse Responses for VAR with Central Bank Assets")

novars <- length(subfolders)
for(j in 1:(novars-1)) {
#  png(savelocation[[j]], width = width, height = height)
  setEPS()
  postscript(savelocation[[j]])
  bazplotirf.allinone(allirfs.multiple[[j]], plot.type = "multiple", ylab = var.names.main.graph, legendbot = "Impulse Response", main = maintitles[j])
  dev.off()
}
#Assets
#png(savelocation[[novars]])
setEPS()
postscript(savelocation[[novars]])
bazplotirf.allinone(allirfs.multiple[[novars]], plot.type = "multiple", ylab = var.names.assets.graph, legendbot = "Impulse Response", main = maintitles[j])
dev.off()





#######2 in 1 plots###########
#No Greece and Main
#png("~/Thesis/Figures and Graphs/no greece/2in1.png", width = width, height = height)
setEPS()
postscript("~/Thesis/Figures and Graphs/no greece/2in1.eps")
bazplotirf.allinone.double(irf.withci = allirfs.multiple$nogreece, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
                           ylab = var.names.main.graph, legendbot = c("Impulse Response w/o Greece", "Impulse Response - Main"),
                           main = "temp no greece")
dev.off()

#No Ireland and Main
#png("~/Thesis/Figures and Graphs/no ireland/2in1.png", width = width, height = height)
setEPS()
postscript("~/Thesis/Figures and Graphs/no ireland/2in1.eps")
bazplotirf.allinone.double(irf.withci = allirfs.multiple$noireland, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
                           ylab = var.names.main.graph, legendbot = c("Impulse Response w/o Ireland", "Impulse Response - Main"),
                           main = "temp no ire")
dev.off()

#Assets and Main
#png("~/Thesis/Figures and Graphs/assets irf/2in1.png", width = width, height = height)
setEPS()
postscript("~/Thesis/Figures and Graphs/assets/2in1.eps")
bazplotirf.allinone.double(irf.withci = allirfs.multiple$assets, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
                           ylab = var.names.assets.graph, legendbot = c("Impulse Response - CB Assets", "Impulse Response - Main"),
                           main = "temp assets")
dev.off()

#2 identification schemes
#png("~/Thesis/Figures and Graphs/ortho 2/2in1.eps", width = width, height = height)
#bazplotirf.allinone.double(irf.withci = allirfs.multiple$ortho2, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.main)
#dev.off()

###########testing#############
#load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")
#temp = allirfs.multiple$main