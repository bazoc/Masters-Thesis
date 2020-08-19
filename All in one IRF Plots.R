#Options
source("~/Thesis/R Code/Steps, Conf, Runs.R")
width = 800
height = 600
load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")

######Single plots######
mainfolder <- "~/Thesis/FiguresandGraphs/"
subfolders <- c(main = "main", exog = "exog", pre =  "pre", post =  "post", 
                maxreact = "maxreact", minreact = "minreact", north = "north", 
                south = "south", nogreece = "nogreece", noireland = "noireland", 
                assets = "assets")
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
#png("~/Thesis/FiguresandGraphs/nogreece/2in1.png", width = width, height = height)
setEPS()
postscript("~/Thesis/FiguresandGraphs/nogreece/2in1.eps")
bazplotirf.allinone.double(irf.withci = allirfs.multiple$nogreece, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
                           ylab = var.names.main.graph, legendbot = c("Orthogonal Impulse Response w/o Greece", "Impulse Response - Baseline VAR"),
                           main = "temp no greece")
dev.off()

#No Ireland and Main
#png("~/Thesis/FiguresandGraphs/noireland/2in1.png", width = width, height = height)
setEPS()
postscript("~/Thesis/FiguresandGraphs/noireland/2in1.eps")
bazplotirf.allinone.double(irf.withci = allirfs.multiple$noireland, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
                           ylab = var.names.main.graph, legendbot = c("Orthogonal Impulse Response w/o Ireland", "Impulse Response - Baseline VAR"),
                           main = "temp no ire")
dev.off()

#Assets and Main
#png("~/Thesis/FiguresandGraphs/assets/2in1.png", width = width, height = height)
setEPS()
postscript("~/Thesis/FiguresandGraphs/assets/2in1.eps")
bazplotirf.allinone.double(irf.withci = allirfs.multiple$assets, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
                           ylab = var.names.assets.graph, legendbot = c("Orthogonal Impulse Response - CB Assets", "Impulse Response - Baseline VAR"),
                           main = "Impulse Response Functions for VAR with CB Assets")
dev.off()

#2 identification schemes
#png("~/Thesis/FiguresandGraphs/ortho 2/2in1.eps", width = width, height = height)
#bazplotirf.allinone.double(irf.withci = allirfs.multiple$ortho2, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.main)
#dev.off()

############Triple IRF plots##########################
#Identifications
setEPS()
postscript("~/Thesis/FiguresandGraphs/Triple/ident.eps")
bazplotirf.allinone.triple(irf.withci = allirfs.multiple$main, irf.noci1 = allirfs.multiple$ident2, irf.noci2 = allirfs.multiple$ident3, plot.type = "multiple", 
                           ylab = var.names.main.graph, main = "Orthogonal Impulse Response Functions for the Differening Identification methods", lwd = 2, confon = F, 
                           legendbot = c("Baseline Specification", "Alternative Identification One", "Alternative Identification Two")
)
dev.off()


#No Ireland, Greece,  Germany
setEPS()
postscript("~/Thesis/FiguresandGraphs/Triple/countries.eps")
bazplotirf.allinone.triple(irf.withci = allirfs.multiple$noireland, irf.noci1 = allirfs.multiple$nogreece, irf.noci2 = allirfs.multiple$nogermany, plot.type = "multiple", 
                           ylab = var.names.main.graph, main = "Orthogonal Impulse Response Functions without Germany, Greece and Ireland", lwd = 2, confon = F, 
                           legendbot = c("No Ireland", "No Greece", "No Germany")
)
dev.off()

###########testing#############
#load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")
#temp = allirfs.multiple$main