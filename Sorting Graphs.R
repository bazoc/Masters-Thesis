load("~/Thesis/Data/Sorting result.Rdata")


#Bar plot with countries and a line


setEPS()
postscript("~/Thesis/Figures and Graphs/sortingresultsbar.eps")

par(oma = c(3,1,1,1))
barplot(allqs$`24`$sorttable, las = 2, main = "Results of Grouping Algorithm",
        ylab = "No. of times in strong group",
        xlab = NA, col = "lightblue")
abline(allqs$`20`$quants[1], b = c(0), col = "red", lwd = 2, lty = 2)
abline(a = allqs$`20`$quants[2], b = c(0), col = "black", lwd = 2, lty = 2)

legend("top", legend = c("Upper 95% Cut off", "Lower 95% Cut off") ,col = c("black", "red"), lwd = 2, lty = 2, xpd = TRUE, cex = .9, seg.len=2)
mtext("Country", 1, outer = T)
dev.off()

#IMF Mortgage Market

countfreq <- allqs$`24`$countfreq
imfmort <- c("Austria" = 0.31,
             "Finland" = 0.49,
             "France" = 0.23,
             "Germany" = 0.28,
             "Greece" = 0.35,
             "Ireland" = 0.39,
             "Italy" = 0.26,
             "Netherlands" = 0.71,
             "Portugal" = NA,
             "Spain" = 0.4)

plot(x = imfmort, 
     y = countfreq, 
     type = "n",
     ylab = "Frequency pf times in Strong Reaction Group",
     xlab = "IMF Mortgage Market Index",
     xlim = c(0,1))
points(x = imfmort, 
       y = countfreq,
       col = 'blue',
       pch = 16, #Symbol shown
       cex = 1)
abline(lm(countfreq ~ imfmort), col = "red", lwd = 2)
mtext("Comparison of Strong Reaction Group and IMF Mortgage Market Index", 3, line = 1, outer = F, cex = 1.2)
namen <- names(allqs)
larrect.var <- list()
smlrect.var <- list()
for(i in 1:length(namen)) {
large.reaction.panel <- filter(main.panel,
                               Country %in% allqs[[namen[i]]]$large.reaction.group)
larrect.var[[namen[i]]] <- bazfevar(y = large.reaction.panel,
                                 p = laglen,
                                 type = "const")


small.reaction.panel <- filter(main.panel,
                               Country %in% allqs[[namen[i]]]$small.reaction.group)
smlrect.var[[namen[i]]] <- bazfevar(y = small.reaction.panel,
                                 p = laglen,
                                 type = "const")
}

imp = c("demeaned_int", "demeaned_lhou")
larrect.irf <- list()
smlrect.irf <- list()
for(j in 1:length(namen)) {
  for(i in 1:2) {
    larrect.irf[[namen[j]]][[imp[i]]] <- bazirf.varest(larrect.var[[j]], impulse = imp[i], n.ahead = steps, ortho = T, ci = conf, runs = 100, seed = 7540)
    smlrect.irf[[namen[j]]][[imp[i]]] <- bazirf.varest(smlrect.var[[j]], impulse = imp[i], n.ahead = steps, ortho = T, ci = conf, runs = 100, seed = 3047)
  }
}
num = "24"
bazplotirf.double(irf.withci = larrect.irf[[num]][[1]], irf.noci = smlrect.irf[[num]][[1]], plot.type = "multiple", 
                  ylab = var.names.main.graph, 
                  legendbot = c("Impulse Response for Strong Reaction Group", "Impulse Response for Weak Reaction Group"))
bazplotirf.double(irf.withci = larrect.irf[[num]][[2]], irf.noci = smlrect.irf[[num]][[2]], plot.type = "multiple", 
                  ylab = var.names.main.graph, 
                  legendbot = c("Impulse Response for Strong Reaction Group", "Impulse Response for Weak Reaction Group"))
allqs$`20`
#4 is piy