fevdfevar = vars:::fevd.varest(fevar.main, n.ahead = 40)

names(fevdfevar) <- var.names.fancy.main
fevdcols <- c("pink", "lightsteelblue", "paleturquoise4", "lemonchiffon2", "gray88")
png("~/Thesis/Figures and Graphs/mainfevd.png")

plot(fevdfevar, plot.type = "multiple", mar = c(4,1,2,3), col = fevdcols, xlab = "Quarters Ahead", legend = F, oma = c(4, 1, 1, 1))


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("GDP", "Res. Investment", "Inflation", "Interest", "House Prices"), xpd = FALSE, horiz = TRUE, inset = c(0, 
                                                                                 0), bty = "n", pch = rep(15,5), col = fevdcols, cex = .9)

dev.off()


fevdsvar <- vars:::fevd.svarest(svar.main, n.ahead = 40)
plot(fevdsvar, plot.type = "multiple", mar = c(4,1,2,3), col = fevdcols, xlab = "Quarters Ahead", legend = F, oma = c(4, 1, 1, 1))
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("GDP", "Res. Investment", "Inflation", "Interest", "House Prices"), xpd = FALSE, horiz = TRUE, inset = c(0,0), bty = "n", pch = rep(15,5), col = fevdcols, cex = .9)

dev.off()