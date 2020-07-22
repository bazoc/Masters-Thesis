fevdfevar = fevd(fevar, n.ahead = 40)

names(fevdfevar) <- var_names_fancy
fevdcols <- c("pink", "lightsteelblue", "paleturquoise4", "lemonchiffon2", "gray88")
png("~/Thesis/Figures and Graphs/mainfevd.png")

plot(x, plot.type = "multiple", mar = c(4,1,2,3), col = fevdcols, xlab = "Quarters Ahead", legend = F, oma = c(4, 1, 1, 1))


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", c("GDP", "House Prices", "Res. Investment", "Inflation", "Interest"), xpd = TRUE, horiz = TRUE, inset = c(0, 
                                                                                 0), bty = "n", pch = rep(15,5), col = fevdcols, cex = .9)

dev.off()

