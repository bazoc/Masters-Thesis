load(file = "~/Thesis/Data/Sorting result.Rdata")

png("~/Thesis/Figures and Graphs/countrygroups.png")
barplot(sorttable,
     xlab = "Country",
     ylab = "No. of significant times in Strong Group",
     main = "Country Prevalence in the strong reaction group",
     las = 2, 
     cex.axis = 1)
#abline(a = quants[1], b = 0)
#abline(a = quants[2], b = 0)
dev.off()