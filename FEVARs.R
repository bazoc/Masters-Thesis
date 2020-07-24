source("~/Thesis/R Code/setup.R")

#Interest rates all observations
fevar.main <- bazfevar(main.panel, p = laglen, type = "const")

#Interest rates no crash years
fevar.exog <- bazfevar(exog.panel[1:7], p = laglen, type = "const", exogen = exog.panel[8:18])

#Interest rates pre recession
fevar.pre <- bazfevar(y = pre.panel,
                     p = laglen,
                     type = "const")

#Interest rates post recession
fevar.post <- bazfevar(y = post.panel,
                     p = laglen,
                     type = "const")

#Interest rates dum variable for pre and post recession
#Interest rates post recession
exogenousdummy <- as.matrix(dum.panel[,8], ncol = 1, nrow = length(dum.panel[,8]))
colnames(exogenousdummy) <- "dumcrash"
fevar.dum <- bazfevar(y = dum.panel[,1:7],
                       p = laglen,
                       type = "const",
                       exogen = exogenousdummy)

#Central Bank Assets post recession
fevar.assets <- bazfevar(y = assets.panel,
                       p = laglen,
                       type = "const")
#Summaries
summary(fevar.main)
summary(fevar.exog)


summary(fevar.pre)
#Good on the roots


summary(fevar.post)
#Good on roots

summary(fevar.dum)
#Very significant in interest rates and house prices, not significant in any of the others

summary(fevar.assets)
#Good on roots