#source("~/Thesis/R Code/setup.R")

#Interest rates all observations
fevar.main <- bazfevar(full.panel, p = laglen, type = "const")

#Interest rates year dummy variables
fevar.exog <- bazfevar(dum.panel[1:7], p = laglen, type = "const", exogen = dum.panel[8:18])

#Interest rates pre recession
fevar.pre <- bazfevar(y = pre.panel,
                     p = laglen,
                     type = "const")

#Interest rates post recession
fevar.post <- bazfevar(y = post.panel,
                     p = laglen,
                     type = "const")

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


summary(fevar.assets)
#Good on roots