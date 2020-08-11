source("~/Thesis/R Code/setup.R")
load("~/Thesis/Data/Sorting result.Rdata")


#####Interest rates all observations############
fevar.main <- bazfevar(main.panel, p = laglen, type = "const")

##################Interest rates no crash years####
fevar.exog <- bazfevar(exog.panel[1:7], p = laglen, type = "const", exogen = exog.panel[8:18])

################Interest rates pre recession################
fevar.pre <- bazfevar(y = pre.panel,
                     p = laglen,
                     type = "const")

#################################Interest rates post recession################################
fevar.post <- bazfevar(y = post.panel,
                     p = laglen,
                     type = "const")

#####################Interest rates dum variable for pre and post recession########################
exogenousdummy <- as.matrix(dum.panel[,8], ncol = 1, nrow = length(dum.panel[,8]))
colnames(exogenousdummy) <- "dumcrash"
fevar.dum <- bazfevar(y = dum.panel[,1:7],
                       p = laglen,
                       type = "const",
                       exogen = exogenousdummy)

############################Central Bank Assets post recession#######################################
fevar.assets <- bazfevar(y = assets.panel,
                       p = laglen,
                       type = "const")

#############################Sorted groups###############################
large.reaction.panel <- filter(main.panel,
                               Country %in% large.reaction.group)
fevar.large.reaction <- bazfevar(y = large.reaction.panel,
                                 p = laglen,
                                 type = "const")

small.reaction.panel <- filter(main.panel,
                               Country %in% small.reaction.group)
fevar.small.reaction <- bazfevar(y = small.reaction.panel,
                                 p = laglen,
                                 type = "const")


##########################North vs. South#################################
north.panel <- filter(main.panel,
                      Country %in% north)
fevar.north <- bazfevar(y = north.panel,
                        p = laglen,
                        type = "const")
south.panel <- filter(main.panel,
                      Country %in% south)
fevar.south <- bazfevar(y = south.panel,
                        p = laglen,
                        type = "const")

#################################No Greece############################
nogreece.panel <- filter(main.panel,
                         Country != "Greece")
fevar.nogreece  <- bazfevar(y = nogreece.panel,
                            p = laglen,
                            type = "const")

###############################No Ireland###########################
noireland.panel <- filter(main.panel,
                          Country != "Ireland")
fevar.noireland <- bazfevar(y = noireland.panel,
                            p = laglen,
                            type = "const") 

###########################No Greece nor Ireland#################################
neither.panel <- filter(main.panel,
                        Country != "Ireland" & Country != "Greece")
fevar.neither <- bazfevar(y = neither.panel,
                            p = laglen,
                            type = "const") 

#########################Other identification####################################
other.panel <- main.panel[,c("Country", "yqtr")]

##############################Summaries##########################
summary(fevar.main)$roots
summary(fevar.exog)$roots


summary(fevar.pre)$roots
#Good on the roots


summary(fevar.post)$roots
#Good on roots

summary(fevar.dum)$roots
#Very significant in interest rates and house prices, not significant in any of the others

summary(fevar.assets)$roots
#Good on roots

summary(fevar.large.reaction)$roots
summary(fevar.small.reaction)$roots
summary(fevar.north)$roots
summary(fevar.south)$roots
summary(fevar.nogreece)$roots
summary(fevar.noireland)$roots
#Good on all the roots

##########################Make Coefficient table#######################
maincoeftable <- list()
maincoeftable[["lgdp"]] <- tidy(fevar.main$varresult$demeaned_lgdp)
maincoeftable[["lres"]] <- tidy(fevar.main$varresult$demeaned_lres)
maincoeftable[["ldef"]] <- tidy(fevar.main$varresult$demeaned_ldef)
maincoeftable[["int"]]  <- tidy(fevar.main$varresult$demeaned_int)
maincoeftable[["lhou"]] <- tidy(fevar.main$varresult$demeaned_lhou)


for(j in 1:length(var.names.main)) {
  #Round it as well
  coeftable <- maincoeftable[[var.names.main[j]]]
  coeftable <- as.data.frame(coeftable)
  coeftable[,2] <- as.character(coeftable[,2])
  for(i in 1:(length(var.names.main)*laglen)) { # All the coefficients
    if(coeftable[i,5] < .01) {
      coeftable[i,2] <- paste(coeftable[i,2], "***")
    }
    else if(coeftable[i,5] < .05) {
      coeftable[i,2] <- paste(coeftable[i,2], "**")
    }
    else if(coeftable[i,5] < .1) {
      coeftable[i,2] <- paste(coeftable[i,2], "*")
    }
  }
  maincoeftable[[var.names.main[j]]] <- coeftable
}

write.csv(maincoeftable$lgdp, "~/Thesis/Data/Main Coefficients/GDP.csv")
write.csv(maincoeftable$lres, "~/Thesis/Data/Main Coefficients/RES.csv")
write.csv(maincoeftable$ldef, "~/Thesis/Data/Main Coefficients/DEF.csv")
write.csv(maincoeftable$int , "~/Thesis/Data/Main Coefficients/INT.csv")
write.csv(maincoeftable$lhou, "~/Thesis/Data/Main Coefficients/HOU.csv")


svar.main <- SVAR(fevar.main, Amat = NULL, Bmat = bmat) #Exact same as the other orthogonalised
