#y = dum.panel[,1:7]
#lag.max = 10
#type = "const"
#season = NULL
#exogen = dum.panel[,8:19]
#panel_identifier = c(1,2)


bazfeVARselect <- function (y, lag.max = 10, type = c("const", "trend", 
                                    "both", "none"), season = NULL, exogen = NULL, panel_identifier = c(1,2)) 
{
  if (any(is.na(y))) 
    stop("\nNAs in y.\n")
  colnames(y) <- make.names(colnames(y))
  lag.max <- abs(as.integer(lag.max))
  type <- match.arg(type)
  lag <- abs(as.integer(lag.max + 1))

  var.name <- colnames(y)[-(panel_identifier)]
  demy <- matrix(NA, ncol = ncol(y)-2, nrow = nrow(y))
  colnames(demy) <- paste0("demeaned_", var.name)
  ydata <- y[,-panel_identifier]
  ydata <- as.matrix(ydata)
  countries <- unique(y[,panel_identifier[1]])
  no.cont <- length(countries)
  cont.obs <- length(unique(y[,panel_identifier[2]]))
  for(j in 1:length(var.name)) {
    ave <- by(y, y[,panel_identifier[1]], function(m) mean(m[,j+2]))
    for(i in 1:nrow(y)) {
      country <- y[i,panel_identifier[1]]
      demy[i,j] <- ydata[i,j] - ave[[country]]
    }
  }
  ycont <- list()
  for(i in 1:no.cont) {
    y.min <- (1 + (i-1)*cont.obs)
    y.max <- i*cont.obs
    ycont[[countries[i]]] <- demy[y.min:y.max,]
  }
  tot.obs <- dim(demy)[1]
  K <- dim(demy)[2]
  #Total sample size
  ylagscont <- list()
  yendcont <- list()
  ylagged <- NULL
  yendog <- NULL
  for(i in 1:no.cont) {
    ylagscont[[countries[i]]] <- embed(ycont[[i]], dimension = lag)[, -c(1:K)]
    yendcont[[countries[i]]] <- ycont[[i]][-c(1:lag.max), ]
    
    ylagged <- rbind(ylagged, ylagscont[[countries[i]]])
    yendog <- rbind(yendog, yendcont[[countries[i]]])
  } 


  sample <- nrow(ylagged)
  rhs <- switch(type, const = rep(1, sample), trend = seq(lag.max + 
                                                            1, length = sample), both = cbind(rep(1, sample), seq(lag.max + 
                                                                                                                    1, length = sample)), none = NULL)
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < sample) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:sample, ]
    rhs <- cbind(rhs, dums)
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen), 
                                sep = "")
      warning(paste("No column names supplied in exogen, using:", 
                    paste(colnames(exogen), collapse = ", "), 
                    ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    exogencont <- list()
    exogen1 <- NULL
    for(i in 1:no.cont) {
      y.min <- (1 + (i-1)*cont.obs)
      y.max <- i*cont.obs
      exogencont[[countries[i]]] <- exogen[y.min:y.max,]
      exogencont[[i]] <- exogencont[[i]][-c(1:lag.max),]
      exogen1 <- rbind(exogen1, exogencont[[countries[i]]])
    }
    rownames(exogen1) <- NULL
    rhs <- cbind(rhs, exogen1)
  }
  idx <- seq(K, K * lag.max, K)
  if (!is.null(rhs)) {
    detint <- ncol(as.matrix(rhs))
  }
  else {
    detint <- 0
  }
  criteria <- matrix(NA, nrow = 4, ncol = lag.max)
  rownames(criteria) <- c("AIC(n)", "HQ(n)", "SC(n)", 
                          "FPE(n)")
  colnames(criteria) <- paste(seq(1:lag.max))
  for (i in 1:lag.max) {
    ys.lagged <- cbind(ylagged[, c(1:idx[i])], rhs)
    sampletot <- nrow(y)
    nstar <- ncol(ys.lagged)
    resids <- lm.fit(x = ys.lagged, y = yendog)$residuals
    sigma.det <- det(crossprod(resids)/sample)
    criteria[1, i] <- log(sigma.det) + (2/sample) * (i * 
                                                       K^2 + K * detint)
    criteria[2, i] <- log(sigma.det) + (2 * log(log(sample))/sample) * 
      (i * K^2 + K * detint)
    criteria[3, i] <- log(sigma.det) + (log(sample)/sample) * 
      (i * K^2 + K * detint)
    criteria[4, i] <- ((sample + nstar)/(sample - nstar))^K * 
      sigma.det
  }
  order <- apply(criteria, 1, which.min)
  return(list(selection = order, criteria = criteria))
}

mainlagselect <- bazfeVARselect(y = main.panel[,c("Country", "yqtr", "lgdp", "lhou", "lres", "ldef", "int")],
                                lag.max =5,
                                type = "const",
                                season = NULL,
                                panel_identifier = c(1,2)
)
bazfeVARselect(y = pre.panel[,c("Country", "yqtr", "lgdp", "lhou", "lres", "ldef", "int")],
               lag.max = 10,
               type = "const",
               season = NULL,
               panel_identifier = c(1,2)
)
bazfeVARselect(y = post.panel,
               lag.max = 10, 
               season = NULL,
               panel_identifier = c(1,2))

write.csv(mainlagselect$criteria, "~/Thesis/Data/Lag select main.csv")

laglen = 4