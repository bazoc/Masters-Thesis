#y = dum.panel
#p = 3
#type = "const"
#season = NULL
#exogen = exogenousdummy
#lag.max = NULL
#ic = c("AIC", "HQ", "SC", "FPE")
#panel_identifier = c(1,2)


bazfevar <- function (y, p = 1, type = c("const", "trend", "both", 
                             "none"), season = NULL, exogen = NULL, lag.max = NULL, 
          ic = c("AIC", "HQ", "SC", "FPE"), panel_identifier = c(1,2), allreadydemeaned = F, exogenfixed = NULL) 
{
  call <- match.call()
  
  if(class(y[1,1]) == "character") {
    
    if (any(is.na(y))) 
      stop("\nNAs in y.\n")
    if (ncol(y) < 2) 
      stop("The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.\n")
    if (is.null(colnames(y))) {
      colnames(y) <- paste("y", 1:ncol(y), sep = "")
      warning(paste("No column names supplied in y, using:", 
                  paste(colnames(y), collapse = ", "), ", instead.\n"))
    }
    colnames(y) <- make.names(colnames(y))
    y.orig <- y
    type <- match.arg(type)
  
    #Demean the y
  
    var.name <- colnames(y)[-panel_identifier]
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
    if (!is.null(lag.max)) {
      lag.max <- abs(as.integer(lag.max))
      ic <- paste(match.arg(ic), "(n)", sep = "")
      p <- VARselect(y, lag.max = lag.max, type = type, season = season, 
                     exogen = exogen)$selection[ic]
    }
    #Total sample size
    tot.sample <- (cont.obs - p)*no.cont
    cont.sample <- (cont.obs - p)
    ylagscont <- list()
    yendcont <- list()
    ylags <- NULL
    yend <- NULL
    for(i in 1:no.cont) {
      ylagscont[[countries[i]]] <- embed(ycont[[i]], dimension = p + 1)[, -(1:K)]
      yendcont[[countries[i]]] <- ycont[[i]][-c(1:p), ]
      
      ylags <- rbind(ylags, ylagscont[[countries[i]]])
      yend <- rbind(yend, yendcont[[countries[i]]])
    } 
    
    temp1 <- NULL
    temp <- NULL
    for (i in 1:p) {
    temp <- paste(colnames(demy), ".l", i, sep = "")
    temp1 <- c(temp1, temp)
    }
    colnames(ylags) <- temp1
  
    if (type == "const") {
      rhs <- cbind(ylags, rep(1, tot.sample))
      colnames(rhs) <- c(colnames(ylags), "const")
    }
    else if (type == "trend") {
      rhs <- cbind(ylags, seq(p + 1, length = sample))
      colnames(rhs) <- c(colnames(ylags), "trend")
    }
    else if (type == "both") {
      rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
      colnames(rhs) <- c(colnames(ylags), "const", "trend")
    }
    else if (type == "none") {
      rhs <- ylags
      colnames(rhs) <- colnames(ylags)
    }
    if (!(is.null(season))) {
      season <- abs(as.integer(season))
      dum <- (diag(season) - 1/season)[, -season]
      dums <- dum
      while (nrow(dums) < obs) {
        dums <- rbind(dums, dum)
      }
      dums <- dums[1:obs, ]
      colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
      rhs <- cbind(rhs, dums[-c(1:p), ])
    }
    if (!(is.null(exogen))) {

    }
    exogen1 <- NULL
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
      tmp <- colnames(rhs)
      exogencont <- list()
      exogen1 <- NULL
      if(ncol(exogen) == 1) {
        for(i in 1:no.cont) {
          y.min <- (1 + (i-1)*cont.obs)
          y.max <- i*cont.obs
        
          exogencont[[countries[i]]] <- as.matrix(exogen[y.min:y.max,])
          exogencont[[countries[i]]] <- exogencont[[countries[i]]][-c(1:p),]
          exogen1 <- c(exogen1, exogencont[[countries[i]]])
        }
      }
      else {
        for(i in 1:no.cont) {
          y.min <- (1 + (i-1)*cont.obs)
          y.max <- i*cont.obs
          
          exogencont[[countries[i]]] <- exogen[y.min:y.max,]
          exogencont[[countries[i]]] <- exogencont[[countries[i]]][-c(1:p),]
          exogen1 <- rbind(exogen1, exogencont[[countries[i]]])
        }
      }
      rownames(exogen1) <- NULL
      rhs <- cbind(rhs, exogen1)
      colnames(rhs) <- c(tmp, colnames(exogen))
    }
    datamat <- as.data.frame(rhs)
    colnames(datamat) <- colnames(rhs)
    equation <- list()
    for (i in 1:K) {
      y <- yend[, i]
      equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datamat)
      if (any(c("const", "both") %in% type)) {
        attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
      }
    }
    call <- match.call()
    if ("season" %in% names(call)) 
      call$season <- eval(season)
    result <- list(varresult = equation, datamat = data.frame(cbind(yend, 
                                                                    rhs)), y = demy, y.orig = y.orig, type = type, p = p, K = K, obs = tot.sample, 
                   totobs = tot.sample + p, restrictions = NULL, call = call, is.fe = T, yend = yend, cont.obs = cont.obs, exogen= exogen1, no.cont = no.cont)
    class(result) <- c("varest")
  }
  else if(class(y[1,1]) != "character") {
    y <- as.matrix(y)
    if (any(is.na(y))) 
      stop("\nNAs in y.\n")
    if (ncol(y) < 2) 
      stop("The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.\n")
    if (is.null(colnames(y))) {
      colnames(y) <- paste("y", 1:ncol(y), sep = "")
      warning(paste("No column names supplied in y, using:", 
                    paste(colnames(y), collapse = ", "), ", instead.\n"))
    }
    colnames(y) <- make.names(colnames(y))
    y.orig <- y
    type <- match.arg(type)
    obs <- dim(y)[1]
    K <- dim(y)[2]
    if (!is.null(lag.max)) {
      lag.max <- abs(as.integer(lag.max))
      ic <- paste(match.arg(ic), "(n)", sep = "")
      p <- VARselect(y, lag.max = lag.max, type = type, season = season, 
                     exogen = exogen)$selection[ic]
    }
    sample <- obs - p
    ylags <- embed(y, dimension = p + 1)[, -(1:K)]
    temp1 <- NULL
    for (i in 1:p) {
      temp <- paste(colnames(y), ".l", i, sep = "")
      temp1 <- c(temp1, temp)
    }
    colnames(ylags) <- temp1
    yend <- y[-c(1:p), ]
    if (type == "const") {
      rhs <- cbind(ylags, rep(1, sample))
      colnames(rhs) <- c(colnames(ylags), "const")
    }
    else if (type == "trend") {
      rhs <- cbind(ylags, seq(p + 1, length = sample))
      colnames(rhs) <- c(colnames(ylags), "trend")
    }
    else if (type == "both") {
      rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
      colnames(rhs) <- c(colnames(ylags), "const", "trend")
    }
    else if (type == "none") {
      rhs <- ylags
      colnames(rhs) <- colnames(ylags)
    }
    if (!(is.null(season))) {
      season <- abs(as.integer(season))
      dum <- (diag(season) - 1/season)[, -season]
      dums <- dum
      while (nrow(dums) < obs) {
        dums <- rbind(dums, dum)
      }
      dums <- dums[1:obs, ]
      colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
      rhs <- cbind(rhs, dums[-c(1:p), ])
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
      tmp <- colnames(rhs)
      rhs <- cbind(rhs, exogen[-c(1:p), ])
      colnames(rhs) <- c(tmp, colnames(exogen))
    }
    datamat <- as.data.frame(rhs)
    colnames(datamat) <- colnames(rhs)
    equation <- list()
    for (i in 1:K) {
      y <- yend[, i]
      equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datamat)
      if (any(c("const", "both") %in% type)) {
        attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
      }
    }
    if ("season" %in% names(call)) 
      call$season <- eval(season)
    result <- list(varresult = equation, datamat = data.frame(cbind(yend, 
                                                                    rhs)), y = y.orig, type = type, p = p, K = K, obs = sample, 
                   totobs = sample + p, restrictions = NULL, call = call)
    class(result) <- "varest"

  }
  
  return(result)
}
fevar.main <- bazfevar(main.panel, p = laglen, type = "const")

#object = fevar
#cause = c("demeaned_lgdp", "demeaned_lhou")
bazgrangertest <- function (object, cause = NULL) 
{
  if(class(object) == "varest") 1
 
  else if(class(object) != "svar")
    stop("\nFix this function so it works with SVAR")
  type <- object$type
  K <- object$K
  p = object$p
  datamat <- object$datamat[,-c(1:K)]
  yend <- object$yend
  mainequation <- object$varresult
  call <- match.call()
  allequations <- list()
  allequations <- list("main" = mainequation)
  if(is.null(cause)) {
    grangenames <- colnames(object$y)
    M <- K
  }
  if(!is.null(cause)) {
    grangenames <- cause
    M <- length(cause)
  }
  totnames <- colnames(object$y)
  grantest <- list()
  for(j in 1:M) {
    causalvar <- grangenames[j]
    g <- which(gsub("\\.l[[:digit:]]", "", colnames(datamat)) %in% 
                 causalvar)
    
    datamatcur <- datamat[,-g]
    equation <- list()
    for (i in 1:K) {
      y <- yend[, i]
      equation[[colnames(yend)[i]]] <- lm(y ~ -1 + ., data = datamatcur)
      if (any(c("const", "both") %in% type)) {
        attr(equation[[colnames(yend)[i]]]$terms, "intercept") <- 1
      }
    }
    allequations[causalvar] <- list(equation)
    temptest <- list()
    for(i in 1:K)  {
      XY <- allequations[[causalvar]][[totnames[i]]]
      YX <- mainequation[[totnames[i]]]
      temptest[[totnames[i]]] <- anova(XY, YX)
    }
    grantest[[causalvar]] <- temptest
  }
  result <- list(grantest = grantest, causalvars = grangenames)
  class(result) <- "grantest"
  return(result)
}
grangerdanger <- bazgrangertest(fevar.main)

#x = grangerdanger
summary.grantest <- function(x) {
  
  causnames <- x$causalvars
  effectnames <- names(x[[1]][[1]])
  E <- length(effectnames)
  C <- length(causnames)
  temp <- character()
  for(i in 1:E) {
    temp <- c(temp,"F Value", "P Value")
  }
  temp1 <- paste(rep(effectnames, each = 2), temp, sep = " ")
  
  critstattot <- list()
  critstat <- matrix(NA, nrow = C, ncol = E*2)
  colnames(critstat) <- temp
  rownames(critstat) <- causnames
  for(j in 1:C) {
    for(i in 1:E) {
      n = i*2
      Fvalue <- x[[1]][[causnames[j]]][[effectnames[i]]][["F"]][[2]]
      Pvalue <- x[[1]][[causnames[j]]][[effectnames[i]]][["Pr(>F)"]][[2]]
      Pvalue <- round(Pvalue, digits = 4)
      critstat[j,n-1] <- Fvalue
      critstat[j,n] <- Pvalue
    }
  }
  forexcel <- critstat
  colnames(forexcel) <- temp1
  for(i in 1:E) {
    n <- i*2
    critstattot[[effectnames[i]]] <- critstat[,c(n,n-1)]
  }
  howtoread = "Do rows granger cause columns?"
  grangersummary <- critstattot
  result <- list(grangersummary = grangersummary, causalvars = causnames, howtoread = howtoread, forexcel = forexcel)
  return(result)
}

fevar.main <- bazfevar(main.panel, p = laglen, type = "const")
grangerdanger <- bazgrangertest(fevar.main)
granger.summary <- summary(grangerdanger)
granger.summary$forexcel
write.csv(granger.summary$forexcel, "~/Thesis/Data/Granger Test.csv")