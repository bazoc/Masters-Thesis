preirf <- bazirf.varest(fevar.pre, n.ahead = 40, impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)
postirf <- bazirf.varest(fevar.post, n.ahead = 40, impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)

bazplotirf.double <- function (irf1 = NULL, irf2 = NULL, plot.type = c("multiple"), 
                        names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
                        col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 
                                                                                             4, 0, 4), oma.multi = c(6, 4, 6, 4), adj.mtext = NA, 
                        padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, ...) 
{
  if(is.null(irf1) | is.null(irf2)) {
    stop("\nNeed two IRFs")
  }
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  plot.type <- match.arg(plot.type)
  inames <- irf1$impulse
  rnames <- irf1$response
  if (is.null(names)) {
    names <- inames
  }
  else {
    names <- as.character(names)
    if (!(all(names %in% inames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      inames <- inames[1]
    }
    else {
      inames <- names
    }
  }
  
  nvi <- length(inames)
  nvr <- length(rnames)
  ifelse(is.null(lty), lty <- c(1, 1, 2, 2), lty <- rep(lty, 
                                                        4)[1:4])
  ifelse(is.null(lwd), lwd <- c(1, 1, 1, 1), lwd <- rep(lwd, 
                                                        4)[1:4])
  ifelse(is.null(col), col <- c("black", "gray", 
                                "red", "red"), col <- rep(col, 4)[1:4])
  dataplot <- function(x, iname) {
    impulses <- x$irf[[iname]]
    range <- matrix(NA, nrow = ncol(x$Upper[[1]]), ncol = 2)
    axisrange <- range
    colnames(range) <- c("min","max")
    rownames(range) <- colnames(x$Upper[[1]])
    upper <- NULL
    lower <- NULL
    upper1 <- NULL
    lower1 <- NULL
    axistop <- NULL
    axisbot <- NULL
    if (x$boot) {
      for(i in 1:ncol(x$Upper[[iname]])) {
        upper1 <- x$Upper[[iname]][,i]
        lower1 <- x$Lower[[iname]][,i]
        range[i,] <- range(cbind(upper1, lower1))
      }
    }
    
    #Make the axis the same in all of the graphs
    bigrange <- range(range)
    #Total range
    size <- bigrange[2] - bigrange[1]
    bigrangeindex <- ifelse(bigrange[2] == max(abs(bigrange)), 2, 1)
    #Get ratios
    ratios <- abs(bigrange / size)
    
    #Want the ratios to be no greater than 1:4
    newratio = F
    if(ratios[1] / ratios[2] >= 4) {
      ratios[1] = .8
      ratios[2] = .2
      newratio = T
    }
    if(ratios[2] / ratios[1] >= 4) {
      ratios[2] = .8
      ratios[1] = .2
      newratio = T
    }
    #Biggest value
    myfun <- function(x) max(abs(x))
    furthest <- apply(range, 1, myfun)
    
    #Make the furthest one away show the entire range, and the furthest one in each double
    furthestsingle <- max(furthest)
    tehboi <- which(furthest == furthestsingle)
    varindex <- apply(range, 1, function(m) which(abs(m) == max(abs(m))))
    
    
    if(newratio == F) {
      for(i in 1:nrow(range)) {
        #The furthest away one has the full range
        if(i == tehboi) {
          axistop <- bigrange[2]
          axisbot <- bigrange[1]
        }
        #If min is biggest
        else if(range[i,1]/bigrange[1] >= range[i,2]/ bigrange[2]) {
          axistop <- abs(furthest[i] * ratios[2] / ratios[1])
          axisbot <- -furthest[i]
        }
        #If max is biggest
        else if(range[i,1]/bigrange[1] < range[i,2]/ bigrange[2]) {
          axistop <- range[i,2]
          axisbot <- -abs(furthest[i] * ratios[1] / ratios[2])
        }
        axisrange[i,] <- c(axisbot, axistop)
      }
    }
    if(newratio == T) {
      for(i in 1:nrow(range)) {
        #The furthest one away has the full range
        if(i == tehboi) {
          axisbot <- -abs(ifelse(bigrangeindex == 1, bigrange[1], (bigrange[2] *ratios[1] / ratios[2])))
          axistop <- abs(ifelse(bigrangeindex == 2, bigrange[2], (bigrange[1] *ratios[2] / ratios[1])))
        }
        else if(range[i,1]/bigrange[1] >= range[i,2]/ bigrange[2]) {
          axistop <- abs(furthest[i] * ratios[2] / ratios[1])
          axisbot <- -furthest[i]
        }
        else if(range[i,1]/bigrange[1] < range[i,2]/ bigrange[2]) {
          axistop <- furthest[i]
          axisbot <- -abs(furthest[i] * ratios[1] / ratios[2])
        }
        axisrange[i,] <- c(axisbot, axistop)
      }
    }
    #      else if(abs(range[i,2]) == furthest[i]) {
    
    
    if (x$boot) {
      upper <- x$Upper[[iname]]
      lower <- x$Lower[[iname]]
    }
    if ((x$model == "varest") || (x$model == "vec2var")) {
      if (x$ortho) {
        text1 <- paste("Orthogonal Impulse Response from", 
                       iname, sep = " ")
      }
      else {
        text1 <- paste("Impulse Response from", 
                       iname, sep = " ")
      }
    }
    else if (x$model == "svarest") {
      text1 <- paste("SVAR Impulse Response from", 
                     iname, sep = " ")
    }
    else if (x$model == "svecest") {
      text1 <- paste("SVECM Impulse Response from", 
                     iname, sep = " ")
    }
    if (x$cumulative) 
      text1 <- paste(text1, "(cumulative)", sep = " ")
    text2 <- ""
    if (x$boot) 
      text2 <- paste((1 - x$ci) * 100, "% Bootstrap CI, ", 
                     x$runs, "runs")
    result <- list(impulses = impulses, upper = upper, lower = lower, 
                   range = range, text1 = text1, text2 = text2, axisrange = axisrange)
    return(result)
  }
  plot.multiple <- function(dp, nc = nc, ...) {
    x1 <- dp1$impulses
    y1 <- dp1$upper
    z1 <- dp1$lower
    x2 <- dp2$impulses
    y2 <- dp2$upper
    z2 <- dp2$lower
    
    
    ifelse(is.null(main), main <- dp1$text1, main <- main)
    ifelse(is.null(sub), sub <- dp1$text2, sub <- sub)
    ifelse(is.null(ylim), ylim1 <- dp1$axisrange, ylim1 <- ylim)
    if(is.null(ylim)) {
      mins <- apply(cbind(dp1$axisrange[,1], dp2$axisrange[,1]), 1, min)
      maxs <- apply(cbind(dp1$axisrange[,2], dp2$axisrange[,2]), 1, max)
      ylim <- matrix(c(mins,maxs), ncol = 2)
    }
    range1 <- range(c(x1, y1, z1))
    range2 <- range(c(x2, y2, z2))
    nvr <- ncol(x1)
    if (missing(nc)) {
      nc <- ifelse(nvr > 4, 2, 1)
    }
    nr <- ceiling(nvr/nc)
    par(mfrow = c(nr, nc), mar = mar.multi, oma = oma.multi, bg = "lightgray")
    if (nr > 1) {
      for (i in 1:(nvr - nc)) {
        ifelse(is.null(ylab), ylabel <- colnames(x1)[i], 
               ylabel <- ylab[i])
        xy1 <- xy.coords(x1[, i])
        xy2 <- xy.coords(x2[, i])
        plot(xy1, axes = FALSE, type = "n", ylab = ylabel, 
             ylim = ylim[i,], ...)
        lines(x = xy1$x, y = xy1$y, col = col[1], lty = lty[1], lwd = lwd[1], ...)
        lines(x = xy2$x, y = xy2$y, col = col[1], lty = lty[3], lwd = lwd[1], ...)
        axis(2, at = pretty(ylim[i,])[-1])
        abline(h = 0, col = "red")
        if (!is.null(y)) 
          lines(y[, i], col = col[3], lty = lty[3], lwd = lwd[3])
        if (!is.null(z)) 
          lines(z[, i], col = col[3], lty = lty[3], lwd = lwd[3])
        box()
      }
      for (j in (nvr - nc + 1):nvr) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
               ylabel <- ylab[j])
        xy1 <- xy.coords(x1[, j])
        xy2 <- xy.coords(x2[, j])
        plot(xy1, axes = FALSE, type = "n", ylab = ylabel, 
             ylim = ylim[j,], ...)
        lines(x = xy1$x, y = xy1$y, col = col[1], lty = lty[1], lwd = lwd[1], ...)
        lines(x = xy2$x, y = xy2$y, col = col[1], lty = lty[3], lwd = lwd[1], ...)
        axis(2, at = pretty(ylim[j,])[-1])
        axis(1, at = 1:(nrow(x)), labels = c(0:(nrow(x) - 
                                                  1)))
        box()
        abline(h = 0, col = "red")
        if (!is.null(y)) 
          lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        if (!is.null(z)) 
          lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      }
      mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext, ...)
      mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext, ...)
    }
    else {
      for (j in 1:nvr) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
               ylabel <- ylab[j])
        xy <- xy.coords(x[, j])
        plot(xy, type = "l", ylab = ylabel, ylim = ylim, 
             col = col[1], lty = lty[1], lwd = lwd[1], ...)
        if (!is.null(y)) 
          lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        if (!is.null(z)) 
          lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        abline(h = 0, col = "red")
      }
      mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext, ...)
      mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext, ...)
    }
  }
  
  if (plot.type == "multiple") {
    for (i in 1:nvi) {
      dp1 <- dataplot(x = irf1, iname = inames[i])
      dp2 <- dataplot(irf2, iname = inames[i])
      plot.multiple(dp1, dp2, nc = nc)#, ...)
      if (nvi > 1) 
        par(ask = TRUE)
    }
  }
}
bazplotirf.double(irf1 = preirf, irf2 = postirf, plot.type = "multiple", ylab = var.names.main)

irf1 <- preirf
irf2 <- postirf
#bazplotirf(mainirfortho1, plot.type = "multiple", ylab = var_names_fancy)
#plot(mainirfortho1, plot.type = "multiple")
#x <- mainirfortho1
plot.type = c("multiple") 
names = NULL
main = NULL
sub = NULL
lty = NULL
lwd = NULL 
col = NULL
ylim = NULL
ylab =  NULL
xlab = NULL
mar.multi = c(0,4, 0, 4)
oma.multi = c(6, 4, 6, 4)
adj.mtext = NA 
padj.mtext = NA
col.mtext = NA
