#Things wrong with this, related:
#1) Won't workwith different length variables
#Should work now with different ordering 
#mainirfortho1 <- bazirf.varest(fevar.main, n.ahead = 40,impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)
bazplotirf.allinone.double <- function (irf.withci = NULL, irf.noci = NULL, plot.type = c("multiple"), 
                        names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
                        col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(.5, 
                                                                                             2, .5, 1), oma.multi = c(6, 5, 6, 1), adj.mtext = NA, 
                        padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, cause = NULL, ...) 
{
  if(is.null(irf.withci) | is.null(irf.noci)) {
    stop("\nPlease provide IRFs")
  }
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  plot.type <- match.arg(plot.type)
  inames1 <- irf.withci$impulse
  rnames1 <- irf.withci$response
  inames2 <- irf.noci$impulse
  rnames2 <- irf.noci$impulse
  
  if(all(sort(inames1) ==sort(inames2)) & !(all(inames ==inames2))) { #Same variables different order
    orderinames <- order(inames1)
    for(i in length(irf.noci$irf)) {
      irf.noci$irf[[i]] <- irf.noci$irf[[i]][,orderinames]
      irf.noci$Lower[[i]] <- irf.noci$Lower[[i]][orderinames]
      irf.noci$Upper[[i]] <- irf.noci$Upper[[i]][orderinames]
      
    }
    irf.noci$irf <- irf.noci$irf[orderinames]
    irf.noci$Lower <- irf.noci$Lower[orderinames]
    irf.noci$Upper <- irf.noci$Upper[orderinames]
    inames2 <- inames1
    rnames2 <- rnames1
  }
  if (is.null(names)) {
    names <- inames1
  }
  else {
    names <- as.character(names)
    if (!(all(names %in% inames))) {
      warning("\nInvalid variable name(s) supplied, using first variable.\n")
      inames1 <- inames1[1]
    }
    else {
      inames1 <- names
    }
  }
  
  nvi <- length(inames1)
  nvr <- length(rnames1)
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
        #text1 <- paste("Orthogonal Impulse Response from", 
        #               iname, sep = " ")
        text1 = iname
      }
      else {
        #text1 <- paste("Impulse Response from", 
        #               iname, sep = " ")
        text1 = iname
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
  plot.multiple.double <- function(dp, nc = nc, ...) {
    x1 <- dp1$impulses
    y1 <- dp1$upper
    z1 <- dp1$lower
    x2 <- dp2$impulses
    y2 <- dp2$upper
    z2 <- dp2$lower
    
    ifelse(is.null(main), main <- "Orthogonalised Impulse Responses", main <- main)
    ifelse(is.null(sub), sub <- dp1$text2, sub <- sub)
    ifelse(is.null(ylim), ylim1 <- dp1$axisrange, ylim1 <- ylim)
    ifelse(is.null(cause), cause <- ylab[graphnum], main <- main)
    
    if(is.null(ylim)) {
      mins <- apply(cbind(dp1$axisrange[,1], dp2$axisrange[,1]), 1, min)
      maxs <- apply(cbind(dp1$axisrange[,2], dp2$axisrange[,2]), 1, max)
      ylim <- matrix(c(mins,maxs), ncol = 2)
    }
    
    ylim[differ,] <- dp1$axisrange[differ,]
    nvr <- ncol(x1)
    #setting individual rows to show reponses from individual shocks
    
    if (missing(nc)) {
      #Want number of columns to equal number of variables
      nc <- nvr
    }
    #Square so number of rows also equals number of variables
    #nr <- ceiling(nvr/nc) old
    nr <- nvr
    
    for (j in 1:nvr) {
      ifelse(is.null(ylab), ylabel <- colnames(x1)[j], 
             ylabel <- ylab[j])
      xy1 <- xy.coords(x1[, j])
      xy2 <- xy.coords(x2[, j])
      plot(xy1, axes = FALSE, type = "n", ylab = NA, 
           ylim = ylim[j,], xlab = NA)#, ...)
      lines(x = xy1$x, y = xy1$y, col = col[1], lty = lty[1], lwd = lwd[1])#, ...)
      if(!(j %in% differ)) {
        lines(x = xy2$x, y = xy2$y, col = col[1], lty = lty[3], lwd = lwd[1])#, ...)
      }
      axis(2, at = NULL, cex = .0001, las = 1)
      abline(h = 0, col = "red")
      if (!is.null(y1)) 
        lines(y1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      if (!is.null(z1)) 
        lines(z1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      abline(h = 0, col = "red")
      if(graphnum == 1) {
        mtext(ylab[j], 3, line = 1, outer = F, adj = adj.mtext, 
              padj = padj.mtext, col = col.mtext)#, ...)
      }
      if(j == 1) {
        mtext(cause, 2, line = 3, outer = FALSE, adj = adj.mtext, 
              padj = padj.mtext, col = col.mtext)#, ...)
      }
        box()
      }
      mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext)#, ...)
      mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext)#, ...)
  }
  plot.multiple.single <- function(dp, nc = nc, ...) {
    x <- dp$impulses
    y <- dp$upper
    z <- dp$lower
    ifelse(is.null(main), main <- "Orthogonalised Impulse Responses", main <- main)
    ifelse(is.null(cause), cause <- ylab[graphnum], main <- main)
    ifelse(is.null(sub), sub <- dp$text2, sub <- sub)
    ifelse(is.null(ylim), ylim <- dp$axisrange, ylim <- ylim)
    range <- range(c(x, y, z))
    nvr <- ncol(x)
    #setting individual rows to show reponses from individual shocks
    
    if (missing(nc)) {
      #Want number of columns to equal number of variables
      nc <- nvr
    }
    #Square so number of rows also equals number of variables
    #nr <- ceiling(nvr/nc) old
    nr <- nvr
    
    for (j in 1:nvr) {
      ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
             ylabel <- ylab[j])
      xy <- xy.coords(x[, j])
      plot(xy, type = "l", ylim = ylim[j,], axes = F, 
           col = col[1], ylab = NA, lty = lty[1], lwd = lwd[1], xlab = NA)#, ...)
      
      axis(2, at = NULL, cex = .0001, las = 1)
      if(graphnum == nvr) {
        axis(1, at = NULL, cex = .0001, las = 1)
        
      }
      
      if (!is.null(y)) 
        lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      if (!is.null(z)) 
        lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      abline(h = 0, col = "red")
      if(graphnum == 1) {
        mtext(ylab[j], 3, line = 1, outer = F, adj = adj.mtext, 
              padj = padj.mtext, col = col.mtext)#, ...)
      }
      if(j == 1) {
        mtext(cause, 2, line = 3, outer = FALSE, adj = adj.mtext, 
              padj = padj.mtext, col = col.mtext)#, ...)
      }
      box()
    }
    mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
          padj = padj.mtext, col = col.mtext)#, ...)
    mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
          padj = padj.mtext, col = col.mtext)#, ...)
  }
  
  if (plot.type == "multiple") {
    par(mfrow = c(nvr, nvi), mar = mar.multi, oma = oma.multi, bg = "lightgray")
    differ = which(inames1 != inames2)
    for (i in 1:nvi) {
      if(!(i %in% differ)) {
        dp1 <- dataplot(irf.withci, iname = inames1[i])
        dp2 <- dataplot(irf.noci, iname = inames2[i])
        graphnum = i
        plot.multiple.double(dp1, dp2, nc = nc, graphnum = graphnum, cause = cause, ylab = ylab, differ = differ, ...)
      }
      else {
        dp <- dataplot(irf.withci, iname = inames1[i])
        graphnum = i
        plot.multiple.single(dp, nc = nc, graphnum = graphnum, cause = cause, ylab = ylab, ...)
      }
    }
  }
}
bazplotirf.allinone.double(irf.withci = irf.assets.ortho, irf.noci = allirfs.multiple$main, plot.type = "multiple", ylab = var.names.assets)
#dev.off()
#load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")
#irf.withci = allirfs.multiple$nogreece
#irf.noci = allirfs.multiple$main
#ylab = var.names.main
#oma.multi = c(6, 5, 6, 1)
#mar.multi = c(.5, 2, .5, 1)