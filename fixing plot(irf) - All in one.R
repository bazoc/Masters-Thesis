#mainirfortho1 <- bazirf.varest(fevar.main, n.ahead = 40,impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)
bazplotirf.allinone <- function (x, plot.type = c("multiple"), 
                        names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
                        col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 
                                                                                             4, 0, 4), oma.multi = c(6, 4, 6, 4), adj.mtext = NA, 
                        padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, cause = NULL, ...) 
{
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  plot.type <- match.arg(plot.type)
  inames <- x$impulse
  rnames <- x$response
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
  plot.multiple <- function(dp, nc = nc, ...) {
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
    #par(mfrow = c(nr, nc), mar = mar.multi, oma = oma.multi, bg = "lightgray")
    
    
    #if (nr > 1) {
    #for (i in 1:(nvr - nc)) {
    #  ifelse(is.null(ylab), ylabel <- colnames(x)[i], 
    #         ylabel <- ylab[i])
    #  xy <- xy.coords(x[, i])
    #  plot(xy, axes = FALSE, type = "l", ylab = ylabel, 
    #       ylim = ylim[i,], col = col[1], lty = lty[1], lwd = lwd[1])#, ...)
    #  axis(2, at = pretty(ylim[i,])[-1])
    #  abline(h = 0, col = "red")
    #  if (!is.null(y)) 
    #    lines(y[, i], col = col[3], lty = lty[3], lwd = lwd[3])
    #  if (!is.null(z)) 
    #    lines(z[, i], col = col[3], lty = lty[3], lwd = lwd[3])
    #  box()
    #}
    # for (j in (nvr - nc + 1):nvr) {
    #    ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
    #           ylabel <- ylab[j])
    #    xy <- xy.coords(x[, j])
    #   plot(xy, axes = FALSE, type = "l", ylab = ylabel, 
    #         ylim = ylim[j,], col = col[1], lty = lty[1], lwd = lwd[1])#, 
    #        # ...)
    #    axis(2, at = pretty(ylim[j,])[-1])
    #    axis(1, at = 1:(nrow(x)), labels = c(0:(nrow(x) - 
    #                                              1)))
    #    box()
    #   abline(h = 0, col = "red")
    #    if (!is.null(y)) 
    #      lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
    #    if (!is.null(z)) 
    #      lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
    #  }
    #  mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
    #        padj = padj.mtext, col = col.mtext)#, ...)
    #  mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
    #        padj = padj.mtext, col = col.mtext)#, ...)
    #}
    if(nvr < 100) {
      for (j in 1:nvr) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
               ylabel <- ylab[j])
        xy <- xy.coords(x[, j])
        plot(xy, type = "l", ylab = NULL, ylim = ylim[j,], 
             col = col[1], lty = lty[1], lwd = lwd[1])#, ...)
        
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
        
      }
      mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext)#, ...)
      mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext)#, ...)
    }
  }
  
  if (plot.type == "multiple") {
    par(mfrow = c(nvr, nvi), mar = mar.multi, oma = oma.multi, bg = "lightgray")
    for (i in 1:nvi) {
      dp <- dataplot(x, iname = inames[i])
      graphnum = i
      plot.multiple(dp, nc = nc, graphnum = graphnum, cause = cause, ylab = ylab, ...)
    }
  }
}
#dev.off()
#bazplotirf.allinone(mainirfortho1, plot.type = "multiple", mar = c(1,1,1,1), cause = NULL, ylab = var.names.main)


#plot(irf.main.ortho.1, plot.type = "multiple")
#x <- irf.main.ortho.1
#plot.type = c("multiple") 
#names = NULL
#main = NULL
#sub = NULL
#lty = NULL
#lwd = NULL 
#col = NULL
#ylim = NULL
#ylab = var.names.fancy.main
#xlab = NULL

#mar.multi = c(0,4, 0, 4)
#oma.multi = c(6, 4, 6, 4)
#adj.mtext = NA 
#padj.mtext = NA
#col.mtext = NA
#iname = inames[i]