#preirf <- bazirf.varest(fevar.pre, n.ahead = 40, impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)
#postirf <- bazirf.varest(fevar.post, n.ahead = 40, impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)

bazplotirf.double <- function (irf.withci = NULL, irf.noci = NULL, plot.type = c("multiple"), 
                        names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
                        col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 
                                                                                             4, 0, 4), oma.multi = c(6, 4, 6, 4), adj.mtext = NA, 
                        padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, legendbot = NULL, ...) 
{
  if(is.null(irf.withci) | is.null(irf.noci)) {
    stop("\nNeed two IRFs")
  }
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  plot.type <- match.arg(plot.type)
  inames <- irf.withci$impulse
  rnames <- irf.withci$response
  ifelse(is.null(legendbot), legendbot <- c("IRF with CI", "IRF no CI"), legendbot <- legendbot)
  
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
  plot.multiple <- function(dp1, dp2, nc = nc, ...) {
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
    par(mfrow = c(nr, nc), mar = mar.multi, oma = oma.multi, bg = "white")
    if (nr > 1) {
      for (i in 1:(nvr - nc)) {
        ifelse(is.null(ylab), ylabel <- colnames(x1)[i], 
               ylabel <- ylab[i])
        xy1 <- xy.coords(x1[, i])
        xy2 <- xy.coords(x2[, i])
        plot(xy1, axes = FALSE, type = "n", ylab = ylabel, 
             ylim = ylim[i,], ...)
        
        axis(2, at = pretty(ylim[i,])[-1], las = 1)
        polygon(c(xy1$x, rev(xy1$x)),
                c(y1[ ,i],rev(z1[ ,i])),
                col = "lightgrey", border = NA)
        abline(h = 0, col = "red")
        
        lines(x = xy1$x, y = xy1$y, col = col[1], lty = lty[1], lwd = lwd[1], ...)
        lines(x = xy2$x, y = xy2$y, col = "blue", lty = lty[3], lwd = lwd[1], ...)
        axis(2, at = pretty(ylim[i,])[-1], las = 1)

        #if (!is.null(y1)) 
        #  lines(y1[, i], col = col[3], lty = lty[3], lwd = lwd[3])
        #if (!is.null(z1)) 
        #  lines(z1[, i], col = col[3], lty = lty[3], lwd = lwd[3])
        box()
      }
      for (j in (nvr - nc + 1):nvr) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
               ylabel <- ylab[j])
        xy1 <- xy.coords(x1[, j])
        xy2 <- xy.coords(x2[, j])
        plot(xy1, axes = FALSE, type = "n", ylab = ylabel, 
             ylim = ylim[j,], ...)
        polygon(c(xy1$x, rev(xy1$x)),
                c(y1[ ,j],rev(z1[ ,j])),
                col = "lightgrey", border = NA)
        abline(h = 0, col = "red")
        
        lines(x = xy1$x, y = xy1$y, col = col[1], lty = lty[1], lwd = lwd[1], ...)
        lines(x = xy2$x, y = xy2$y, col = "blue", lty = lty[3], lwd = lwd[1], ...)
        axis(2, at = pretty(ylim[j,])[-1], las = 1)
        axis(1, at = NULL)
        box()
        #if (!is.null(y1)) 
        #  lines(y1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        #if (!is.null(z1)) 
        #  lines(z1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      }
      mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext, ...)
      #mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
      #      padj = padj.mtext, col = col.mtext, ...)
    }
    else {
      for (j in 1:nvr) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
               ylabel <- ylab[j])
        xy <- xy.coords(x[, j])
        plot(xy, type = "l", ylab = ylabel, ylim = ylim, 
             col = col[1], lty = lty[1], lwd = lwd[1], ...)
        if (!is.null(y)) 
          lines(y1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        if (!is.null(z)) 
          lines(z1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        abline(h = 0, col = "red")
      }
      mtext(main, 3, line = 2, outer = TRUE, adj = adj.mtext, 
            padj = padj.mtext, col = col.mtext, ...)
      #mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
      #      padj = padj.mtext, col = col.mtext, ...)
    }
  }
  
  if (plot.type == "multiple") {
    for (i in 1:nvi) {
      dp1 <- dataplot(x = irf.withci, iname = inames[i])
      dp2 <- dataplot(irf.noci, iname = inames[i])
      plot.multiple(dp1, dp2, nc = nc)#, ...)
      if (nvi > 1) 
        par(ask = TRUE)
    }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',legend = c(legendbot) ,col = c("black", "blue"), lwd = 2, lty = c(1, 3), xpd = TRUE, cex = 1.2, seg.len=3, bty = 'n')
}
#bazplotirf.double(irf.withci = preirf, irf.noci = postirf, plot.type = "multiple", ylab = var.names.main,
#                  legendbot = c("first", "second"), lwd = 2)

#irf.withci <- preirf
#irf.noci <- postirf
#bazplotirf(mainirfortho1, plot.type = "multiple", ylab = var_names_fancy)
#plot(mainirfortho1, plot.type = "multiple")
#x <- mainirfortho1
#plot.type = c("multiple") 
#names = NULL
#main = NULL
#sub = NULL
#lty = NULL
#lwd = NULL 
#col = NULL
#ylim = NULL
#ylab =  NULL
#xlab = NULL
#mar.multi = c(0,4, 0, 4)
#oma.multi = c(6, 4, 6, 4)
#adj.mtext = NA 
#padj.mtext = NA
#col.mtext = NA
