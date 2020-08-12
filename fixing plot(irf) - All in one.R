#mainirfortho1 <- bazirf.varest(fevar.main, n.ahead = 20,impulse = "demeaned_int", ortho = T, ci = .95, runs = 1000, seed = 253)
bazplotirf.allinone <- function (x, plot.type = c("multiple"), 
                        names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
                        col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(.5, 
                                                                                             2, .5, 1), oma.multi = c(6, 5, 6, 1), adj.mtext = NA, 
                        padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, cause = NULL, legendbot = NULL,...) 
{
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  plot.type <- match.arg(plot.type)
  inames <- x$impulse
  rnames <- x$response
  ifelse(is.null(legendbot), legendbot <- c("IRF"), legendbot <- legendbot)
  
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
    
    for (j in 1:nvr) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[j], 
               ylabel <- ylab[j])
        xy <- xy.coords(x[, j])
        plot(xy, type = "n", ylim = ylim[j,], axes = F, 
             col = col[1], ylab = NA, lty = lty[1], lwd = lwd[1], xlab = NA)#, ...)
        abline(h = 0, col = "red")
        lines(x = xy$x, y = xy$y, col = col[1], lty = lty[1], lwd = lwd[1], ...)
        
        axis(2, at = NULL, cex = .0001, las = 1)
        if(graphnum == nvr) {
          axis(1, at = NULL, cex = .0001, las = 1)
          
        }
        
        if (!is.null(y)) 
          lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        if (!is.null(z)) 
          lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
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
      #mtext(sub, 1, line = 4, outer = TRUE, adj = adj.mtext, 
      #      padj = padj.mtext, col = col.mtext)#, ...)
  }
  
  if (plot.type == "multiple") {
    par(mfrow = c(nvr, nvi), mar = mar.multi, oma = oma.multi, bg = "white")
    for (i in 1:nvi) {
      dp <- dataplot(x, iname = inames[i])
      graphnum = i
      plot.multiple(dp, nc = nc, graphnum = graphnum, cause = cause, ylab = ylab, ...)
    }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',legend = c(legendbot, "95% Bootstrapped C.I. - 1000 Runs") ,col = c("black", "red"), lwd = 2, lty = c(1, 3), xpd = TRUE, cex = 1.2, seg.len=3, bty = 'n')
}
#bazplotirf.allinone(temp, plot.type = "multiple", ylab = var.names.main, lwd = 1.6)
#dev.off()