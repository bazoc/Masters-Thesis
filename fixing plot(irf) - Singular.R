#mainirfortho1 <- bazirf.varest(fevar.main, impulse = "demeaned_lhou", n.ahead = 20, ortho = T, ci = .95, runs = 1000, seed = 253)
bazplotirf <- function (x, plot.type = c("multiple"), 
          names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
          col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(0, 
                                                                               4, 0, 4), oma.multi = c(6, 4, 6, 4), adj.mtext = NA, 
          padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, legendbot = NULL,  ...) 
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
    ifelse(is.null(main), main <- dp$text1, main <- main)
    ifelse(is.null(sub), sub <- dp$text2, sub <- sub)
    ifelse(is.null(ylim), ylim <- dp$axisrange, ylim <- ylim)
    range <- range(c(x, y, z))
    nvr <- ncol(x)
    if (missing(nc)) {
      nc <- ifelse(nvr > 4, 2, 1)
    }
    nr <- ceiling(nvr/nc)
    par(mfrow = c(nr, nc), mar = mar.multi, oma = oma.multi, bg = "lightgray")
    if (nr > 1) {
      for (i in 1:(nvr - nc)) {
        ifelse(is.null(ylab), ylabel <- colnames(x)[i], 
               ylabel <- ylab[i])
        xy <- xy.coords(x[, i])
        plot(xy, axes = FALSE, type = "l", ylab = ylabel, 
             ylim = ylim[i,], col = col[1], lty = lty[1], lwd = lwd[1], ...)
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
        xy <- xy.coords(x[, j])
        plot(xy, axes = FALSE, type = "l", ylab = ylabel, 
             ylim = ylim[j,], col = col[1], lty = lty[1], lwd = lwd[1], 
             ...)
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
          lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
        if (!is.null(z)) 
          lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
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
      dp <- dataplot(x, iname = inames[i])
      plot.multiple(dp, nc = nc, ...)
      if (nvi > 1) 
        par(ask = TRUE)
    }
  }
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',legend = c(legendbot, "95% Bootstrapped C.I. - 1000 Runs") ,col = c("black", "red"), lwd = 2, lty = c(1, 3), xpd = TRUE, cex = 1.2, seg.len=3, bty = 'n')
}
bazplotirf(mainirfortho1, plot.type = "multiple", ylab = var.names.main)
#x <- mainirfortho1
iname = "demeaned_lhou"
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
#ylab = var_names_fancy
#xlab = NULL

#mar.multi = c(0,4, 0, 4)
#oma.multi = c(6, 4, 6, 4)
#adj.mtext = NA 
#padj.mtext = NA
#col.mtext = NA
