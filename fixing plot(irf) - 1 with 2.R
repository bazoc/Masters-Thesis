#Things wrong with this, related:
#1) Won't workwith different length variables
#Should work now with different ordering 
bazplotirf.allinone.double <- function (irf.withci = NULL, irf.noci = NULL, plot.type = c("multiple"), 
                        names = NULL, main = NULL, sub = NULL, lty = NULL, lwd = NULL, 
                        col = NULL, ylim = NULL, ylab = NULL, xlab = NULL, nc, mar.multi = c(.5, 
                                                                                             2, .5, 1), oma.multi = c(6, 5, 6, 1), adj.mtext = NA, 
                        padj.mtext = NA, col.mtext = NA, impnames = NULL, resnames = NULL, cause = NULL, legendbot = NULL, confon = T, ...) 
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
  ifelse(is.null(legendbot), legendbot <- c("IRF with CI", "IRF no CI", "95% CI of IRF"), legendbot <- legendbot)
  
  if(all(sort(inames1) ==sort(inames2)) & !(all(inames1 ==inames2))) { #Same variables different order
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
      if(confon == T) {
      polygon(c(xy1$x, rev(xy1$x)),
              c(y1[ ,j],rev(z1[ ,j])),
              col = "lightgrey", border = NA)
      }
      abline(h = 0, col = "red")
      
      lines(x = xy1$x, y = xy1$y, col = col[1], lty = lty[1], lwd = lwd[1])#, ...)
      if(!(j %in% differ)) {
        lines(x = xy2$x, y = xy2$y, col = "blue", lty = lty[3], lwd = lwd[1])#, ...)
      }
      if(graphnum == nvr) {
        axis(1, at = NULL, cex = .0001, las = 1)
      }
      axis(2, at = NULL, cex = .0001, las = 1)
      #if (!is.null(y1)) 
      #  lines(y1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      #if (!is.null(z1)) 
      #  lines(z1[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      #abline(h = 0, col = "red")
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
      plot(xy, type = "n", ylim = ylim[j,], axes = F, 
           col = col[1], ylab = NA, lty = lty[1], lwd = lwd[1], xlab = NA)#, ...)
      if(confon == T) {
      polygon(c(xy$x, rev(xy$x)),
              c(y[ ,j],rev(z[ ,j])),
              col = "lightgrey", border = NA)
      }
      abline(h = 0, col = "red")
      lines(x = xy$x, y = xy$y, col = col[1], lty = lty[1], lwd = lwd[1])#, ...)
      
      
      axis(2, at = NULL, cex = .0001, las = 1)
      if(graphnum == nvr) {
        axis(1, at = NULL, cex = .0001, las = 1)
        
      }
      
      #if (!is.null(y)) 
      #  lines(y[, j], col = col[3], lty = lty[3], lwd = lwd[3])
      #if (!is.null(z)) 
      #  lines(z[, j], col = col[3], lty = lty[3], lwd = lwd[3])
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
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
  legend('bottom',legend = c(legendbot) ,col = c("black", "blue", "grey"), 
         lwd = 2, lty = c(1, 3, -1), pch = c(-1, -1, 15), xpd = TRUE, cex = 1.2, seg.len=3, bty = 'n')
}
#bazplotirf.allinone.double(irf.withci = allirfs.multiple$assets, irf.noci = allirfs.multiple$main, plot.type = "multiple", 
#                           ylab = var.names.assets.graph, main = "poodidyscoop", lwd = 1.9)
#dev.off()
#load(file = "~/Thesis/Data/All Multiple IRFs.Rdata")
#irf.withci = allirfs.multiple$nogreece
#irf.noci = allirfs.multiple$main
#ylab = var.names.main
#oma.multi = c(6, 5, 6, 1)
#mar.multi = c(.5, 2, .5, 1)