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
        axistop <- abs(range[i,1] * ratios[2] / ratios[1])
        axisbot <- range[i,1]
      }
      #If max is biggest
      else if(range[i,1]/bigrange[1] < range[i,2]/ bigrange[2]) {
        axistop <- range[i,2]
        axisbot <- -abs(range[i,2] * ratios[1] / ratios[2])
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
