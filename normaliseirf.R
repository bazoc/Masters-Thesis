#Need to find out what makes the first one equal .1, then divide all other things by it
normaliseirf <- function(x) {
  inames <- names(x$irf)
  tmp1 <- x
  if(!is.null(inames)) {
    for(i in 1:length(inames)) {
      if(inames[i] == "demeaned_int") {
        invnum <- 1/ x$irf[[i]][1,inames[i]] 
        tmp1$irf[[i]] <- x$irf[[i]] * invnum * 10 #Makes first shock  10 (percent for interest) and normalises all other columns
        tmp1$Lower[[i]] <- x$Lower[[i]] * invnum * 10 #Makes first shock 10 (percent for interest) and normalises all other columns
        tmp1$Upper[[i]] <- x$Upper[[i]] * invnum * 10 #Makes first shock 10 (percent for interest) and normalises all other columns
      }
      else {
        invnum <- 1/ x$irf[[i]][1,inames[i]] 
        tmp1$irf[[i]] <- x$irf[[i]] * invnum / 10 #Makes first shock .1 and normalises all other columns
        tmp1$Lower[[i]] <- x$Lower[[i]] * invnum / 10 #Makes first shock .1 and normalises all other columns
        tmp1$Upper[[i]] <- x$Upper[[i]] * invnum / 10 #Makes first shock .1 and normalises all other columns
      }
    }
  }
  else{
    vnames <- names(x)
    for(i in 1:length(vnames)) {
      if(vnames[i] == "demeaned_int") {
        impmat <- x[[vnames[i]]]
        impname <- names(impmat$irf)
        invnum <- 1/ impmat$irf[[1]][1, impname]
        tmp1[[vnames[i]]]$irf[[1]] <- impmat$irf[[1]] * invnum * 10 #Makes first shock  10 (percent for interest) and normalises all other columns
        tmp1[[vnames[i]]]$Lower[[1]] <- impmat$Lower[[1]] * invnum * 10 #Makes first shock 10 (percent for interest) and normalises all other columns
        tmp1[[vnames[i]]]$Upper[[1]] <- impmat$Upper[[1]] * invnum * 10 #Makes first shock 10 (percent for interest) and normalises all other columns
        
      }
      else {
        impmat <- x[[vnames[i]]]
        impname <- names(impmat$irf)
        invnum <- 1/ impmat$irf[[1]][1, impname]
        tmp1[[vnames[i]]]$irf[[1]] <- impmat$irf[[1]] * invnum / 10 #Makes first shock  10 (percent for interest) and normalises all other columns
        tmp1[[vnames[i]]]$Lower[[1]] <- impmat$Lower[[1]] * invnum / 10 #Makes first shock 10 (percent for interest) and normalises all other columns
        tmp1[[vnames[i]]]$Upper[[1]] <- impmat$Upper[[1]] * invnum / 10 #Makes first shock 10 (percent for interest) and normalises all other columns
      }
    }
  }
  return(tmp1)
}