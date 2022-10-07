#' Function to calculate the index
#' @param x the variable to be standarised
#' @param method estimation method
#' @param scale indicates whether the variable is aggregate over time
#' @export
#'
funSDEI <- function(x, method=c("fitDis", "empirical", "none"), scale){

  if (missing('method')){
    method <- 'fitDis'
  } else {
    method <- match.arg(method)
  }

  if (missing('scale')){
    scale <- NULL
  }

  if (!is.null(scale)){

    dat.xts <- xts::xts(x[,2],as.POSIXct(x[,1]))
    # endpoints(x[,1],'weeks',scale/(7*24)) does not give same output as endpoints(x[,1],'hours',scale)
    if(scale %% (7*24) == 0){
      ends <- endpoints(x[,1],'weeks',scale/(7*24))
    }else{
      ends <- endpoints(x[,1],'hours',scale)
    }

    new.x <- period.apply(dat.xts,ends,sum)
    new.x <- data.frame(index(new.x), coredata(new.x))
    colnames(new.x) <- colnames(x)

    # remove last entry if it corresponds to an incomplete day or week
    if(length(unique(diff(ends))) != 1){
      x <- new.x[-nrow(new.x), ]
    }else{
      x <- new.x
    }

  }

  if (method == "fitDis"){
    fdat  <- fitDis(x[,2], "none")
    p     <- fdat$pnon
    SDEI <-qnorm(p,0,1)
    return(list("SDEI"= data.frame(date=x[,1], SDEI=SDEI), "infodis"= fdat[[1]]))

  }else if (method == "empirical"){

    n   <- length(x[,2]);
    EP  <- ecdf(x[,2])  # Get the empirical probability
    p  <- EP(x[,2])*n/(n+1)  # Use the Weibull plotting position
    SDEI <- qnorm(p,0,1)
    return(list("SDEI"= data.frame(date=x[,1], SDEI=SDEI)))
  }else if (method == "none"){
    # extract raw data
    colnames(x) <- c("date", "SDEI")
    return(list("SDEI"= x))
  }


}


