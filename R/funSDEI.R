#' @title Calculation of Standardised indices to monitor energy droughts
#'
#' @description Given a time series of a variable, e.g., wind and solar power generation, returns a standardised indices
#' based on the method chose. The index can be calculate in any timescale that is of interest (e.g., hourly, daily.)
#' @param x the variable to be standarised
#' @param method estimation method
#' @param scale indicates whether the variable is aggregate over time
#' @references
#' Sam Allen and Noelia Otero. 2022. Standardised indices to monitor energy droughts.
#' @export
#'
funSDEI <- function(x, method=c("fitDis", "empirical", "none"), scale, index_type=c("probability", "bounded", "normal")){

  if (missing('method')){
    method <- 'fitDis'
  } else {
    method <- match.arg(method)
  }

  if (missing('index_type')){
    method <- 'normal'
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
    if (index_type == "probability"){
      SDEI <- p
    }else if (index_type == "bounded"){
      SDEI <- 2*p - 1
    }else {
      SDEI <- qnorm(p,0,1)
    }
    return(list("SDEI"= data.frame(date=x[,1], SDEI=SDEI), "infodis"= fdat[[1]]))
  }else if (method == "empirical"){

    n   <- length(x[,2]);
    EP  <- ecdf(x[,2])  # Get the empirical probability
    p <- (1 + EP(x[,2])*n)/(n + 2) # # Use the Weibull plotting position to ensure values not equal to 0 or 1
    if (index_type == "probability"){
      SDEI <- p
    }else if (index_type == "bounded"){
      SDEI <- 2*p - 1
    }else {
      SDEI <- qnorm(p,0,1)
    }
    return(list("SDEI"= data.frame(date=x[,1], SDEI=SDEI)))
  }else if (method == "none"){
    # extract raw data
    colnames(x) <- c("date", "SDEI")
    return(list("SDEI"= x))
  }


}


