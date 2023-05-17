#' @title Calculate standardised energy indices
#'
#' @description Inputs a time series of a chosen variable (e.g. residual load or
#' energy demand) and returns a time series of standardised indices. Different
#' types of indices can be calculated, on any timescale that is of interest.
#'
#' @param x vector or time series to be converted to standardised indices.
#' @param distribution string; distribution used to construct the indices.
#' @param scale optional; numeric specifying the number of values to aggregate over.
#' @param index_type string; the type of index: "probability", "bounded" or "normal".
#'
#' @details
#' Details about the std_index function will be added here
#'
#' @return
#' Time series of standardised indices.
#'
#'
#' @references
#' Allen, S. and N. Otero (2022):
#' `Standardised indices to monitor energy droughts',
#' \emph{EarthArXiv preprint} 4752.
#' \doi{10.31223/X51S92}
#'
#' McKee, T. B., Doesken, N. J., & Kleist, J. (1993):
#' `The relationship of drought frequency and duration to time scales',
#' \emph{In Proceedings of the 8th Conference on Applied Climatology} 17, 179-183.
#'
#'
#' @author Sam Allen, Noelia Otero
#'
#' @examples
#' Examples of the std_index function will be added here
#'
#' @name std_index
#'
#' @export
std_index <- function(x, method=c("fitDis", "empirical", "none"), scale, index_type=c("probability", "bounded", "normal")){

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


