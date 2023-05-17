#' @title Calculate distribution parameters and statistics
#'
#' @description This function calculates the distribution parameters and a number
#'  of statistics for a numeric vector and returns them as a named vector.
#'  This function is used internally by the standardized.index function or
#'  can be used to provide input to it.
#'
#' @param data vector of data
#' @param distr character string specifying the distribution, see details
#' @param method distribution fitting method, see details
#' @param na.thres maximum percentage of NA values allowed in data, default = 10\%
#'
#' @details
#' This has been adapted from code available at
#' \url{https://github.com/WillemMaetens/standaRdized}.
#'
#' Supported distributions are: gamma ('gamma'), 3-parameter gamma ('gamma3'), Weibull ('weibull'), 3-parameter Weibull ('weibull3'),
#' Generalized Extreme Value ('gev'), and Generalized Logistic ('glogis').
#' Supported distribution fitting methods are: Maximum Likelihood Estimation ('mle', default for 'gamma','weibull','gev' and 'glogis') and L-Moments ('lmom', default for 'gamma3' and 'weibull3').
#' 'mle' is not supported for distributions 'gamma3' and 'weibull3'. For distr = 'glogis' and method = 'lmom', the 'glo' distribution from package 'lmomco' is used, and its parameters are returned.
#'
#' @return a named vector containing:
#' \itemize{
#' \item{fitted distribution parameters, parameters are distribution-specific}
#' \item{prob.zero}{empirical probability of zeros in the data, used in SI calculation with with distributions not including zero such as gamma and Weibull}
#' \item{n.obs}{the total number of observations in the data (including NA values)}
#' \item{n.na}{the number of NA values in the data}
#' \item{pct.na}{the percentage of NA values in the data}
#' \item{ks.pval}{p-value for a two-sided Kolmogorov-Smirnov test that data comes form the fitted distribution}
#' \item{ad.pval}{p-value for a two-sided Anderson-Darling test that data comes form the fitted distribution}
#' The data (without NA values) and distr, method and na.thres settings are added to the result as addditional attributes.
#' }
#'
#'
#' @examples
#' data(Ukkel_RR)
#' # calculate the total rainfall for all months June
#' monthly.precipitation <- apply.monthly(x=Ukkel_RR,FUN=sum)
#' data <- c(coredata(monthly.precipitation[format(index(monthly.precipitation),'%m')=='06']))
#' # fit gamma distribution to the data
#' fit <- fit.distribution(data=data,distr='gamma',method='mle')
#' fitplot(x=fit,main='June precipitation',xlab='precipitation (mm)')
#' # fit gev distribution to the data
#' fit <- fit.distribution(data=data,distr='gev',method='mle')
#' fitplot(x=fit,main='June precipitation',xlab='precipitation (mm)')
#' # fit glogis distribution to the data
#' fit <- fit.distribution(data=data,distr='glogis',method='mle')
#' fitplot(x=fit,main='June precipitation',xlab='precipitation (mm)')
#'
#' @importFrom stats dgamma pgamma qgamma dweibull pweibull qweibull ks.test
#' @importFrom fitdistrplus fitdist
#' @importFrom lmomco pwm.ub pwm2lmom are.lmom.valid pargam parpe3 parwei parwei pargev parglo
#' @importFrom goftest ad.test
#'
#' @name fit_dist
NULL

#' @rdname fit_dist
#' @export
fit_dist <- function(data,distr,method=c('mle','lmom'),na.thres=10){
  distr <- match.arg(arg=distr,choices=c('gamma','gamma3','weibull','weibull3','gev','glogis'))

  if (distr %in% c('gamma3','weibull3') & missing('method')){
    method <- 'lmom'
  } else {
    method <- match.arg(method)
  }

  # parameter definitions
  if(distr=='gamma'){
    params <- c(shape=as.numeric(NA),rate=as.numeric(NA))
  } else if (distr=='gamma3'){
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA),thres=as.numeric(NA))
  } else if (distr=='weibull'){
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA))
  } else if (distr=='weibull3') {
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA),thres=as.numeric(NA))
  } else if (distr=='gev'){
    params <- c(shape=as.numeric(NA),scale=as.numeric(NA),location=as.numeric(NA))
  } else if (distr=='glogis'){
    if (method=='mle'){
      params <- c(shape=as.numeric(NA),scale=as.numeric(NA),location=as.numeric(NA))
    } else {
      distr <- 'glo' # change name to conform with lmomco package definition
      params <- c(xi=as.numeric(NA),alpha=as.numeric(NA),kappa=as.numeric(NA))
    }
  }

  # initialize data properties and goodness-of-fit statistics vectors
  fit.props <- c(prob.zero=as.numeric(NA),
                 n.obs=as.integer(NA),
                 n.na=as.numeric(NA),
                 pct.na=as.numeric(NA),
                 ks.pval=as.numeric(NA),
                 ad.pval=as.numeric(NA))

  # determine n.obs
  fit.props['n.obs'] <- as.integer(length(data))
  # check data for NA values and omit if necessary
  if(length(data)!=0){
    fit.props['n.na'] <- length(data[which(is.na(data))])
    fit.props['pct.na'] <- (fit.props['n.na']/fit.props['n.obs'])*100
    data <- data[!is.na(data)]
  }
  # determine prob.zero
  fit.props['prob.zero'] <- length(which(data==0))/length(data)

  # check na.thres
  if(fit.props['pct.na'] >= na.thres | length(data)==0){
    # NA output
    res <- c(params,fit.props)
    attributes(res) <- c(attributes(res),list(data=data,distr=distr,method=method,na.thres=na.thres))
    return(res)
  }

  # check data for zeros or negative values in case of gamma or weibull distribution
  if (distr == 'gamma' | distr=='weibull'){
    if(any(data<0)){
      warning(paste(distr,'distribution: all data values must be zero or positive, distribution not fitted'))
      return(c(params,fit.props)) # NA output
    }
    data <- data[which(data!=0)]
  }

  # fit distribution to data

  # maximum likelihood estimation parameter fit
  if (method=='mle'){
    # provide start estimates for the distribution fitting
    if(distr=='gamma3' | distr=='weibull3'){
      stop(paste('method mle not supported for ',distr,', use method lmom',sep=''))
    } else if (distr=='gev' | distr=='glogis'){
      start <- list(shape=1,scale=1,location=0)
    } else {
      start <- NULL
    }
    # fit distribution
    fit <- try(fitdistrplus::fitdist(data=data,distr=distr,method='mle',start=start))
    if(!inherits(fit,'try-error')){
      params <- fit$estimate
    } else {
      warning('distribution fitting failed')
      return(c(params,fit.props)) # NA output
    }
  }

  # L-moments parameter fit
  if (method=='lmom'){
    pwm <- lmomco::pwm.ub(data)
    lmom <- lmomco::pwm2lmom(pwm)
    if (!lmomco::are.lmom.valid(lmom) | is.na(sum(lmom[[1]])) | is.nan(sum(lmom[[1]]))) {
      warning('invalid moments, no distribution fitted')
      return(c(params,fit.props)) # NA output
    }
    # remap parameters
    if(distr=='gamma'){
      fit <- lmomco::pargam(lmom)
      params['shape'] <- fit$para['alpha']
      params['rate'] <- 1/fit$para['beta']
    } else if (distr=='gamma3') {
      fit <- lmomco::parpe3(lmom)
      params['shape'] <- (2/fit$para['gamma'])^2
      params['scale'] <- fit$para['sigma']/sqrt(params['shape'])
      params['thres'] <- fit$para['mu'] - (params['shape']*params['scale'])
    } else if (distr=='weibull'){
      # this is the 3 parameter weibull distribution, any threshold should be accounted for by the prob.zero value
      fit <- lmomco::parwei(lmom)
      params['shape'] <- fit$para['delta']
      params['scale'] <- fit$para['beta']
    } else  if (distr=='weibull3'){
      fit <- lmomco::parwei(lmom)
      params['shape'] <- fit$para['delta']
      params['scale'] <- fit$para['beta']
      params['thres'] <- -1*fit$para['zeta']
    } else if (distr=='gev'){
      fit <- lmomco::pargev(lmom)
      params['shape'] <- -1*fit$para['kappa']
      params['scale'] <- fit$para['alpha']
      params['location'] <- fit$para['xi']
    } else if (distr=='glo'){
      fit <- lmomco::parglo(lmom)
      params['xi'] <- fit$para['xi']
      params['alpha'] <- fit$para['alpha']
      params['kappa'] <- fit$para['kappa']
    }
  }

  # calculate goodness-of-fit
  if(!any(is.na(params))){
    suppressWarnings(ks.pval <- try(do.call('ks.test',c(list(x=data,y=paste0('p',distr)),as.list(params)))$p.value))
    if(!inherits(ks.pval,'try-error')){
      fit.props['ks.pval'] <- ks.pval
    }
    suppressWarnings(ad.pval <- try(do.call('ad.test',c(list(x=data,null=paste0('p',distr,sep='')),as.list(params)))$p.value))
    if(!inherits(ad.pval,'try-error')){
      fit.props['ad.pval'] <- ad.pval
    }
  }

  # return value
  res <- c(params,fit.props)
  attributes(res) <- c(attributes(res),list(data=data,distr=distr,method=method,na.thres=na.thres))
  return(res)
}


#' Function to estimate the distribution function
#' @param xx the variable to analysis
#' @param mtype predefined distribution
#' @param th whether a threshold is needed
#' @export
#'
choose_dist <- function(xx, mtype, th){

  if (mtype%in%c("gev","pareto")){
    if (mtype=="gev"){
      ff <- gev.fit(xx)
      params <- ff$mle
    }else if(mtype=="pareto"){
      thx <- quantile(xx, probs=th)
      ff <- gpd.fit(xx, thx[[1]])
      params <- c(ff$threshold, ff$mle)
    }
    nam.mar <- mtype
    estimates <- params
    gf <- gofTest(xx, distribution = mtype, test = "ks")


  }else{

    if (mtype!="none"){
      distributions <- mtype
    }else{
      distributions <-  c("norm", "lnorm", "gamma", "weibull", "exp")
    }



    distr_aic <-  list()
    distr_fit <-  list()
    ks.lis    <- list()
    gf.list <- list()


    for (distribution in distributions) {

      distr_fit[[distribution]] <-  tryCatch({fitdist(data=xx, distribution)}, error=function(e) {cat(distribution)})

      if ( !is.null(distr_fit[[distribution]] )){
        distr_aic[[distribution]] <-  distr_fit[[distribution]]$aic
        pdis <- paste("p",distribution, sep="")
        gf.list[[distribution]] <- gofstat(distr_fit[[distribution]])
        if (distribution == "norm"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, mean= distr_fit[[distribution]]$estimate["mean"],
                                            sd = distr_fit[[distribution]]$estimate["sd"])
        }else if (distribution == "lnorm"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, mean= distr_fit[[distribution]]$estimate["meanlog"],
                                            sd = distr_fit[[distribution]]$estimate["sdlog"])
        }else if (distribution == "gamma"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, shape= distr_fit[[distribution]]$estimate["shape"],
                                            rate = distr_fit[[distribution]]$estimate["rate"])
        }else if (distribution == "weibull"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, shape= distr_fit[[distribution]]$estimate["shape"],
                                            scale = distr_fit[[distribution]]$estimate["scale"])
        }else if (distribution == "exp"){
          ks.lis[[distribution]] <- ks.test(xx, pdis, rate= distr_fit[[distribution]]$estimate["rate"])
        }
      }
    }

    # ind.min <- which.min(distr_aic)
    # nam.mar <- names(distr_aic[ind.min])
    # ks.pval <- ks.lis[[ind.min]]$p.value
    #
    df.bic <- setNames(melt(lapply(gf.list, function(x) x$bic)), c("value", "name"))
    df.aic <- setNames(melt(lapply(gf.list, function(x) x$aic)), c("value", "name"))
    df.kspval <- setNames(melt(lapply(gf.list, function(x) x$ks)), c("value", "name"))
    df.cvpval <- setNames(melt(lapply(gf.list, function(x) x$cvm)), c("value", "name"))
    # select based on the minimum aic and compatible with pval >0.05
    ind <- which.min(df.aic$value)
    if (df.kspval$value[ind]>0.05 | length(df.kspval$value)==1){
      # I will keep ind, otherwise I look for the next lower aic
      nam.mar <- names(distr_aic[ind])
      ks.pval <- df.kspval$value[ind]
      estimates <- distr_fit[[ind]]$estimate

    }else{
      # find another index
      new_ind <- order(df.aic$value)[-1] #excluding the first
      ind.ks <-  which(df.kspval$value[new_ind] >0.05)

      if ( length(ind.ks)>1 ){
        # take the largest pvalue
        ind.ks    <- order(df.kspval$value[new_ind], decreasing = T)[1]
        ks.pval   <- df.kspval$value[ind.ks]
        estimates <- distr_fit[[ind.ks]]$estimate
        nam.mar   <- names(distr_aic[ind.ks])

      }else{

        new.ind.ks    <- order(df.kspval$value[new_ind], decreasing = T)[1]
        ks.pval   <- df.kspval$value[new.ind.ks]
        estimates <- distr_fit[[new.ind.ks]]$estimate
        nam.mar   <- names(distr_aic[new.ind.ks])

      }
    }
    # cdfcomp(distr_fit)

  }
  mm <- eval(parse(text=(paste("p",nam.mar, sep=""))))
  if (length(estimates)>2){
    p_non <- mm(xx, estimates[1],estimates[2], estimates[3])
  }else{
    p_non <- mm(xx, estimates[1],estimates[2])
  }

  if (mtype%in%c("gev","pareto")){

    return(list(data.frame(nam.mar=mtype, pval=gf$p.value), "estimates"=params, "pnon"=p_non))
  }else{
    return(list(data.frame(nam.mar,ks.pval), "estimates"=estimates, "pnon"=p_non))

  }

}
