#' Function to estimate the distribution
#' @param xx the variable to analysis
#' @param mtype predefined distribution
#' @param th whether a threshold is needed
#' @export
#'

fitDis <- function(xx, mtype, th){

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
