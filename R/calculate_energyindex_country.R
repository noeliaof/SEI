#' Function to obtain the SDEI for selected countries
#' @param data dataframe that contains data for one or more countries
#' @param method method to calculate the index
#' @param scale indicates time scale of aggregation for the index
#' @details whether the index will be calculated on aggregated amounts or not (e.g., daily, 6h)
#' @param nvars variable name, e.g., wind production
#' @param index_type  indicates the type of index: "probability", "bounded" or "normal" 
#' @export

calculate_energyindex_country <- function(data, method="fitdis", scale, nvars, index_type="normal"){

  n_country <- unique(data$country)
  ener_ind <- l_sdei <- l_info <-  list()
  for ( icountry in 1:length(n_country)){
    for( ivar in 1:length(nvars)){
      X <- data%>%dplyr::filter(country==n_country[icountry])%>%dplyr::select(date,nvars[ivar])
      country_index <- tryCatch({ funSDEI(X, method, scale = scale, index_type = index_type)},
                                error=function(e) {cat("\n","error to get the index in", n_country[icountry], "for",nvars[ivar])})
      if (is.null(country_index)){
        ener_ind[[ivar]]<- NA
      }else{
        ener_ind[[ivar]] <- country_index
      }

    }
    names(ener_ind) <- nvars

    n_sdei   <- lapply(ener_ind, function(x) x[[1]])
    if (method == "fitdis"){
      info_dis <- lapply(ener_ind, function(x) x[[2]])
      df_info <- setNames(melt(info_dis, id=c("nam.mar","ks.pval")), c("nam.mar","ks.pval", "type"))
    }else{
      df_info <- NULL
    }

    df_sdei <- setNames(melt(n_sdei, id=c("date","SDEI")), c("date","SDEI","type"))
    l_sdei[[icountry]] <- df_sdei
    l_info[[icountry]] <- df_info
  }


  if(method == "fitdis"){
    names(l_sdei) <- names(l_info) <- n_country
    return(list("SDEI"= l_sdei, "info_dis"=l_info))
  }else{
    names(l_sdei) <- n_country
    return(list("SDEI"= l_sdei))
  }

}

