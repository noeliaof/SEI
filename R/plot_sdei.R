#' Function to plot the index
#' @param dx dataframe with the SDEI information, dates, index and type of standardized index
#' @param yy yeas of study
#' @param ivar variable to plot, e.g., index name
#' @export
#' 
plot_sdei <- function(dx, yy = seq(1979, 2019), ivar = "SDEI"){
  # Visualise the index
  dd <- "date"
  
  if(length(unique(dx$type)) > 1){
    p <- dx%>%dplyr::filter(format(date,"%Y")%in%yy)%>%ggplot(aes_string(x=dd,y=ivar)) +
      geom_ribbon(aes_string(ymin=pmin(ivar,0), ymax=0), fill="blue", alpha=0.5) +
      geom_ribbon(aes_string(ymin=0, ymax=pmax(ivar,0)), fill="red", alpha=0.5) +
      scale_x_datetime(name="Date", expand=c(0,0)) + scale_y_continuous(name=ivar, expand=expansion(c(0, 0.1))) +
      #geom_hline(aes(yintercept=0), color="grey") +
      #geom_hline(aes(yintercept=1), color="grey",linetype="dashed")+
      #geom_hline(aes(yintercept=-1), color="grey",linetype="dashed")+
      facet_wrap(~type) + 
      theme_bw()
  }else{
    p <- dx%>%dplyr::filter(format(date,"%Y")%in%yy)%>%ggplot( aes_string(x=dd,y=ivar)) +
      geom_ribbon(aes_string(ymin=pmin(ivar,0), ymax=0), fill="blue", alpha=0.5) +
      geom_ribbon(aes_string(ymin=0, ymax=pmax(ivar,0)), fill="red", alpha=0.5) +
      scale_x_datetime(name="Date", expand=c(0,0)) + scale_y_continuous(name=ivar, expand=expansion(c(0, 0.1))) +
      #geom_hline(aes(yintercept=0), color="grey") +
      #geom_hline(aes(yintercept=1), color="grey",linetype="dashed")+
      #geom_hline(aes(yintercept=-1), color="grey",linetype="dashed")+
      theme_bw()
  }
  
  return(p)
  
}

