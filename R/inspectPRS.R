#' A patchwork of plots and PRS statistics for an overall summary of a PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the exposure variable
#' @param outcome A character string containing the name of the outcome variable
#' @param covariates Character variable containing names of covariates
#' @param comparison Character variable containing name a secondary PRS
#' @param nquantiles Number of quantiles 
#'
#' @return
#' @export
#'
#' @examples
#' inspectPRS(PRSdata, exposure="prs",outcome="disease",covariates="age,sex",nquantiles=10)

inspectPRS <- function(PRSdata, exposure, outcome, covariates, comparison=NA,nquantiles) {

   plot1 <- prs_density_plot(PRSdata, exposure="prs",outcome="disease")
   plot2 <- ggroc_plot(PRSdata, exposure="prs",outcome="disease",covariates="age,sex",nquantiles=10)
   plot3 <- prs_quantile_plot(PRSdata,exposure="prs",outcome="disease",covariates="age,sex",nquantiles=10)
   plot4 <- annotation_plot(PRSdata, exposure="prs",outcome="disease", covariates="age,sex", nquantiles=10)

   CombinedPlot <- patchwork::wrap_plots(plot1, plot2, plot3, plot4, ncol = 2) +
                   patchwork::plot_annotation(title = paste("PRS: ",exposure), theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 26, hjust = 0.5)))
   return(CombinedPlot)
}
