#' A patchwork of plots and PRS summary statistics for an overview of Polygenic Risk Scores
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the exposure variable
#' @param outcome A character string containing the name of the outcome variable
#' @param covariates Character variable containing names of covariates
#' @param comparison Character variable containing name a secondary PRS
#' @param nquantiles Number of quantiles
#' @param reporttitle A character string containing the report title
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' inspectPRS(PRSdata, exposure="prs",outcome="disease",covariates=c("age", "sex"),nquantiles=10, reporttitle="PRS using toy data")
#' }
inspectPRS <- function(PRSdata, exposure, outcome, covariates, comparison=NA,nquantiles,reporttitle=NA) {

   reporttitle <- ifelse(is.na(reporttitle),paste("PRS: ",exposure),reporttitle)
   ModelOutput <- prs_models(PRSdata, exposure, outcome, covariates, comparison=comparison, nquantiles)

   plot1 <- prs_density_plot(PRSdata, exposure=exposure,outcome=outcome)
   plot2 <- ggroc_plot(PRSdata, exposure=exposure,outcome=outcome,covariates=covariates,nquantiles=nquantiles, model=ModelOutput)
   plot3 <- prs_quantile_plot(PRSdata,exposure=exposure,outcome=outcome,covariates=covariates,nquantiles=nquantiles, model=ModelOutput)
   plot4 <- annotation_plot(PRSdata, exposure=exposure,outcome=outcome, covariates=covariates, nquantiles=nquantiles, model=ModelOutput)

   CombinedPlot <- patchwork::wrap_plots(plot1, plot2, plot3, plot4, ncol = 2) +
                   patchwork::plot_annotation(title = reporttitle, theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 26, hjust = 0.5)))
   return(CombinedPlot)
}

