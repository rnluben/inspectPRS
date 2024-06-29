#' A patchwork of plots and PRS summary statistics for an overview of Polygenic Risk Scores
#'
#' @inheritParams prs_models
#'
#' @return A patchwork ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' inspectPRS(PRSdata, exposure="prs",outcome="disease",covariates=c("age", "sex"),nquantiles=10)
#' }
inspectPRS <- function(PRSdata, exposure, outcome, covariates, comparison=NA,nquantiles) {

  prsModel <- prs_models(PRSdata, exposure=exposure,outcome=outcome,covariates=covariates,nquantiles=nquantiles)

   plot1 <- prs_density_plot(PRSdata, exposure=exposure,outcome=outcome)
   plot2 <- ggroc_plot(prsModel)
   plot3 <- prs_quantile_plot(prsModel)
   plot4 <- annotation_plot(prsModel)

   CombinedPlot <- patchwork::wrap_plots(plot1, plot2, plot3, plot4, ncol = 2) +
                   patchwork::plot_annotation(title = paste("PRS: ",exposure), theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 26, hjust = 0.5)))
   return(CombinedPlot)
}

