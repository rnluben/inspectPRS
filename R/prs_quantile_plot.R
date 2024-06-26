
#' A function to draw a quantile plot comparing cases and controls by PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the polygenic risk score
#' @param outcome A character string containing the name of the outcome variable
#' @param covariates A character string containing a comma separated variable list of covariates
#' @param nquantiles Number of quantiles
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' prs_quantile_plot(PRSdata,exposure="prs",outcome="disease",covariates="age,sex",nquantiles=10)
#' }
prs_quantile_plot <- function(PRSdata,exposure,outcome,covariates,nquantiles) {

   ModelOutput <- prs_models(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles)

   QuantilePlot <- ModelOutput$TidyOut %>%
               dplyr::filter(qPRS!="All") %>%
               dplyr::mutate(`Odds ratio (95% confidence interval)` = ifelse(is.na(estimate),1,estimate)) %>%
               dplyr::mutate(`Polygenic risk score quantiles`=qPRS) %>%
               ggplot2::ggplot(ggplot2::aes(x=`Polygenic risk score quantiles`, y=`Odds ratio (95% confidence interval)`)) +
                      ggplot2::geom_point(size = 3) +
                      ggplot2::theme_bw(base_size = 10) +
                      ggplot2::geom_errorbar(ggplot2::aes(ymin=conf.low, ymax=conf.high), width=.1) +
                      ggplot2::scale_y_log10() +
                      ggplot2::theme(axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20, r = 20, b = 0, l = 0))) +
                      ggplot2::theme(text = ggplot2::element_text(size = 20)) +
                      ggplot2::xlab(paste0("Quantiles of PRS \"",{{exposure}},"\""))
   return(QuantilePlot)
}
