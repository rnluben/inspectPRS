#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the exposure variable
#' @param outcome A character string containing the name of the outcome variable
#'
#' @return
#' @export
#'
#' @examples
#' prs_density_plot(PRSdata, exposure="prs",outcome="poag")
prs_density_plot <- function(PRSdata, exposure, outcome) {
   DensityPlot <- PRSdata %>%
                  dplyr::select(OUTCOME={{outcome}}, EXPOSURE={{exposure}}) %>%
                  ggplot2::ggplot(ggplot2::aes(x = EXPOSURE, colour = OUTCOME)) +
                         ggplot2::geom_density() +
                         ggplot2::theme_bw() +
                         ggplot2::labs(color='') +
                         ggplot2::theme(text = ggplot2::element_text(size = 20)) +
                         ggplot2::xlab(paste0("PRS \"",{{exposure}},"\""))
   return(DensityPlot)
}
