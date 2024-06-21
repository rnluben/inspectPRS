#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the polygenic risk score 
#' @param outcome A character string containing the name of the outcome variable
#' @param covariates A character string containing a comma separated variable list of covariates
#' @param comparison A character string which if not NA contains the name of a comparison PRS 
#' @param nquantiles Number of quantiles 
#'
#' @return
#' @export
#'
#' @examples
#' annotation_plot(PRSdata, exposure="prs",outcome="disease", covariates="age,sex", nquantiles=10)
annotation_plot <- function(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles ) {

   ModelOutput <- prs_models_NEW(PRSdata, exposure, outcome, covariates, comparison=comparison, nquantiles)

   Annotation <- paste0("PRS ",exposure," and outcome ",outcome,
                        "<br><br>[A] Comparison of PRS in cases and controls P = ",ModelOutput$TidyCText,
                        "<br><br>[B] Test accuracy using area under receiver operating curves",
                        "<br><br>",ModelOutput$DelongROC1,
                        "<br><br>",ModelOutput$DelongROC2,
                        "<br>DeLong test for difference P = ",ModelOutput$DelongPValue,
                        "<br><br>[C] Trend over ",nquantiles," quantiles of PRS<br>by odds ratio P = ",ModelOutput$TidyQText)
   ggAnnotate <- ggplot2::ggplot() + ggplot2::annotate("text", x = 10, y = 10,size = 6,label = Annotation) + ggplot2::theme_bw() + ggplot2::labs(color='') + ggplot2::theme_void()
   return(ggAnnotate)
}
