#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the exposure variable
#' @param outcome A character string containing the name of the outcome variable
#' @param TidyCText blah 
#' @param DelongROC blah 
#' @param DelongPValue blah 
#' @param nq  Number of quantiles 
#' @param TidyQText  blah 
#'
#' @return
#' @export
#'
#' @examples
#' annotation_plot(PRSdata, exposure="prs",outcome="disease", covariates="age,sex", nquantiles=10)
annotation_plot <- function(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles ) {

   ModelOutput <- prs_models_NEW(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles)

   Annotation <- paste0("PRS ",exposure," and outcome ",outcome,
                        "<br><br>[A] Comparison of PRS in cases and controls P = ",TidyCText,
                        "<br><br>[B] Test accuracy using area under receiver operating curves",
                        "<br><br>",DelongROC,
                        "<br>DeLong test for difference P = ",DelongPValue,
                        "<br><br>[C] Trend over ",nquantiles," quantiles of PRS<br>by odds ratio P = ",TidyQText)                                                                                             
   ggAnnotate <- ggplot2::ggplot() + ggplot2::annotate("richtext", x = 10,  y = 10,size = 6,label = Annotation) + ggplot2::theme_bw() + ggplot2::labs(color='') + ggplot2::theme_void()        
   return(ggAnnotate)
}
