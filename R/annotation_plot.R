#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the polygenic risk score
#' @param outcome A character string containing the name of the outcome variable
#' @param covariates A character vector containing names of covariates
#' @param comparison A character string which if not NA contains the name of a comparison PRS
#' @param nquantiles Number of quantiles
#' @param model An existing model object from prs_models
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' annotation_plot(PRSdata, exposure="prs",outcome="disease", covariates=c("age" ,"sex"), nquantiles=10)
#' }
annotation_plot <- function(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles, model=NA) {

   if(any(is.na(model))) {
      ModelOutput <- prs_models(PRSdata, exposure, outcome, covariates, comparison=comparison, nquantiles)
   } else {
      ModelOutput=model
   }

   BaseModel_AUROC <- round(ModelOutput$DelongROC1,3)
   ComparisonModel_AUROC <- round(ModelOutput$DelongROC2,3)

   Annotation <- paste0("PRS ",exposure," and outcome ",outcome,
                        "<br><br>[A] Comparison of PRS in cases and controls",
                        "<br>Unadjusted P = ",ModelOutput$TidyDText,
                        "<br>Adjusted P = ",ModelOutput$TidyCText,
                        "<br><br>[B] Test accuracy using area under receiver operating curves",
                        "<br>Base model: ",BaseModel_AUROC,
                        "<br>Comparison model: ",ComparisonModel_AUROC,
                        "<br>DeLong test for difference P = ",ModelOutput$DelongPValue,
                        "<br><br>[C] Trend over ",nquantiles," quantiles of PRS<br>by odds ratio P = ",ModelOutput$TidyQText)
   Geom_RT <- ggtext::GeomRichtext
   ggAnnotate <- ggplot2::ggplot() + ggplot2::annotate(geom=Geom_RT, x = 10, y = 10,size = 6,label = Annotation) + ggplot2::theme_bw() + ggplot2::labs(color='') + ggplot2::theme_void()
   return(ggAnnotate)
}
