#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @param prsModel A `prsModel` object, created by [prs_models()].
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' prs_models(PRSdata, exposure="prs",outcome="disease", covariates=c("age" ,"sex"), nquantiles=10) |>
#'   annotation_plot()
#' }
annotation_plot <- function(prsModel) {
  UseMethod("annotation_plot")
}


#' @export
annotation_plot.prsModel <- function(prsModel) {

   ModelOutput <- prsModel

   BaseModel_AUROC <- round(ModelOutput$DelongROC1,3)
   ComparisonModel_AUROC <- round(ModelOutput$DelongROC2,3)

   Annotation <- paste0("PRS ",ModelOutput$params$exposure," and outcome ",ModelOutput$params$outcome,
                        "<br><br>[A] Comparison of PRS in cases and controls P = ",ModelOutput$TidyCText,
                        "<br><br>[B] Test accuracy using area under receiver operating curves",
                        "<br>Base model: ",BaseModel_AUROC,
                        "<br>Comparison model: ",ComparisonModel_AUROC,
                        "<br>DeLong test for difference P = ",ModelOutput$DelongPValue,
                        "<br><br>[C] Trend over ",ModelOutput$params$nquantiles," quantiles of PRS<br>by odds ratio P = ",ModelOutput$TidyQText)
   Geom_RT <- ggtext::GeomRichtext
   ggAnnotate <- ggplot2::ggplot() + ggplot2::annotate(geom=Geom_RT, x = 10, y = 10,size = 6,label = Annotation) + ggplot2::theme_bw() + ggplot2::labs(color='') + ggplot2::theme_void()
   return(ggAnnotate)
}


