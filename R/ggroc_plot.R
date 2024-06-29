#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @inheritParams annotation_plot
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#' prs_models(PRSdata, exposure="prs",outcome="disease",covariates=c("age", "sex"),nquantiles=10) |>
#'   ggroc_plot()
#' }
ggroc_plot <- function(prsModel) {
  UseMethod("ggroc_plot")
}


#' @export
ggroc_plot.prsModel <- function(prsModel) {

   ModelOutput <- prsModel

   ggROC_C <- pROC::ggroc(ModelOutput$ROC_C) +  ggplot2::geom_text(data =ModelOutput$AUCLabel, ggplot2::aes(0.5, 1,label = paste(label_AUC)), hjust = 1)
   ggROC_B <- pROC::ggroc(ModelOutput$ROC_B) +  ggplot2::geom_text(data =ModelOutput$AUCLabel, ggplot2::aes(0.5, 1,label = paste(label_AUC)), hjust = 1)

   ggROC_D <- pROC::ggroc(list(ModelOutput$ROC_B,ModelOutput$ROC_C)) + ggplot2::theme(aspect.ratio = 1) +
              ggplot2::theme_bw() +
              ggplot2::labs(color='')  +
              ggplot2::scale_color_manual(labels = c(ModelOutput$ModelBLabel,ModelOutput$ModelCLabel) ,values = c("blue", "red")) +
              ggplot2::theme(text = ggplot2::element_text(size = 20)) + ggplot2::theme(aspect.ratio = 1)

   return(ggROC_D)
}
