#' A function to draw a density plot comparing cases and controls by PRS
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
#' ggroc_plot(PRSdata, exposure="prs",outcome="disease",covariates="age,sex",nquantiles=10)
ggroc_plot <- function(PRSdata, exposure, outcome, covariates, comparison=NA,nquantiles) {

   ModelOutput <- prs_models(PRSdata, exposure, outcome, covariates, comparison=comparison, nquantiles)

   ggROC_C <- pROC::ggroc(ModelOutput$ROC_C) +  ggplot2::geom_text(data =ModelOutput$AUCLabel, ggplot2::aes(0.5, 1,label = paste(label_AUC)), hjust = 1) 
   ggROC_B <- pROC::ggroc(ModelOutput$ROC_B) +  ggplot2::geom_text(data =ModelOutput$AUCLabel, ggplot2::aes(0.5, 1,label = paste(label_AUC)), hjust = 1) 

   ggROC_D <- pROC::ggroc(list(ModelOutput$ROC_B,ModelOutput$ROC_C)) + ggplot2::theme(aspect.ratio = 1) +
              ggplot2::theme_bw() +
              ggplot2::labs(color='')  +
              ggplot2::scale_color_manual(labels = c(ModelOutput$ModelBLabel,ModelOutput$ModelCLabel) ,values = c("blue", "red")) +
              ggplot2::theme(text = ggplot2::element_text(size = 20)) + ggplot2::theme(aspect.ratio = 1)

   return(ggROC_D)
}
