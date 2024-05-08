#' A function to draw a density plot comparing cases and controls by PRS
#'
#' @param PRSdata A dataframe which includes an exposure and outcome
#' @param exposure A character string containing the name of the exposure variable
#' @param outcome A character string containing the name of the outcome variable
#' @param covariates Character variable containing names of covariates
#' @param comparison Character variable containing name a secondary PRS
#'
#' @return
#' @export
#'
#' @examples
#' ggroc_plot(PRSdata, exposure="prs",outcome="disease",covariates="age,sex")
ggroc_plot <- function(PRSdata, exposure, outcome, covariates, comparison=NA) {

   covariates_list <- stringr::str_split_1(covariates, ",")

   # Construct models B=Base model, U=Unadjusted model Q=Quantile model C=Continuous model D=Duo (two inputs)
   if(is.na(comparison)) {
      ModelB <- PRSdata %>% dplyr::select(OUTCOME={{outcome}}, {{covariates_list}}) %>%
                stats::glm(OUTCOME ~ ., data=., family = binomial)
      ModelBLabel <- stringr::str_replace_all(covariates,","," and ") 
      comparison_name <- ""
   } else {
      ModelB <- PRSdata %>% dplyr::select(`Risk score`= {{comparison}}, OUTCOME={{outcome}},{{covariates_list}}) %>%
                stats::glm(OUTCOME ~ ., data=., family = binomial)
      ModelBLabel <- paste0(comparison,", ",stringr::str_replace_all(covariates,","," and "))
      comparison_name <- paste0("_",comparison)
   }

   ModelC <- PRSdata %>% dplyr::select(`Risk score`= {{exposure}}, OUTCOME={{outcome}},{{covariates_list}}) %>%
                         stats::glm(OUTCOME ~ ., data=., family = binomial)
   ModelCLabel <- paste0(exposure,", ",stringr::str_replace_all(covariates,","," and "))


   PRSdata <- PRSdata %>% modelr::add_predictions(ModelB) %>% dplyr::rename(PredictB=pred)
   PRSdata <- PRSdata %>% modelr::add_predictions(ModelC) %>% dplyr::rename(PredictC=pred)
#  PRSdata$PredictB <- stats::predict(ModelB, type="response")  # Check that specifying newdata=PRS is necessary !!
#  PRSdata$PredictC <- stats::predict(ModelC, type="response")

   ROC_C <- PRSdata %>% dplyr::select(OUTCOME={{outcome}}, PredictC) %>% pROC::roc(OUTCOME ~ PredictC, data = .)
   CI_C <- ci.auc(ROC_C, method="bootstrap")
   ROC_B <- PRSdata %>% dplyr::select(OUTCOME={{outcome}}, PredictB) %>% pROC::roc(OUTCOME ~ PredictB, data = .)
   CI_B <- ci.auc(ROC_B, method="bootstrap")

   # Test difference in ROC curves.  Methods bootstrap, venkatraman and specificity all give similar results
   `Age+Sex` <- ROC_B
   `Age+Sex+PRS` <- ROC_C
   Delong <- pROC::roc.test(`Age+Sex`, `Age+Sex+PRS`, paired=TRUE, method="delong")
   print(Delong)
   DelongText <- paste(capture.output(Delong), collapse="<br>")
   DelongPValue <- capture.output(Delong) %>% as_tibble %>% dplyr::filter(grepl("p-value",value)) %>% tidyr::separate(value, c(NA, "pvalue"), ", ") %>% tidyr::separate(pvalue, c(NA,NA,"P")," ")  %>% pull 
   DelongPValue <- exp_sup(as.numeric(DelongPValue))
   DelongROC  <- capture.output(Delong) %>% as_tibble %>% dplyr::filter(row_number()==11) %>% tidyr::separate(value, c(NA,"ROC1","ROC2"), "  ") %>%
                 dplyr::mutate(AUROC=paste0(ModelCLabel,", AUROC = ",round(as.numeric(ROC2),3),"<br>vs<br>",ModelBLabel,", AUROC =  ",round(as.numeric(ROC1),3))) %>% dplyr::select(AUROC) %>% pull
   TitleText <- paste0("Polygenic risk score from \"",exposure,"\" and ",outcome," for ",NCase," cases and ",NControl," controls in EPIC-Norfolk")
   TidyCText <- TidyC %>% dplyr::filter(term=="`Risk score`") %>% dplyr::select(p.value) %>% pull
   TidyCText <- exp_sup(TidyCText)
   TidyQText <- TidyQT %>% dplyr::filter(term=="`p-trend`") %>% dplyr::select(p.value) %>% pull
   TidyQText <- exp_sup(TidyQText)

   AUCLabel <- ROC_C$auc %>% as_tibble %>%
               dplyr::mutate(label_AUC = paste0("AUC = ",round(value,4))) %>%
               dplyr::bind_rows(.id = "name") %>%
               dplyr::mutate(name = paste0("PRS: ", exposure," Outcome: ",outcome," adjusted for age and sex"))
   ggROC_C <- pROC::ggroc(ROC_C) +  geom_text(data =AUCLabel, aes(0.5, 1,label = paste(label_AUC)), hjust = 1) 
   ggROC_B <- pROC::ggroc(ROC_B) +  geom_text(data =AUCLabel, aes(0.5, 1,label = paste(label_AUC)), hjust = 1) 

   ggROC_D <- pROC::ggroc(list(ROC_B,ROC_C)) + ggplot2::theme(aspect.ratio = 1) + ggplot2::theme_bw() + ggplot2::labs(color='')  + ggplot2::scale_color_manual(labels = c(ModelBLabel,ModelCLabel) ,values = c("blue", "red")) +  ggplot2::theme(text = ggplot2::element_text(size = 20))
   return(ggROC_D)
}
