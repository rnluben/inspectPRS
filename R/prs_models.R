#' Polygenic risk score modelling functions
#'
#' @param PRSdata Data frame containing exposure and outcome
#' @param exposure Character variable containing name of polygenic risk score variable
#' @param outcome Character variable containing name of binary outcome variable
#' @param covariates Character vector containing names of covariates
#' @param comparison Character variable containing name a secondary PRS
#' @param nquantiles Number of quantiles
#'
#' @return A list.
#' @export
#'
#' @examples
#' \dontrun{
#' prs_models(PRSdata,exposure="prs",outcome="disease",covariates= c("age", "sex"),nquantiles=10)
#' }
prs_models <- function(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles) {
   qExposure <- paste0("q",exposure)
   QExposure <- paste0("Q",exposure)
   comparison_var <- ifelse(is.na(comparison),exposure,comparison)

   PRSdata <- PRSdata %>%
              dplyr::select({{exposure}},{{outcome}},{{comparison_var}},{{covariates}}) %>%
              dplyr::filter(!is.na({{exposure}}) & !is.na({{outcome}})) %>%
              dplyr::mutate(!!qExposure := statar::xtile(!!as.name(exposure),nquantiles)) %>%
              dplyr::mutate(!!QExposure := !!as.name(qExposure))
   N <- PRSdata %>% dplyr::count()
   NCase <- PRSdata %>% dplyr::filter(!!as.name(outcome)=="1") %>% dplyr::count()
   NControl <- PRSdata %>% dplyr::filter(!!as.name(outcome)=="0") %>% dplyr::count()

   # Construct models B=Base model, U=Unadjusted model Q=Quantile model C=Continuous model D=Duo (two inputs)
   if(is.na(comparison)) {
      PRSdataB <- PRSdata %>% dplyr::select(OUTCOME={{outcome}}, {{covariates}})
      ModelB <- stats::glm(OUTCOME ~ ., data=PRSdataB, family = stats::binomial)
      ModelBLabel <- paste0(covariates,collapse=" and ")
      comparison_name <- ""
   } else {
      PRSdataB <- PRSdata %>% dplyr::select(`Risk score`= {{comparison_var}}, OUTCOME={{outcome}},{{covariates}})
      ModelB <- stats::glm(OUTCOME ~ ., data=PRSdataB, family = stats::binomial)
      ModelBLabel <- paste0(comparison,", ",paste0(covariates,collapse=" and "))
      comparison_name <- paste0("_",comparison)
   }
   ModelU <- PRSdata %>% dplyr::select(`Risk score`=dplyr::all_of(qExposure), OUTCOME={{outcome}}, QQ=dplyr::all_of(QExposure)) %>%
                         dplyr::mutate(dplyr::across(`Risk score`, as.factor)) %>%
                         dplyr::mutate(`Risk score` = forcats::fct_reorder(`Risk score`, QQ)) %>%
                         stats::glm(OUTCOME ~ `Risk score`, data=., family = stats::binomial)
   ModelQ <- PRSdata %>% dplyr::select(`Risk score`=dplyr::all_of(qExposure), OUTCOME={{outcome}}, QQ=dplyr::all_of(QExposure),{{covariates}}) %>%
                         dplyr::mutate(dplyr::across(`Risk score`, as.factor)) %>%
                         dplyr::mutate(`Risk score` = forcats::fct_reorder(`Risk score`, QQ)) %>% dplyr::select(-QQ) %>%
                         stats::glm(OUTCOME ~ ., data=., family = stats::binomial)
   PRSdataC <- PRSdata %>% dplyr::select(`Risk score`= {{exposure}}, OUTCOME={{outcome}},{{covariates}})
   ModelC <-             stats::glm(OUTCOME ~ ., data=PRSdataC, family = stats::binomial)
   ModelCLabel <- paste0(exposure,covariates,collapse=" and ")
   ModelQT <- PRSdata %>% dplyr::select(`p-trend`= dplyr::all_of(QExposure), OUTCOME={{outcome}},{{covariates}}) %>%
                          stats::glm(OUTCOME ~ ., data=., family = stats::binomial)

   N_PRS <- PRSdata %>% dplyr::filter(!is.na({{outcome}})) %>% dplyr::count(!!as.name(qExposure)) %>% dplyr::mutate(qPRS = paste0("Q",!!as.name(qExposure)))
   N_PRS <- PRSdata %>% dplyr::filter(!is.na({{outcome}})) %>% dplyr::count() %>% dplyr::mutate(qPRS="All") %>% dplyr::bind_rows(N_PRS,.)
   TidyC <- broom::tidy(ModelC)
   TidyU <- broom::tidy(ModelU)
   TidyQ <- broom::tidy(ModelQ, conf.int = TRUE, exp = TRUE)
   TidyQT <- broom::tidy(ModelQT, conf.int = TRUE, exp = TRUE) %>% dplyr::filter(term=="`p-trend`")
   TidyOut1 <- dplyr::bind_rows(TidyQ,TidyQT) %>%
             dplyr::filter(grepl("Risk score",term)) %>%
             dplyr::add_row(term = "`Risk score`1") %>%
             dplyr::mutate(sortcol = as.numeric(substr(term,13,14))) %>%
             dplyr::arrange(sortcol)
   TidyOut1 <- dplyr::bind_rows(TidyOut1 %>% dplyr::arrange(sortcol),TidyQT)
   TidyOut <- dplyr::bind_cols(N_PRS,TidyOut1) %>% dplyr::select(-sortcol) %>%
              dplyr::mutate(dplyr::across(qPRS, as.factor)) %>%
              dplyr::mutate(qPRS = forcats::fct_reorder(qPRS, !!as.name(qExposure),.na_rm = TRUE))

   PRSdataB <- PRSdataB %>% modelr::add_predictions(ModelB) %>% dplyr::rename(PredictB=pred)
   PRSdataC <- PRSdataC %>% modelr::add_predictions(ModelC) %>% dplyr::rename(PredictC=pred)
#  PRSdata$PredictB <- stats::predict(ModelB, type="response")  # Check that specifying newdata=PRS is necessary !!
#  PRSdata$PredictC <- stats::predict(ModelC, type="response")

   ROC_C <- PRSdataC %>% pROC::roc(OUTCOME ~ PredictC, data = .)
   CI_C <- pROC::ci.auc(ROC_C, method="bootstrap")
   ROC_B <- PRSdataB %>% pROC::roc(OUTCOME ~ PredictB, data = .)
   CI_B <- pROC::ci.auc(ROC_B, method="bootstrap")

   # Test difference in ROC curves.  Methods bootstrap, venkatraman and specificity all give similar results
   BASE <- ROC_B
   BASEPRS <- ROC_C
   Delong <- pROC::roc.test(BASE, BASEPRS, paired=TRUE, method="delong")
   print(Delong)
   DelongText <- paste(utils::capture.output(Delong), collapse="<br>")
   DelongPValue <- exp_sup(Delong$p.value)
   DelongROC1  <- Delong$roc1$auc
   DelongROC2  <- Delong$roc2$auc
   TitleText <- paste0("Polygenic risk score from \"",exposure,"\" and ",outcome," for ",NCase," cases and ",NControl," controls")
   TidyCText <- TidyC %>% dplyr::filter(term=="`Risk score`") %>% dplyr::select(p.value) %>% dplyr::pull()
   TidyCText <- exp_sup(TidyCText)
   TidyQText <- TidyQT %>% dplyr::filter(term=="`p-trend`") %>% dplyr::select(p.value) %>% dplyr::pull()
   TidyQText <- exp_sup(TidyQText)

   AUCLabel <- ROC_C$auc %>% tibble::as_tibble() %>%
               dplyr::mutate(label_AUC = paste0("AUC = ",round(value,4))) %>%
               dplyr::bind_rows(.id = "name") %>%
               dplyr::mutate(name = paste0("PRS: ", exposure," Outcome: ",outcome," adjusted for age and sex"))


   ReturnList <- list(TidyOut=TidyOut,TidyCText=TidyCText, TidyQText=TidyQText, AUCLabel=AUCLabel, DelongPValue=DelongPValue, DelongROC1=DelongROC1, DelongROC2=DelongROC2, NCase=NCase,NControl=NControl, ROC_C=ROC_C,ROC_B=ROC_B,ModelBLabel=ModelBLabel,ModelCLabel=ModelCLabel)
   return(ReturnList)
}

