#' Polygenic risk score modelling functions
#'
#' @param PRSdata Data frame containing exposure and outcome
#' @param exposure Character variable containing name of polygenic risk score variable
#' @param outcome Character variable containing name of binary outcome variable
#' @param covariates Character variable containing names of covariates
#' @param comparison Character variable containing name a secondary PRS
#' @param nquantiles Number of quantiles
#'
#' @return
#' @export
#'
#' @examples
prs_models <- function(PRSdata, exposure, outcome, covariates, comparison=NA, nquantiles) {
   qExposure <- paste0("q",exposure)
   QExposure <- paste0("Q",exposure)
   comparison_var <- ifelse(is.na(comparison),exposure,comparison)
   covariates_list <- stringr::str_split_1(covariates, ",")

   PRSdata <- PRSdata %>%
              dplyr::select({{exposure}},{{outcome}},{{comparison_var}},{{covariates_list}}) %>%
              dplyr::filter(!is.na({{exposure}}) & !is.na({{outcome}})) %>%
              dplyr::mutate("q{exposure}" := statar::xtile(!!as.name(exposure),nquantiles)) %>%
              dplyr::mutate("Q{exposure}" := !!as.name(qExposure))
   N <- PRSdata %>% dplyr::count()
   NCase <- PRSdata %>% dplyr::filter(!!as.name(outcome)=="POAG") %>% dplyr::count()
   NControl <- PRSdata %>% dplyr::filter(!!as.name(outcome)=="Control") %>% dplyr::count()

   # Construct models B=Base model, U=Unadjusted model Q=Quantile model C=Continuous model D=Duo (two inputs)
   if(is.na(comparison)) {
      ModelB <- PRSdata %>% dplyr::select(OUTCOME={{outcome}}, QQ=all_of(QExposure),{{covariates_list}}) %>%
                stats::glm(OUTCOME ~ sex + age3, data=., family = binomial)
      ModelBLabel <- "Age and sex"
      comparison_name <- ""
   } else {
      ModelB <- PRSdata %>% dplyr::select(`Risk score`= {{comparison}}, OUTCOME={{outcome}},{{covariates_list}}) %>%
                stats::glm(OUTCOME ~ `Risk score` + sex + age3, data=., family = binomial)
      ModelBLabel <- paste0(comparison,", age and sex")
      comparison_name <- paste0("_",comparison)
   }
   ModelU <- PRSdata %>% dplyr::select(`Risk score`=dplyr::all_of(qExposure), OUTCOME={{outcome}}, QQ=all_of(QExposure)) %>%
                         dplyr::mutate(dplyr::across(`Risk score`, as.factor)) %>%
                         dplyr::mutate(`Risk score` = forcats::fct_reorder(`Risk score`, QQ)) %>%
                         stats::glm(OUTCOME ~ `Risk score`, data=., family = binomial)
   ModelQ <- PRSdata %>% dplyr::select(`Risk score`=dplyr::all_of(qExposure), OUTCOME={{outcome}}, QQ=all_of(QExposure),{{covariates_list}}) %>%
                         dplyr::mutate(dplyr::across(`Risk score`, as.factor)) %>%
                         dplyr::mutate(`Risk score` = forcats::fct_reorder(`Risk score`, QQ)) %>%
                         stats::glm(OUTCOME ~ `Risk score` + sex + age3, data=., family = binomial)
   ModelC <- PRSdata %>% dplyr::select(`Risk score`= {{exposure}}, OUTCOME={{outcome}},{{covariates_list}}) %>%
                         stats::glm(OUTCOME ~ `Risk score` + sex + age3, data=., family = binomial)
   ModelCLabel <- paste0(exposure,", age and sex")
   ModelQT <- PRSdata %>% dplyr::select(`p-trend`= dplyr::all_of(QExposure), OUTCOME={{outcome}},{{covariates_list}}) %>%
                          stats::glm(OUTCOME ~ `p-trend` + sex + age3, data=., family = binomial)

   N_PRS <- PRSdata %>% dplyr::filter(!is.na({{outcome}})) %>% dplyr::count(!!as.name(qExposure)) %>% dplyr::mutate(qPRS = paste0("Q",!!as.name(qExposure)))
   N_PRS <- PRSdata %>% dplyr::filter(!is.na({{outcome}})) %>% dplyr::count() %>% dplyr::mutate(qPRS="All") %>% dplyr::bind_rows(N_PRS,.)
   TidyC <- broom::tidy(ModelC)
   TidyU <- broom::tidy(ModelU)
   TidyQ <- broom::tidy(ModelQ, conf.int = TRUE, exp = TRUE)
   TidyQT <- broom::tidy(ModelQT, conf.int = TRUE, exp = TRUE) %>% filter(term=="`p-trend`")
   TidyOut <- dplyr::bind_rows(TidyQ,TidyQT) %>%
             dplyr::filter(grepl("Risk score",term)) %>%
             dplyr::add_row(term = "`Risk score`1") %>%
             dplyr::mutate(sortcol = as.numeric(substr(term,13,14))) %>%
             dplyr::arrange(sortcol)
   TidyOut <- dplyr::bind_rows(TidyOut %>% arrange(sortcol),TidyQT)
   TidyOut <- dplyr::bind_cols(N_PRS,TidyOut) %>% dplyr::select(-sortcol) %>% dplyr::mutate(dplyr::across(qPRS, as.factor)) %>% dplyr::mutate(qPRS = forcats::fct_reorder(qPRS, !!as.name(qExposure)))
   return(TidyOut)
}
