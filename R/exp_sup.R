#' Format a P value in markdown or HTML
#'
#' @param x A numeric vector with one element
#' @param pvalue Number of decimal places needed
#' @param boldsig Whether to make bold significant P values
#' @param md Whether to use markdown or HTML
#' @param sig A number vector of the significance level
#'
#' @return
#' @export
#'
#' @examples
#' exp_sup(0.00000456)
#' exp_sup(0.002, md=TRUE, boldsig=TRUE)
exp_sup <- function(x, pvalue = 2, boldsig=FALSE, md=FALSE, sig=0.05) {
   SupStart <-ifelse(md,"^","<sup>")
   SupEnd <- ifelse(md,"^","</sup>")
   BoldStart <- ifelse(md,"**","<b>")
   BoldEnd <- ifelse(md,"**","</b>")
   p_formatted <- ifelse(is.na(x),"",sprintf(paste0("%4.", pvalue, "f \U00D7 10",SupStart,"%d",SupEnd), x / 10^floor(log10(abs(x))), floor(log10(abs(x)))))
   ifelse(x < sig &  boldsig, paste0(BoldStart,p_formatted,BoldEnd),p_formatted)
}
