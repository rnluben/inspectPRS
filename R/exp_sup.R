exp_sup <- function(x, pvalue = 2, boldsig=FALSE, md=FALSE, sig=0.05) {
   SupStart <-ifelse(md,"^","<sup>")
   SupEnd <- ifelse(md,"^","</sup>")
   BoldStart <- ifelse(md,"**","<b>")
   BoldEnd <- ifelse(md,"**","</b>")
   p_formatted <- ifelse(is.na(x),"",sprintf(paste0("%4.", pvalue, "f \U00D7 10",SupStart,"%d",SupEnd), x / 10^floor(log10(abs(x))), floor(log10(abs(x)))))
   ifelse(x < sig &  boldsig, paste0(BoldStart,p_formatted,BoldEnd),p_formatted)
}
