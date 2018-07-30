################################################
# function to report LR test for nested models
################################################

anova.text <- function(model1, model2) {
  result <- anova(model1, model2)
  pvalue <- ifelse(result[2,8] %>% round(3) < 0.001, 
                   "p<0.001", 
                   paste0("p=", result[2,8] %>% round(3) ) )
  return( paste0("chisq=", result[2,6] %>% round(1), ", ",
                 "df=", result[2,7], ", ", 
                 pvalue) )
}

