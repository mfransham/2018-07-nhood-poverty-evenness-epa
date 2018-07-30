###############################################
# model output for child poverty evenness
###############################################

# create table
# with R2 value, t values and reordered gof statistics
tr.m1.ch <- texreg::extract(m1.ch, include.aic = F)
tr.m1.ch@gof <- c(tr.m1.ch@gof, icc.ch, NA)
tr.m1.ch@gof.names <- c(tr.m1.ch@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m1.ch@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m1.ch@se <- tr.m1.ch@coef / tr.m1.ch@se
tr.m1.ch@gof[5:6] <- tr.m1.ch@gof[5:6] * 1000

tr.m2.ch <- texreg::extract(m2.ch, include.aic = F)
tr.m2.ch@gof <- c(tr.m2.ch@gof, NA, m2.ch.r2)
tr.m2.ch@gof.names <- c(tr.m2.ch@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m2.ch@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m2.ch@se <- tr.m2.ch@coef / tr.m2.ch@se
tr.m2.ch@gof[5:6] <- tr.m2.ch@gof[5:6] * 1000

tr.m7.ch <- texreg::extract(m7.ch, include.aic = F)
tr.m7.ch@gof <- c(tr.m7.ch@gof, NA, m7.ch.r2)
tr.m7.ch@gof.names <- c(tr.m7.ch@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m7.ch@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m7.ch@se <- tr.m7.ch@coef / tr.m7.ch@se
tr.m7.ch@gof[5:6] <- tr.m7.ch@gof[5:6] * 1000

# create final table
tr.ch.final <- texreg::texreg(l=list(tr.m1.ch, tr.m2.ch, tr.m7.ch), 
                              digits = 3, 
                              custom.coef.names = c("(intercept)",
                                                    "Private rented housing change", 
                                                    "P.R. housing change (group mean)",
                                                    "$D$ in 2005", 
                                                    "Social housing change in $D$", 
                                                    "Dilution indicator", 
                                                    "Displacement indicator"), 
                              custom.gof.names = c(rep(NA, 2), "Number of observations", "Number of groups", 
                                                   "Variance(x$10^{3}$): LA supergroup", "Variance(x$10^{3}$): residual", rep(NA,2) ),
                              reorder.gof = c(5:8, 1:4), 
                              caption = "Hierarchical linear model of change in D 2005-2012 for children", 
                              caption.above = T, 
                              custom.note = "$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$; t-values shown in brackets; variance transformed for readability",
                              label = "tab:tbl-ch-model")

# create html version for insertion into Word
# (direct conversion to Word does not work well)
# texreg::htmlreg(file = "output/table-children.doc",
#                l=list(tr.m1.ch, tr.m2.ch, tr.m7.ch), 
#                digits = 3, 
#                custom.coef.names = c("(intercept)",
#                                      "Private rented housing change", 
#                                      "P.R. housing change (group mean)",
#                                      "$D$ in 2005", 
#                                      "Social housing change in $D$", 
#                                      "Dilution indicator", 
#                                      "Displacement indicator"), 
#                custom.gof.names = c(rep(NA, 2), "Number of observations", "Number of groups", 
#                                     "Variance(x$10^{3}$): LA supergroup", "Variance(x$10^{3}$): residual", rep(NA,2) ),
#                reorder.gof = c(5:8, 1:4), 
#                caption = "Hierarchical linear model of change in D 2005-2012 for children", 
#                caption.above = T, 
#                custom.note = "$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$; t-values shown in brackets; variance transformed for readability",
#                label = "tab:tbl-ch-model")

# tidy up
rm(tr.m1.ch, tr.m2.ch, tr.m7.ch)
