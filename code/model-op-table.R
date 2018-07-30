###############################################
# model output for older age poverty evenness
###############################################

# create table
# with R2 value, t values and reordered gof statistics
tr.m1.op <- texreg::extract(m1.op, include.aic = F)
tr.m1.op@gof <- c(tr.m1.op@gof, icc.op, NA)
tr.m1.op@gof.names <- c(tr.m1.op@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m1.op@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m1.op@se <- tr.m1.op@coef / tr.m1.op@se
tr.m1.op@gof[5:6] <- tr.m1.op@gof[5:6] * 1000

tr.m2.op <- texreg::extract(m2.op, include.aic = F)
tr.m2.op@gof <- c(tr.m2.op@gof, NA, m2.op.r2)
tr.m2.op@gof.names <- c(tr.m2.op@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m2.op@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m2.op@se <- tr.m2.op@coef / tr.m2.op@se
tr.m2.op@gof[5:6] <- tr.m2.op@gof[5:6] * 1000

tr.m7.op <- texreg::extract(m7.op, include.aic = F)
tr.m7.op@gof <- c(tr.m7.op@gof, NA, m7.op.r2)
tr.m7.op@gof.names <- c(tr.m7.op@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m7.op@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m7.op@se <- tr.m7.op@coef / tr.m7.op@se
tr.m7.op@gof[5:6] <- tr.m7.op@gof[5:6] * 1000

# create final table
tr.op.final <- texreg::texreg(l=list(tr.m1.op, tr.m2.op, tr.m7.op), 
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
                              caption = "Hierarchical linear model of change in D 2005-2012 for older people", 
                              caption.above = T, 
                              custom.note = "$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$; t-values shown in brackets; variance transformed for readability",
                              label = "tab:tbl-op-model")

# create version for Word
# texreg::htmlreg(file = "output/table-olderpeople.doc", 
#                l=list(tr.m1.op, tr.m2.op, tr.m7.op), 
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
#                caption = "Hierarchical linear model of change in D 2005-2012 for older people", 
#                caption.above = T, 
#                custom.note = "$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$; t-values shown in brackets; variance transformed for readability",
#                label = "tab:tbl-op-model")

# tidy up
rm(tr.m1.op, tr.m2.op, tr.m7.op)


