###############################################
# model output for working age poverty evenness
###############################################

# create table
# with R2 value, t values and reordered gof statistics
tr.m1.wkg <- texreg::extract(m1.wkg, include.aic = F)
tr.m1.wkg@gof <- c(tr.m1.wkg@gof, icc.wkg, NA)
tr.m1.wkg@gof.names <- c(tr.m1.wkg@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m1.wkg@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m1.wkg@se <- tr.m1.wkg@coef / tr.m1.wkg@se
tr.m1.wkg@gof[5:6] <- tr.m1.wkg@gof[5:6] * 1000

tr.m2.wkg <- texreg::extract(m2.wkg, include.aic = F)
tr.m2.wkg@gof <- c(tr.m2.wkg@gof, NA, m2.wkg.r2)
tr.m2.wkg@gof.names <- c(tr.m2.wkg@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m2.wkg@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m2.wkg@se <- tr.m2.wkg@coef / tr.m2.wkg@se
tr.m2.wkg@gof[5:6] <- tr.m2.wkg@gof[5:6] * 1000

tr.m7.wkg <- texreg::extract(m7.wkg, include.aic = F)
tr.m7.wkg@gof <- c(tr.m7.wkg@gof, NA, m7.wkg.r2)
tr.m7.wkg@gof.names <- c(tr.m7.wkg@gof.names, "Intra-class correlation", "Propn explained variance")
tr.m7.wkg@gof.decimal <- c(rep(F,4), rep(T,4))
tr.m7.wkg@se <- tr.m7.wkg@coef / tr.m7.wkg@se
tr.m7.wkg@gof[5:6] <- tr.m7.wkg@gof[5:6] * 1000

# create final table
tr.wkg.final <- texreg::texreg(l=list(tr.m1.wkg, tr.m2.wkg, tr.m7.wkg), 
                               digits = 3, 
                               custom.coef.names = c("(intercept)",
                                                     "Private rented housing change", 
                                                     "P.R. housing change (group mean)",
                                                     "$D$ in 2005", 
                                                     "Social housing change in $D$", 
                                                     "S.H. change in $D$ (group mean)",
                                                     "Dilution indicator", 
                                                     "Displacement indicator"), 
                               reorder.gof = c(5:8, 1:4), 
                               custom.gof.names = c(rep(NA, 2), "Number of observations", "Number of groups", 
                                                    "Variance(x$10^{3}$): LA supergroup", "Variance(x$10^{3}$): residual", rep(NA,2) ),
                               caption = "Hierarchical linear model of change in D 2005-2012 for working age people", 
                               caption.above = T, 
                               custom.note = "$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$; t-values shown in brackets; variance transformed for readability",
                               label = "tab:tbl-wkg-model")

# create html version for insertion into Word
# texreg::htmlreg(file = "output/table-wkg.doc", 
#                l=list(tr.m1.wkg, tr.m2.wkg, tr.m7.wkg), 
#                digits = 3, 
#                custom.coef.names = c("(intercept)",
#                                      "Private rented housing change", 
#                                      "P.R. housing change (group mean)",
#                                      "$D$ in 2005", 
#                                      "Social housing change in $D$", 
#                                      "S.H. change in $D$ (group mean)",
#                                      "Dilution indicator", 
#                                      "Displacement indicator"), 
#                reorder.gof = c(5:8, 1:4), 
#                custom.gof.names = c(rep(NA, 2), "Number of observations", "Number of groups", 
#                                     "Variance(x$10^{3}$): LA supergroup", "Variance(x$10^{3}$): residual", rep(NA,2) ),
#                caption = "Hierarchical linear model of change in D 2005-2012 for working age people", 
#                caption.above = T, 
#                custom.note = "$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$; t-values shown in brackets; variance transformed for readability",
#                label = "tab:tbl-wkg-model")

# tidy up
rm(tr.m1.wkg, tr.m2.wkg, tr.m7.wkg)
