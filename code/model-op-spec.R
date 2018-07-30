#########################################
# model change in working age poverty evenness
#########################################

# run empty model
m1.op <- lmer(data=modeldata.op, 
               seg.chg ~ (1|supergroup_name), 
               REML = F)
summary(m1.op)

# calculate intra-class correlation coefficient
icc.op <- 
  VarCorr(m1.op) %>% as.data.frame() %>% mutate(vcov.pc = vcov / sum(vcov) ) %>% 
  filter(grp=="supergroup_name") %>% select(vcov.pc) %>%  
  as.numeric()

# with prs predictor at both levels
m2.op <- lmer(data=modeldata.op, 
               seg.chg ~ prs_diff + prs.gp + (1|supergroup_name), 
               REML = F)
summary(m2.op)

# how does this compare to empty model?  Not significant, as expected
anova(m1.op, m2.op)

# calculate reduction in variance
m2.op.r2 <- 
  1 - ( VarCorr(m2.op) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.op) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric

# just original value of segregation
m3.op <- lmer(data=modeldata.op, 
              seg.chg ~ D_2005 + (1|supergroup_name), 
              REML = F)
summary(m3.op)
# also not significant
anova(m1.op, m3.op)

# try full model with scaled variables
m7.op <- lmer(data=modeldata.op, 
               seg.chg ~ D_2005 + rsl.D + 
                 scale(dilution) + 
                 scale(displace) + 
                 (1|supergroup_name), 
               REML = F)
summary(m7.op)

# calculate reduction in variance
m7.op.r2 <- 
  1 - ( VarCorr(m7.op) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.op) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()

# try explanatory variables at group level
# not significant: D, RSL, dilution, displace
# none significant - do not include in model
m8.op <- lmer(data=modeldata.op, 
              seg.chg ~ D_2005 + rsl.D + scale(dilution) + scale(displace) + 
                displace.gp +
                (1|supergroup_name), 
              REML = F)
summary(m8.op)
anova(m7.op, m8.op)
1 - ( VarCorr(m8.op) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
        VarCorr(m1.op) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()
