#########################################
# model change in child poverty evenness
#########################################

# run empty model
m1.ch <- lmer(data=modeldata.ch, 
              seg.chg ~ (1|supergroup_name), 
              REML = F)
summary(m1.ch)
ranef(m1.ch)

# calculate intra-class correlation coefficient
icc.ch <- 
  VarCorr(m1.ch) %>% as.data.frame() %>% mutate(vcov.pc = vcov / sum(vcov) ) %>% 
  filter(grp=="supergroup_name") %>% select(vcov.pc) %>%  
  as.numeric()

# with prs predictor at both levels
m2.ch <- lmer(data=modeldata.ch, 
              seg.chg ~ prs_diff + prs.gp + (1|supergroup_name), 
              REML = F)
summary(m2.ch)

# calculate reduction in variance
m2.ch.r2 <- 
  1 - ( VarCorr(m2.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric

# try full model 
m7.ch <- lmer(data=modeldata.ch, 
              seg.chg ~ prs_diff + prs.gp + D_2005 + rsl.D + 
                scale(dilution) + scale(displace) + 
                (1|supergroup_name), 
              REML = F)
summary(m7.ch)

# calculate reduction in variance
m7.ch.r2 <- 
  1 - ( VarCorr(m7.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()

# try with additional variables at group level
# not significant at group level: D, displace
# significant at group level: RSL (1.8% explained variance), dilution (0.5% explained variance)
# in summary: two variables are stat sig at group level, but only explain 2% additional variance 
# so omitted from final model due to considerations of parsimony
m8.ch <- lmer(data=modeldata.ch, 
              seg.chg ~ D_2005 + prs_diff + rsl.D + scale(dilution) + scale(displace) + 
                prs.gp + rsl.gp +
                (1|supergroup_name), 
              REML = F)
summary(m8.ch)
anova(m7.ch, m8.ch)
1 - ( VarCorr(m8.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
        VarCorr(m1.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()
drop1(m8.ch)

m9.ch <- lmer(data=modeldata.ch, 
              seg.chg ~ D_2005 + prs_diff + rsl.D + scale(dilution) + scale(displace) + 
                prs.gp + rsl.gp + dilution.gp + displace.gp +
                (1|supergroup_name), 
              REML = F)
summary(m9.ch)
anova(m7.ch, m9.ch)
1 - ( VarCorr(m9.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
        VarCorr(m1.ch) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()
drop1(m9.ch)

