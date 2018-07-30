#########################################
# model change in working age poverty evenness
#########################################

# run empty model
m1.wkg <- lmer(data=modeldata.wkg, 
               seg.chg ~ (1|supergroup_name), 
               REML = F)
summary(m1.wkg)

# calculate intra-class correlation coefficient
icc.wkg <- 
  VarCorr(m1.wkg) %>% as.data.frame() %>% mutate(vcov.pc = vcov / sum(vcov) ) %>% 
  filter(grp=="supergroup_name") %>% select(vcov.pc) %>%  
  as.numeric()

# with prs predictor at both levels
m2.wkg <- lmer(data=modeldata.wkg, 
               seg.chg ~ prs_diff + prs.gp + (1|supergroup_name), 
               REML = F)
summary(m2.wkg)

# calculate reduction in variance
m2.wkg.r2 <- 
  1 - ( VarCorr(m2.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric

# try full model with scaled variables
m7.wkg <- lmer(data=modeldata.wkg, 
               seg.chg ~ prs_diff + prs.gp + D_2005 + rsl.D + rsl.gp +
                 scale(dilution) + 
                 scale(displace) + 
                 (1|supergroup_name), 
               REML = F)
summary(m7.wkg)

# calculate reduction in variance
m7.wkg.r2 <- 
  1 - ( VarCorr(m7.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()

# try variables at group level
# significant: RSL (11% of variance), dilution (0.5% of variance)
# not significant: D, displace
# summary: include RSL evenness average in final model
m8.wkg <- lmer(data=modeldata.wkg, 
               seg.chg ~ prs_diff + D_2005 + rsl.D + scale(dilution) + scale(displace) +
               prs.gp + rsl.gp +
                 (1|supergroup_name), 
               REML = F)
summary(m8.wkg)
anova(m7.wkg, m8.wkg)
drop1(m8.wkg)
1 - ( VarCorr(m8.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
        VarCorr(m1.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()

m9.wkg <- lmer(data=modeldata.wkg, 
               seg.chg ~ prs_diff + D_2005 + rsl.D + scale(dilution) + scale(displace) +
                 prs.gp + rsl.gp + displace.gp +
                 (1|supergroup_name), 
               REML = F)
summary(m9.wkg)
anova(m8.wkg, m9.wkg)
drop1(m9.wkg)
1 - ( VarCorr(m9.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
        VarCorr(m1.wkg) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()


