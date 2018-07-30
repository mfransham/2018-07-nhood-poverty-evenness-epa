########################################
# model based on other indices
########################################

# collate data
modeldata.ch.alt <- seg.LA.change.ch %>% 
  filter(variable=="G") %>% 
  select(LAD14CD, seg.chg, `2005`) %>% 
  rename(G_2005 = `2005`) %>% 
  left_join(tenure.chg %>% select(GEOGRAPHY_CODE, prs_diff), 
            by=c("LAD14CD"="GEOGRAPHY_CODE") ) %>% 
  left_join(rsl.seg.chg %>% select(LAD14CD, rsl.G), 
            by = "LAD14CD") %>% 
  left_join(dil.disp.ch, by = "LAD14CD") %>% 
  # add supergroup and LA name
  left_join(LAclass %>% select(Lacode, Laname, supergroup_name), 
            by=c("LAD14CD"="Lacode") ) %>% 
  # exclude Isles of Scilly and City of London (1 and 5 LSOAs respectively)
  filter(!LAD14CD %in% c("E06000053", "E09000001") )

# calculate group average and within-group difference for PRS variable
modeldata.ch.alt <- modeldata.ch.alt %>% 
  group_by(supergroup_name) %>% 
  summarise(G.gp = mean(G_2005), 
            prs.gp = mean(prs_diff), 
            rsl.gp = mean(rsl.G), 
            dilution.gp = mean(dilution), 
            displace.gp = mean(displace) ) %>% 
  ungroup() %>% 
  left_join(modeldata.ch.alt, ., by="supergroup_name")

# run empty model
m1.ch.alt <- lmer(data=modeldata.ch.alt, 
              seg.chg ~ (1|supergroup_name), 
              REML = F)
summary(m1.ch.alt)

# ICC
VarCorr(m1.ch.alt) %>% as.data.frame() %>% mutate(vcov.pc = vcov / sum(vcov) ) %>% 
  filter(grp=="supergroup_name") %>% select(vcov.pc) %>%  
  as.numeric()

# try full model 
m7.ch.alt <- lmer(data=modeldata.ch.alt, 
              seg.chg ~ prs_diff + prs.gp + G_2005 + rsl.G + 
                scale(dilution) + scale(displace) + 
                (1|supergroup_name), 
              REML = F)
summary(m7.ch.alt)

# calculate reduction in variance
1 - ( VarCorr(m7.ch.alt) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) )  / 
          VarCorr(m1.ch.alt) %>% as.data.frame() %>%  mutate(var = sdcor^2) %>% select(var) %>% summarise(totalvar = sum(var) ) ) %>% 
  as.numeric()


