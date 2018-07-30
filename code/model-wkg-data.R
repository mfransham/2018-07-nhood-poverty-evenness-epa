########################################
# collate data for working age poverty model
########################################

# collate data
modeldata.wkg <- seg.LA.change.wkg %>% 
  filter(variable=="D") %>% 
  select(LAD14CD, seg.chg, `2005`) %>% 
  rename(D_2005 = `2005`) %>% 
  left_join(tenure.chg %>% select(GEOGRAPHY_CODE, prs_diff), 
            by=c("LAD14CD"="GEOGRAPHY_CODE") ) %>% 
  left_join(rsl.seg.chg %>% select(LAD14CD, rsl.D), 
            by = "LAD14CD") %>% 
  left_join(dil.disp.wkg, by = "LAD14CD") %>% 
  # add supergroup and LA name
  left_join(LAclass %>% select(Lacode, Laname, supergroup_name), 
            by=c("LAD14CD"="Lacode") ) %>% 
  # exclude Isles of Scilly and City of London (1 and 5 LSOAs respectively)
  filter(!LAD14CD %in% c("E06000053", "E09000001") )

# calculate group average and within-group difference for PRS variable
modeldata.wkg <- modeldata.wkg %>% 
  group_by(supergroup_name) %>% 
  summarise(D.gp = mean(D_2005), 
            prs.gp = mean(prs_diff), 
            rsl.gp = mean(rsl.D), 
            dilution.gp = mean(dilution), 
            displace.gp = mean(displace) ) %>% 
  ungroup() %>% 
  left_join(modeldata.wkg, ., by="supergroup_name") %>% 
  mutate(prs.gp.diff = prs_diff - prs.gp)

# output the model data
write.csv(modeldata.wkg, "output/modeldata.wkg.csv", row.names = F)
