###########################################
# wkg poverty - indices of evenness
###########################################

# indices for England, all age poverty
seg.England.wkg <- poverty %>% 
  group_by(date) %>% 
  summarise(D = dissimilarity(wkg.poor.abs, wkg.notpoor.abs), 
            G = Gini.MD(wkg.poor.abs, popwkg))

# indices by region
seg.region.wkg <- poverty %>% 
  group_by(region, date) %>% 
  summarise(D = dissimilarity(wkg.poor.abs, wkg.notpoor.abs), 
            G = Gini.MD(wkg.poor.abs, popwkg))

# indices by LA type
seg.latype.wkg <- poverty %>% 
  group_by(supergroup_name, date) %>% 
  summarise(D = dissimilarity(wkg.poor.abs, wkg.notpoor.abs), 
            G = Gini.MD(wkg.poor.abs, popwkg), 
            A9 = atkinson(wkg.poor.abs, popwkg, 0.9), 
            cov20 = coverage(wkg.poor.abs, wkgpov.sc, 0.2) )

# indices by local authority
seg.LA.wkg <- poverty %>% 
  group_by(LAD14CD, LANAME, date) %>% 
  summarise(D = dissimilarity(wkg.poor.abs, wkg.notpoor.abs), 
            G = Gini.MD(wkg.poor.abs, popwkg), 
            A9 = atkinson(wkg.poor.abs, popwkg, 0.9), 
            cov20 = coverage(wkg.poor.abs, wkgpov.sc, 0.2) ) %>% 
  ungroup()

# change by local authority
seg.LA.change.wkg <- seg.LA.wkg %>% 
  select(-LANAME) %>% 
  melt(id.vars=c("LAD14CD", "date")) %>% 
  dcast(LAD14CD + variable ~ date, value.var="value") %>% 
  mutate(seg.chg = `2012` - `2005`)
