###########################################
# older poverty - indices of evenness
###########################################

# indices for England, all age poverty
seg.England.op <- poverty %>% 
  group_by(date) %>% 
  summarise(D = dissimilarity(older.poor.abs, older.notpoor.abs), 
            G = Gini.MD(older.poor.abs, popolder))

# indices by region
seg.region.op <- poverty %>% 
  group_by(region, date) %>% 
  summarise(D = dissimilarity(older.poor.abs, older.notpoor.abs), 
            G = Gini.MD(older.poor.abs, popolder))

# indices by LA type
seg.latype.op <- poverty %>% 
  group_by(supergroup_name, date) %>% 
  summarise(D = dissimilarity(older.poor.abs, older.notpoor.abs), 
            G = Gini.MD(older.poor.abs, popolder), 
            A9 = atkinson(older.poor.abs, popolder, 0.9), 
            cov20 = coverage(older.poor.abs, penpov.sc, 0.2) )

# indices by local authority
seg.LA.op <- poverty %>% 
  group_by(LAD14CD, LANAME, date) %>% 
  summarise(D = dissimilarity(older.poor.abs, older.notpoor.abs), 
            G = Gini.MD(older.poor.abs, popolder), 
            A9 = atkinson(older.poor.abs, popolder, 0.9), 
            cov20 = coverage(older.poor.abs, penpov.sc, 0.2) ) %>% 
  ungroup()

# change by local authority
seg.LA.change.op <- seg.LA.op %>% 
  select(-LANAME) %>% 
  melt(id.vars=c("LAD14CD", "date")) %>% 
  dcast(LAD14CD + variable ~ date, value.var="value") %>% 
  mutate(seg.chg = `2012` - `2005`)
