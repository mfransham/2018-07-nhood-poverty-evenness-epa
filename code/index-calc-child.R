###########################################
# child poverty - indices of evenness
###########################################

# indices for England, all age poverty
seg.England.ch <- poverty %>% 
  group_by(date) %>% 
  summarise(D = dissimilarity(child.poor.abs, child.notpoor.abs), 
            G = Gini.MD(child.poor.abs, popchild))

# indices by region
seg.region.ch <- poverty %>% 
  group_by(region, date) %>% 
  summarise(D = dissimilarity(child.poor.abs, child.notpoor.abs), 
            G = Gini.MD(child.poor.abs, popchild))

# indices by LA type
seg.latype.ch <- poverty %>% 
  group_by(supergroup_name, date) %>% 
  summarise(D = dissimilarity(child.poor.abs, child.notpoor.abs), 
            G = Gini.MD(child.poor.abs, popchild), 
            A1 = atkinson(child.poor.abs, popchild, 0.1), 
            A9 = atkinson(child.poor.abs, popchild, 0.9), 
            cov10 = coverage(child.poor.abs, childpov.sc, 0.1), 
            cov20 = coverage(child.poor.abs, childpov.sc, 0.2) )

# indices by local authority
seg.LA.ch <- poverty %>% 
  group_by(LAD14CD, LANAME, date) %>% 
  summarise(D = dissimilarity(child.poor.abs, child.notpoor.abs), 
            G = Gini.MD(child.poor.abs, popchild), 
            A1 = atkinson(child.poor.abs, popchild, 0.1), 
            A9 = atkinson(child.poor.abs, popchild, 0.9), 
            cov10 = coverage(child.poor.abs, childpov.sc, 0.1), 
            cov20 = coverage(child.poor.abs, childpov.sc, 0.2) ) %>% 
  ungroup()

# adjustment to D makes tiny differences
# Rutland in 2012 - difference of 0.017.  Otherwise < 0.01, most < 0.001
# seg.LA.ch <- mutate(seg.LA.ch, diff = Dadj - D)

# change by local authority (D)
seg.LA.change.ch <- seg.LA.ch %>% 
  select(-LANAME) %>% 
  melt(id.vars=c("LAD14CD", "date")) %>% 
  dcast(LAD14CD + variable ~ date, value.var="value") %>% 
  mutate(seg.chg = `2012` - `2005`)

# correlations in change across measures
seg.LA.change.ch %>% dcast(LAD14CD ~ variable, value.var = "seg.chg") %>% 
  select(-LAD14CD) %>% pairs()
seg.LA.change.ch %>% dcast(LAD14CD ~ variable, value.var = "seg.chg") %>% 
  select(-LAD14CD) %>% cor()
