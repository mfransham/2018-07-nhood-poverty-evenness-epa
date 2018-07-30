###############################################
# calculate indices by LA and region, all ages
###############################################

# indices for England, all age poverty
even.England <- poverty %>% 
  group_by(date) %>% 
  summarise(D = dissimilarity(poor.abs, notpoor.abs),
            D_MD = dissim.MD(poor.abs, poptot),
            Dadj = dissimilarityAdj(poor.abs, notpoor.abs), 
            Gini.MD = Gini.MD(poor.abs, poptot) )

# indices by LA supergroup
even.supergroup <- poverty %>% 
  group_by(supergroup_name, date) %>% 
  summarise(D = dissimilarity(poor.abs, notpoor.abs), 
            D_MD = dissim.MD(poor.abs, poptot),
            Dadj = dissimilarityAdj(poor.abs, notpoor.abs), 
            Gini.MD = Gini.MD(poor.abs, poptot) )

# indices by local authority
even.LA <- poverty %>% 
  group_by(LAD14CD, LANAME, date) %>% 
  summarise(D = dissimilarity(poor.abs, notpoor.abs), 
            D_MD = dissim.MD(poor.abs, poptot),
            Dadj = dissimilarityAdj(poor.abs, notpoor.abs),
            Gini.MD = Gini.MD(poor.abs, poptot), 
            cov10 = coverage(poor.abs, income.sc, 0.1), 
            cov20 = coverage(poor.abs, income.sc, 0.2), 
            A1 = atkinson(poor.abs, poptot, 0.1), 
            A9 = atkinson(poor.abs, poptot, 0.9) )

# scatterplot matrix
pairs(even.LA[,4:11])
cor(even.LA[,4:11])
