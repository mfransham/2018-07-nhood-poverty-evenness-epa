########################################################################
# can I replicate these results using HMRC data?
# (this is not affected by Census revisions to population denominators)
# answer: yes I can - conclusions are broadly the same using this data
########################################################################

# population denominators (number of children for whom child benefit is claimed)
# were not provided in the HMRC data until the 2011 data point
# population denominators for this earlier period are worked out by using the 
# count and percentage of children in poverty
# when this is zero, population denominators can't be worked out

# read in HMRC data
hmrc <- read.csv("../../Data/HMRC local child poverty measure/lsoas2006-2014.csv")

# note that 2006-2012 used old LSOAs, 2013-2014 used new LSOAs
# original dataset includes all UK small areas - this selects England LSOAs only
# split
hmrcOld <- hmrc %>% filter(date < 2013)
hmrcNew <- hmrc %>% filter(date >=2013)
# apply; this uses lookups created in code/data-retrieval.R
hmrcOld <- left_join(lkupLSOAold, hmrcOld, by=c("LSOA.CODE" = "LSOAcode"))
hmrcNew <- left_join(lkupLSOAnew, hmrcNew, by=c("LSOA.CODE" = "LSOAcode"))
# combine
hmrc <- rbind(hmrcOld, hmrcNew)
# tidy up
rm(hmrcNew, hmrcOld)

# calculate Index of Dissimilarity by region
seg.region.hmrc <- hmrc %>% 
  group_by(region, date) %>% 
  summarise(D = dissimilarity(total_u16, notpoor_u16), 
            G = Gini.MD(total_u16, childben_u16), 
            cov10 = coverage(total_u16, pc_u16, 0.1), 
            cov20 = coverage(total_u16, pc_u16, 0.2))
# this produces NAs because there are NAs in the hmrc dataset

# let's fix this
summary(hmrc$notpoor_u16) # 1735 NAs and one negative number (!)
summary(hmrc$total_u16) # no NAs
hmrc <- hmrc %>% mutate(notpoor_u16 = ifelse(notpoor_u16<0, 0, notpoor_u16))
summary(hmrc$notpoor_u16) # 1735 NAs, now without the negative
# View(hmrc %>% filter(is.na(notpoor_u16))) # this occurs when number of poor children is zero
# set percentages to zero where the value is NA, restrict maximum percentages to 100
hmrc <- hmrc %>% 
  mutate(pc_u16 = ifelse(is.na(pc_u16), 0, pc_u16), 
         pc_all = ifelse(is.na(pc_all), 0, pc_all), 
         pc_u16 = ifelse(pc_u16>100, 100, pc_u16) )

# options - remove NA values or impute

# try removing NAs first
hmrc_NArm <- hmrc %>% filter(!is.na(notpoor_u16))
seg.region.hmrc.NArm <- hmrc_NArm %>% 
  group_by(region, date) %>% 
  summarise(D = dissimilarity(total_u16, notpoor_u16), 
            G = Gini.MD(total_u16, childben_u16), 
            cov10 = coverage(total_u16, pc_u16, 0.1), 
            cov20 = coverage(total_u16, pc_u16, 0.2) )
seg.latype.hmrc.NArm <- hmrc_NArm %>% 
  group_by(supergroup_name, date) %>% 
  summarise(D = dissimilarity(total_u16, notpoor_u16), 
            G = Gini.MD(total_u16, childben_u16), 
            cov10 = coverage(total_u16, pc_u16, 0.1), 
            cov20 = coverage(total_u16, pc_u16, 0.2) )

# impute values instead
samplePopn <- hmrc %>% filter(total_u16 == 5) %>% select(notpoor_u16) # create vector to sample from
hmrc_impute <- hmrc %>% 
  mutate(notpoor_u16 = ifelse(is.na(notpoor_u16), sample(samplePopn$notpoor_u16, 1, replace = T), notpoor_u16), 
         childben_u16 = ifelse(is.na(childben_u16), total_u16 + notpoor_u16, childben_u16) )
summary(hmrc_impute$notpoor_u16) # NAs removed, summary looks similar

# recalculate indices with the imputed data
seg.region.hmrc.impute <- hmrc_impute %>% 
  group_by(region, date) %>% 
  summarise(D = dissimilarity(total_u16, notpoor_u16), 
            G = Gini.MD(total_u16, childben_u16), 
            cov10 = coverage(total_u16, pc_u16, 0.1), 
            cov20 = coverage(total_u16, pc_u16, 0.2))
seg.latype.hmrc.impute <- hmrc_impute %>% 
  group_by(supergroup_name, date) %>% 
  summarise(D = dissimilarity(total_u16, notpoor_u16),
            G = Gini.MD(total_u16, childben_u16), 
            A1 = atkinson(total_u16, childben_u16, 0.1),
            A9 = atkinson(total_u16, childben_u16, 0.9),
            cov10 = coverage(total_u16, pc_u16, 0.1), 
            cov20 = coverage(total_u16, pc_u16, 0.2) ) %>% 
  ungroup()

summary(hmrc_impute$childben_u16)

# what's the difference between the two methods?  Very small
seg.region.hmrc.impute$D - seg.region.hmrc.NArm$D

# tidy up
rm(hmrc, hmrc_NArm, samplePopn, seg.region.hmrc, seg.region.hmrc.NArm, seg.region.hmrc.impute, 
   seg.latype.hmrc.NArm)
