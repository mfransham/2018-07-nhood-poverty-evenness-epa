################################################
# prep indicator of change in RSL segregation
################################################

# read data and link LA names and codes
tenure.lsoa.01 <- read.csv("C:/Users/DPhil/OneDrive for Business/DPhil/Data/Census tenure/UV063-tenure-lsoa-2001.csv", 
                           stringsAsFactors = F) %>% 
  left_join(lkupLSOAold %>% select(LSOA.CODE, LAD14CD, LANAME), by = c("mnemonic"="LSOA.CODE") )
tenure.lsoa.11 <- read.csv("C:/Users/DPhil/OneDrive for Business/DPhil/Data/Census tenure/KS402EW-tenure-lsoa-2011.csv", 
                           stringsAsFactors = F) %>% 
  left_join(lkupLSOAnew %>% select(LSOA.CODE, LAD14CD, LANAME), by = c("mnemonic"="LSOA.CODE") )

# RSL segregation indices for each date
rsl.seg.2001 <- tenure.lsoa.01 %>% 
  group_by(LAD14CD, LANAME) %>% 
  summarise(rsl.D = dissim.MD(Social.rented..Total, All.categories..Tenure) , 
            rsl.G = Gini.MD(Social.rented..Total, All.categories..Tenure), 
            rsl.A9 = atkinson(Social.rented..Total, All.categories..Tenure, 0.9) ) %>% 
  ungroup() %>% 
  mutate(date = 2001)

rsl.seg.2011 <- tenure.lsoa.11 %>% 
  group_by(LAD14CD, LANAME) %>% 
  summarise(rsl.D = dissim.MD(Social.rented, All.households), 
            rsl.G = Gini.MD(Social.rented, All.households), 
            rsl.A9 = atkinson(Social.rented, All.households, 0.9) ) %>% 
  ungroup() %>% 
  mutate(date = 2011)

# look at correlation - very high, as before
pairs(rsl.seg.2001[, 3:5]); pairs(rsl.seg.2011[, 3:5])
cor(rsl.seg.2001[, 3:5]); cor(rsl.seg.2011[, 3:5]); 

# calculate change
rsl.seg.chg <- bind_rows(rsl.seg.2001, rsl.seg.2011) %>%
  melt(measure.vars = c("rsl.D", "rsl.G", "rsl.A9")) %>% 
  dcast(LAD14CD + variable ~ date, value.var = "value") %>% 
  mutate(rsl_diff = `2011` - `2001`) %>% 
  dcast(LAD14CD ~ variable, value.var="rsl_diff")

# correlation of changes
pairs(rsl.seg.chg[,2:4])
cor(rsl.seg.chg[,2:4])

# look at distribution
hist(rsl.seg.chg$rsl.D)
boxplot(rsl.seg.chg$rsl.D)  

# tidy up
rm(tenure.lsoa.01, tenure.lsoa.11, rsl.seg.2001, rsl.seg.2011)
