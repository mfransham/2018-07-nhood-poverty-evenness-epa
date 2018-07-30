####################################################
# construct indicator of shift to PRS
# amongst lower NS-SeC groups
####################################################

# need a measure of this at LA level
# access via Nomis API
# tenure 2001 is for All Household Reference Persons (HRPs) aged 16 to 74
# tenure 2011 is for All Household Reference Persons aged 16 and over
tenure2001 <- read.csv("C:/Users/DPhil/OneDrive for Business/DPhil/Data/Census tenure/tenure-by-nssec-la-2001-nomis.csv", 
                       stringsAsFactors = F)
tenure2011 <- read.csv("C:/Users/DPhil/OneDrive for Business/DPhil/Data/Census tenure/tenure-by-nssec-la-2011-nomis.csv", 
                       stringsAsFactors = F)

# tidy up and create percentages
tenure2001.tbl <- tenure2001 %>% 
  select(GEOGRAPHY_NAME, GEOGRAPHY_CODE, C_NSSEC, C_TENHUK11_NAME, OBS_VALUE) %>% 
  filter(C_NSSEC != 0) %>% 
  mutate(TENURE = ifelse(C_TENHUK11_NAME %in% c("Other social rented", "Rented from council"), "Social rented", C_TENHUK11_NAME)) %>% 
  group_by(GEOGRAPHY_NAME, GEOGRAPHY_CODE, TENURE) %>% 
  summarise(count = sum(OBS_VALUE)) %>% 
  ungroup() %>% 
  group_by(GEOGRAPHY_NAME, GEOGRAPHY_CODE) %>% 
  mutate(percent = count/sum(count/2)*100) %>% 
  ungroup() %>% 
  mutate(date=2001)

# and for 2011
tenure2011.tbl <- tenure2011 %>% 
  select(GEOGRAPHY_NAME, GEOGRAPHY_CODE, C_NSSEC, C_TENHUK11_NAME, OBS_VALUE) %>% 
  filter(C_NSSEC != 0) %>% 
  group_by(GEOGRAPHY_NAME, GEOGRAPHY_CODE, C_TENHUK11_NAME) %>% 
  summarise(count = sum(OBS_VALUE)) %>% 
  ungroup() %>% 
  group_by(GEOGRAPHY_NAME, GEOGRAPHY_CODE) %>% 
  mutate(percent = count/sum(count/2)*100) %>% 
  ungroup() %>% 
  mutate(date=2011) %>% 
  rename(TENURE = C_TENHUK11_NAME)

# combine
tenure.tbl <- bind_rows(tenure2001.tbl, tenure2011.tbl)
tenure.chg <- tenure.tbl %>% 
  filter(substr(TENURE, 1, 7) == "Private") %>% 
  dcast(GEOGRAPHY_NAME + GEOGRAPHY_CODE ~ date, value.var = "percent") %>% 
  # transform to proportion rather than percent
  mutate(`2001` = `2001`/100, 
         `2011` = `2011`/100) %>% 
  # calculate difference
  mutate(prs_diff = `2011` - `2001`)

# look at the distribution
hist(tenure.chg$prs_diff)
boxplot(tenure.chg$prs_diff)

# tidy up
rm(tenure2001, tenure2011, tenure2001.tbl, tenure2011.tbl)


