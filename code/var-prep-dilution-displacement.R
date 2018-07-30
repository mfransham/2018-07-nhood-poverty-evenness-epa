#####################################
# variable prep
# population dilution / displacement
#####################################

# collate data to compare across time periods
pov.comp <- read.csv("C:/Users/DPhil/OneDrive for Business/DPhil/Data/Lookups/LSOA01_LSOA11_LAD11_EW_LU.csv") %>% 
  select(-LSOA01NM, -LSOA11NM) %>% 
  filter(substr(LSOA01CD, 1, 1) == "E") %>% 
  left_join(poverty %>% 
              filter(date==2005) %>% 
              select(LSOA.CODE, LAD14CD, 
                     childpov.sc, child.poor.abs, popchild, 
                     wkgpov.sc, wkg.poor.abs, popwkg, 
                     penpov.sc, older.poor.abs, popolder) %>% 
              rename(pc_ch_05 = childpov.sc, poor_ch_05 = child.poor.abs, pop_ch_05 = popchild, 
                     pc_wkg_05 = wkgpov.sc, poor_wkg_05 = wkg.poor.abs, pop_wkg_05 = popwkg, 
                     pc_op_05 = penpov.sc, poor_op_05 = older.poor.abs, pop_op_05 = popolder), 
            by=c("LSOA01CD" = "LSOA.CODE") ) %>% 
  left_join(poverty %>% 
              filter(date==2012) %>% 
              select(LSOA.CODE, 
                     childpov.sc, child.poor.abs, popchild, 
                     wkgpov.sc, wkg.poor.abs, popwkg, 
                     penpov.sc, older.poor.abs, popolder) %>% 
              rename(pc_ch_12 = childpov.sc, poor_ch_12 = child.poor.abs, pop_ch_12 = popchild, 
                     pc_wkg_12 = wkgpov.sc, poor_wkg_12 = wkg.poor.abs, pop_wkg_12 = popwkg, 
                     pc_op_12 = penpov.sc, poor_op_12 = older.poor.abs, pop_op_12 = popolder), 
            by=c("LSOA11CD" = "LSOA.CODE") )

# sum values in 2005 for those LSOAs that were merged
pov.comp.merged <- pov.comp %>% 
  filter(CHGIND == "M" | CHGIND == "X") %>% 
  group_by(LSOA11CD) %>% 
  summarise(pc_ch_05_m = weighted.mean(pc_ch_05, pop_ch_05), 
            poor_ch_05_m = sum(poor_ch_05), 
            pop_ch_05_m = sum(pop_ch_05), 
            pc_wkg_05_m = weighted.mean(pc_wkg_05, pop_wkg_05), 
            poor_wkg_05_m = sum(poor_wkg_05), 
            pop_wkg_05_m = sum(pop_wkg_05),
            pc_op_05_m = weighted.mean(pc_op_05, pop_op_05), 
            poor_op_05_m = sum(poor_op_05), 
            pop_op_05_m = sum(pop_op_05) ) %>% 
  ungroup()

# sum values in 2012 for those LSOAs that were split
pov.comp.split <- pov.comp %>% 
  filter(CHGIND == "S") %>% 
  group_by(LSOA01CD) %>% 
  summarise(pc_ch_12_s = weighted.mean(pc_ch_12, pop_ch_12), 
            poor_ch_12_s = sum(poor_ch_12), 
            pop_ch_12_s = sum(pop_ch_12), 
            pc_wkg_12_s = weighted.mean(pc_wkg_12, pop_wkg_12), 
            poor_wkg_12_s = sum(poor_wkg_12), 
            pop_wkg_12_s = sum(pop_wkg_12) , 
            pc_op_12_s = weighted.mean(pc_op_12, pop_op_12), 
            poor_op_12_s = sum(poor_op_12), 
            pop_op_12_s = sum(pop_op_12) ) %>% 
  ungroup()

# collate
pov.comp <- pov.comp %>% 
  left_join(pov.comp.merged, by="LSOA11CD") %>% 
  left_join(pov.comp.split, by="LSOA01CD") %>% 
  mutate(pc_ch_05_comp = ifelse(is.na(pc_ch_05_m), pc_ch_05, pc_ch_05_m), 
         poor_ch_05_comp = ifelse(is.na(poor_ch_05_m), poor_ch_05, poor_ch_05_m), 
         pop_ch_05_comp = ifelse(is.na(pop_ch_05_m), pop_ch_05, pop_ch_05_m), 
         pc_ch_12_comp = ifelse(is.na(pc_ch_12_s), pc_ch_12, pc_ch_12_s), 
         poor_ch_12_comp = ifelse(is.na(poor_ch_12_s), poor_ch_12, poor_ch_12_s), 
         pop_ch_12_comp = ifelse(is.na(pop_ch_12_s), pop_ch_12, pop_ch_12_s) ) %>% 
  mutate(pc_wkg_05_comp = ifelse(is.na(pc_wkg_05_m), pc_wkg_05, pc_wkg_05_m), 
         poor_wkg_05_comp = ifelse(is.na(poor_wkg_05_m), poor_wkg_05, poor_wkg_05_m), 
         pop_wkg_05_comp = ifelse(is.na(pop_wkg_05_m), pop_wkg_05, pop_wkg_05_m), 
         pc_wkg_12_comp = ifelse(is.na(pc_wkg_12_s), pc_wkg_12, pc_wkg_12_s), 
         poor_wkg_12_comp = ifelse(is.na(poor_wkg_12_s), poor_wkg_12, poor_wkg_12_s), 
         pop_wkg_12_comp = ifelse(is.na(pop_wkg_12_s), pop_wkg_12, pop_wkg_12_s) ) %>% 
  mutate(pc_op_05_comp = ifelse(is.na(pc_op_05_m), pc_op_05, pc_op_05_m), 
         poor_op_05_comp = ifelse(is.na(poor_op_05_m), poor_op_05, poor_op_05_m), 
         pop_op_05_comp = ifelse(is.na(pop_op_05_m), pop_op_05, pop_op_05_m), 
         pc_op_12_comp = ifelse(is.na(pc_op_12_s), pc_op_12, pc_op_12_s), 
         poor_op_12_comp = ifelse(is.na(poor_op_12_s), poor_op_12, poor_op_12_s), 
         pop_op_12_comp = ifelse(is.na(pop_op_12_s), pop_op_12, pop_op_12_s) ) %>% 
  select(LSOA01CD, CHGIND, LAD14CD, 
         pc_ch_05_comp, poor_ch_05_comp, pop_ch_05_comp, 
         pc_ch_12_comp, poor_ch_12_comp, pop_ch_12_comp, 
         pc_wkg_05_comp, poor_wkg_05_comp, pop_wkg_05_comp, 
         pc_wkg_12_comp, poor_wkg_12_comp, pop_wkg_12_comp, 
         pc_op_05_comp, poor_op_05_comp, pop_op_05_comp, 
         pc_op_12_comp, poor_op_12_comp, pop_op_12_comp) %>% 
  distinct()

# check: what difference does this make to D?
pov.ch.comp.D <- pov.comp %>% 
  group_by(LAD14CD) %>% 
  summarise(D2005 = dissim.MD(poor_ch_05_comp, pop_ch_05_comp), 
         D2012 = dissim.MD(poor_ch_12_comp, pop_ch_12_comp) ) %>% 
  mutate(Dchg = D2012 - D2005) %>% 
  ungroup()

# collate with non-adjusted calculation of D
comp.D <- pov.ch.comp.D %>% 
  select(LAD14CD, Dchg) %>% 
  left_join(seg.LA.change.ch %>% 
              filter(variable=="D") %>% 
              select(LAD14CD, seg.chg), 
            by = "LAD14CD" )

# very little difference for most LAs.  Largest effects of adjustment are to make D bigger
plot(Dchg ~ seg.chg, data = comp.D)

# calculate dilution / displacement metrics
dil.disp.ch <- pov.comp %>% 
  group_by(LAD14CD) %>% 
  filter(percent_rank(pc_ch_05_comp) > 0.8) %>% 
  summarise(dilution = log(sum(pop_ch_05_comp) / sum(pop_ch_12_comp) ), 
            displace = log(sum(poor_ch_12_comp) / sum(poor_ch_05_comp) ) ) %>% 
  ungroup()

# for working age
dil.disp.wkg <- pov.comp %>% 
  group_by(LAD14CD) %>% 
  filter(percent_rank(pc_wkg_05_comp) > 0.8) %>% 
  summarise(dilution = log(sum(pop_wkg_05_comp) / sum(pop_wkg_12_comp) ), 
            displace = log(sum(poor_wkg_12_comp) / sum(poor_wkg_05_comp) ) ) %>% 
  ungroup()

# for older people
dil.disp.op <- pov.comp %>% 
  group_by(LAD14CD) %>% 
  filter(percent_rank(pc_op_05_comp) > 0.8) %>% 
  summarise(dilution = log(sum(pop_op_05_comp) / sum(pop_op_12_comp) ), 
            displace = log(sum(poor_op_12_comp) / sum(poor_op_05_comp) ) ) %>% 
  ungroup()

# tidy up
rm(pov.comp.merged, pov.comp.split, pov.ch.comp.D, comp.D)
