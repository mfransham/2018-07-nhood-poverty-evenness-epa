######################################
# ID poverty rates by date
######################################

# create table of poverty rates at each time point
povrates <- poverty %>% 
  group_by(date) %>% 
  summarise(poor.abs = sum(poor.abs), 
            poptot = sum(poptot), 
            child.poor.abs = sum(child.poor.abs), 
            popchild = sum(popchild), 
            wkg.poor.abs = sum(wkg.poor.abs), 
            popwkg = sum(popwkg), 
            older.poor.abs = sum(older.poor.abs), 
            popolder = sum(popolder) ) %>% 
  mutate(povrate = poor.abs/poptot, 
         povrate.child = child.poor.abs / popchild, 
         povrate.wkg = wkg.poor.abs / popwkg, 
         povrate.older = older.poor.abs / popolder) %>% 
  ungroup()

# by LA class
povrates.supergroup <- poverty %>% 
  group_by(date, supergroup_name) %>% 
  summarise(poor.abs = sum(poor.abs), 
            poptot = sum(poptot), 
            child.poor.abs = sum(child.poor.abs), 
            popchild = sum(popchild), 
            wkg.poor.abs = sum(wkg.poor.abs), 
            popwkg = sum(popwkg), 
            older.poor.abs = sum(older.poor.abs), 
            popolder = sum(popolder) ) %>% 
  mutate(povrate = poor.abs/poptot, 
         povrate.child = child.poor.abs / popchild, 
         povrate.wkg = wkg.poor.abs / popwkg, 
         povrate.older = older.poor.abs / popolder) %>% 
  ungroup() %>% 
  arrange(supergroup_name, date)

# by LA
povrates.la <- poverty %>% 
  group_by(date, LAD14CD, LANAME) %>% 
  summarise(poor.abs = sum(poor.abs), 
            poptot = sum(poptot), 
            child.poor.abs = sum(child.poor.abs), 
            popchild = sum(popchild), 
            wkg.poor.abs = sum(wkg.poor.abs), 
            popwkg = sum(popwkg), 
            older.poor.abs = sum(older.poor.abs), 
            popolder = sum(popolder) ) %>% 
  mutate(povrate = poor.abs/poptot, 
         povrate.child = child.poor.abs / popchild, 
         povrate.wkg = wkg.poor.abs / popwkg, 
         povrate.older = older.poor.abs / popolder) %>% 
  ungroup() 
