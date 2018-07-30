# exploring change in evenness indices
seg.latype.op %>% 
  select(supergroup_name, date, D) %>% 
  dcast(supergroup_name ~ date, value.var="D") %>% 
  select(supergroup_name, `2005`, `2012`) %>% 
  mutate(segchg = `2012`-`2005`) %>% 
  View()
