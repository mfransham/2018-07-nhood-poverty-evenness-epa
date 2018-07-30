######################################
# create chart that shows distribution
# of independent variables in model
######################################

# collate data
modeldata.ind.vars <- modeldata.ch %>% 
  select(LAD14CD, prs_diff, rsl.D, dilution, displace) %>% 
  rename(dil_ch = dilution, 
         dis_ch = displace) %>% 
  left_join(modeldata.wkg %>% 
              select(LAD14CD, dilution, displace) %>% 
              rename(dil_wkg = dilution, 
                     dis_wkg = displace), 
            by="LAD14CD") %>% 
  left_join(modeldata.op %>% 
              select(LAD14CD, dilution, displace) %>% 
              rename(dil_op = dilution, 
                     dis_op = displace), 
            by="LAD14CD") %>% 
  melt(id.vars="LAD14CD") %>% 
  left_join(data.frame(variable = c("prs_diff", "rsl.D", "dil_ch", "dis_ch",
                                    "dil_wkg", "dis_wkg", "dil_op", "dis_op"), 
                       name = c("Private rented housing change", "Social housing change in D",
                                "Dilution (0-15yr)", "Displacement (0-15yr)", 
                                "Dilution (16-59yr)", "Displacement (16-59yr)", 
                                "Dilution (60+yr)", "Displacement (60+yr)") ), 
            by="variable")

# chart
chart_ind_vars <- 
  ggplot(modeldata.ind.vars, aes(x = name, y = value) ) +
  geom_hline(yintercept = 0, linetype = 1, colour="darkgrey", size = 1) +
  geom_boxplot() + 
  coord_flip() +
  theme_minimal() +
  labs(x=NULL, y=NULL) +
  scale_y_continuous(limits=c(-0.6, 0.6), minor_breaks = F, breaks = round(seq(-0.6, 0.6, 0.2), 1)) +
  geom_text(data = modeldata.ind.vars %>% group_by(name) %>% summarise(sd = sd(value) ) %>% ungroup(), 
            aes(x=name, label=paste0("(", round(sd, 3), ")") ), y=0.5, size=3)
