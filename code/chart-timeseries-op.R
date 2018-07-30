##########################################################
# time series chart for older age poverty evenness measures
##########################################################
chart_seg.latype.op <- 
  ggplot(seg.latype.op %>% 
           select(-cov20) %>% 
           melt(id.vars = c("supergroup_name", "date"), value.var="value"), 
       aes(x=date, y=value)) +
  geom_line(aes(colour=variable)) +
  scale_color_brewer(palette="Set1") +
  facet_wrap(~supergroup_name, nrow=2) +
  theme_minimal() + 
  scale_y_continuous(minor_breaks = F) +
  scale_x_continuous(minor_breaks = F, breaks = c(2005,2008,2012)) +
  guides(colour="none", shape="none") +
  labs(x="Year", y="Index of evenness") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        panel.spacing = unit(1, "lines")) + 
  geom_text(data = seg.latype.op %>%  
              select(-cov20) %>% 
              melt(id.vars = c("supergroup_name", "date"), value.var="value") %>% filter(date==2008), 
            aes(label=variable, x=2010, y = value + 0.018),
            size = 3) 

