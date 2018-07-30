##########################################################
# time series chart for child poverty evenness measures
##########################################################

# chart CLIF and IDID measures on one plot
chart_seg.latype.allCPmeasures <- 
  ggplot(seg.latype.hmrc.impute %>% melt(id.vars = c("supergroup_name", "date"), value.var="value"), 
       aes(x=date, y=value)) +
  geom_line(aes(colour=variable)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~supergroup_name, nrow=2) +
  theme_minimal() + 
  scale_y_continuous(minor_breaks = F) +
  scale_x_continuous(minor_breaks = F, breaks = seq(2004,2014,2)) + 
  geom_text(data = seg.latype.hmrc.impute %>% melt(id.vars = c("supergroup_name", "date"), value.var="value") %>% filter(date==2013), 
            aes(label=variable, x=2013, y = ifelse(variable=="cov10", value-0.02, value+0.02)),
            size = 3) +
  geom_point(data = seg.latype.ch %>% melt(id.vars = c("supergroup_name", "date"), value.var="value"), 
             aes(x=date, y=value, colour=variable, shape=variable)) +
  guides(colour="none", shape="none") +
  labs(x="Year", y="Index of concentration") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        panel.spacing = unit(1, "lines"))

# CLIF measures only
chart_seg.latype.hmrcdata <- 
  ggplot(seg.latype.hmrc.impute %>% 
           select(-cov10, -A1, -cov20) %>% 
           melt(id.vars = c("supergroup_name", "date"), value.var="value"), 
         aes(x=date, y=value)) +
  geom_line(aes(colour=variable)) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(~supergroup_name, nrow=2) +
  theme_minimal() + 
  scale_y_continuous(minor_breaks = F) +
  scale_x_continuous(minor_breaks = F, breaks = seq(2004,2014,2)) + 
  geom_text(data = seg.latype.hmrc.impute %>% 
              select(-cov10, -A1, -cov20) %>% 
              melt(id.vars = c("supergroup_name", "date"), value.var="value") %>% filter(date==2013), 
            aes(label=variable, x=2013, y = value+0.02),
            size = 3) +
  guides(colour="none", shape="none") +
  labs(x="Year", y="Index of evenness") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        panel.spacing = unit(1, "lines"))
