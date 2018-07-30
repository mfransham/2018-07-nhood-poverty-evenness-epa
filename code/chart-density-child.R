#########################################
# child poverty
# density (change) charts a la Dorling
#########################################

# calculate densities
dens05 <- density(log2(
  filter(poverty, date==2005)$childpov.sc / filter(povrates, date==2005)$povrate.child), 
  from=-4, to=4)
dens08 <- density(log2(
  filter(poverty, date==2008)$childpov.sc / filter(povrates, date==2008)$povrate.child), 
  from=-4, to=4)
dens12 <- density(log2(
  filter(poverty, date==2012)$childpov.sc / filter(povrates, date==2012)$povrate.child), 
  from=-4, to=4)

# gather as data.frame (output as lists)
densxy.ch <- as.data.frame(cbind(dens05$x, dens05$y, dens08$y, dens12$y))
names(densxy.ch) <- c("x","y05","y08","y12")
rm(dens05, dens08, dens12)

# calculate difference in densities
densxy.ch <- mutate(densxy.ch, 
                 chg08 = y08-y05,
                 chg12 = y12-y08) %>% 
  melt(id.vars = "x", measure.vars = c("y05", "y08", "y12", "chg08", "chg12"), value.name = "value")

# plot
ggplot(filter(densxy.ch, variable %in% c("y05","y12","chg08","chg12")), 
       aes(x, value, colour=variable)) + 
  geom_line() +
  theme_minimal() + 
  annotate("text", x=0, y=0.22, label="2005") +
  annotate("text", x=-1, y=0.28, label="2012") +
  annotate("text", x=0, y=-0.01, label="change 2005-2008") +
  annotate("text", x=0, y=0.05, label="change 2008-2012") + 
  annotate("text", x=-2, y=-0.06, label="1/4 national average") +
  annotate("text", x=2, y=-0.06, label="4x national average") +
  annotate("text", x=0, y=-0.06, label="national average") +
  guides(colour="none")
