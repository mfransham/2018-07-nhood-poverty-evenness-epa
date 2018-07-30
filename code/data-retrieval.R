##############################################
# retrieve income poverty estimates from ID
##############################################

# read in the data
idALL2007 <- read.csv("../../Data/Indices of deprivation/ID2007/idALL2007.csv")
idALL2010 <- read.csv("../../Data/Indices of deprivation/ID2010/idALL2010.csv")
idALL2015 <- read.csv("../../Data/Indices of deprivation/ID2015/idALL2015.csv")

# link ID data to LA classification and region data.  Rename 'English and Welsh Countryside'
LAclass <- read.csv("../../Data/Lookups/LA classification 2011/ONS2011LAclassification.csv")
LAclass$supergroup_name <- LAclass$supergroup_name %>% sub("English and Welsh", "", .)
idALL2007 <- left_join(idALL2007, LAclass[, c(1,3,5)], by=c("LAD15CD"="Lacode"))
idALL2010 <- left_join(idALL2010, LAclass[, c(1,3,5)], by=c("LAD15CD"="Lacode"))
idALL2015 <- left_join(idALL2015, LAclass[, c(1,3,5)], by=c("LAD15CD"="Lacode"))

# need these for analysis of HMRC data
lkupLSOAold <- idALL2007 %>% select(LSOA.CODE, LAD14CD, LANAME, region, supergroup_name)
lkupLSOAnew <- idALL2015 %>% select(LSOA.CODE, LAD14CD, LANAME, region, supergroup_name)

# select required columns: people in poverty, total population, LSOA, LA
poverty2007 <- idALL2007[, c(2,5,7,11,38,32,39,35,41,43,44)]
poverty2010 <- idALL2010[, c(2,5,7,11,56,32,57,35,59,71,72)]
poverty2015 <- idALL2015[, c(2,5,7,12,72,40,73,44,75,77,78)]

# add date indicator
poverty2007$date <- 2005
poverty2010$date <- 2008
poverty2015$date <- 2012

# create consistent names
newnames <- names(poverty2007)
newnames[4:9] <- c("income.sc","poptot","childpov.sc","popchild","penpov.sc","popolder")
names(poverty2007) <- newnames
names(poverty2010) <- newnames
names(poverty2015) <- newnames

# join together
poverty <- rbind(poverty2007, poverty2010, poverty2015)

# calculate absolute numbers
poverty <- mutate(poverty,
    poor.abs = income.sc * poptot, 
    notpoor.abs = poptot - poor.abs,
    older.poor.abs = penpov.sc * popolder,
    older.notpoor.abs = popolder - older.poor.abs,
    child.poor.abs = childpov.sc * popchild,
    child.notpoor.abs = popchild - child.poor.abs,
    wkg.poor.abs = poor.abs - older.poor.abs - child.poor.abs,
    wkg.poor.abs = ifelse(wkg.poor.abs<0, 0, wkg.poor.abs),
    wkg.notpoor.abs = notpoor.abs - older.notpoor.abs - child.notpoor.abs, 
    popwkg = poptot - popchild - popolder, 
    wkgpov.sc = wkg.poor.abs/popwkg)

# tidy up
rm(idALL2007, idALL2010, idALL2015, poverty2007, poverty2010, poverty2015, newnames)
