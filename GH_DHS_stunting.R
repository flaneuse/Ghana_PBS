
# Import DHS stunting data for comparison to FTF PBS ----------------------
# Laura Hughes, lhughes@usaid.gov, 28 March 2017
# Data is from the 2014 Ghana DHS: http://kidsprogram.com/what-we-do/survey/survey-display-437.cfm


# Setup -------------------------------------------------------------------

library(haven)
library(dplyr)
library(ggplot2)
library(llamar)
library(geocenter)


baseDir = '~/Documents/USAID/Ghana/rawdata/GH_2008_DHS/'
kids = read_stata(paste0(baseDir,"GH_2014_DHS/ghkr72dt/GHKR72FL.DTA"))
kids = read_stata(paste0(baseDir,"GH_2008_DHS/ghkr5adt/GHKR5AFL.DTA"))
geo = geocenter::read_shp(baseDir = paste0(baseDir, "GH_2014_DHS/"), folderName = "ghge71fl", layerName = "GHGE71FL")
geo = geocenter::read_shp(baseDir = paste0(baseDir, "GH_2008_DHS/"), folderName = "ghge5afl", layerName = "GHGE5AFL")
geo = geo@data

# reminder: relevent vars: ------------------------------------------------
# • v024 is the region (1-10); FTF is in 7-10 (Brong Ahafo, Northern, Upper East, Upper West) & lat >= 8 [cuts off some of the Brong Ahafo region]
# • hw70 (stuntingZ) is the height/age std dev from new WHO
# • v022 is the strata
# • v005 is the woman's sampling weight
# • v021 is the primary sampling unit (PSU)


# REMINDER: stunting (hw70), wasting (hw71), underweight (hw72) filters ----------------------------------------------
# "Flagged" cases defined in http://kidsprogram.com/pubs/pdf/DHSG1/Guide_to_DHS_Statistics_29Oct2012_DHSG1.pdf
  # Children with height for age z-scores below –6 SD or above +6 SD, with weight for age z-scores below –6 SD or above +6
  # SD, or with weight for height z-scores below –4 SD or above +6 SD are flagged as having invalid
  # data. Also invalid are combinations of z-scores where height for age is less than –3.09 SD and
  # weight for age is more than +3.09 SD, or where height for age is more than +3.09 SD and weight
  # for age is less than –3.09 SD. 


# • Height/weight measured: !is.na(hw70)
# • Age measured, Age < 60 months: hw1 < 60
# • Flag for whether the kid had valid measurement (hw70  < 9990)
# • Odema = NO (not in DHS modules)
# • De jure (resided in house previous night): hv103 == 1; missing from KR module
# • HAZ, WAZ, or WHZ within bounds: 
#   • stunting/HAZ-- hw70 >= -6 & hw70 <= 6; 
#   • wasting/WAZ-- hw71 >= -6 & hw71 <= 6; 
#   • underweight/WHZ-- hw72 >= -4 & hw72 <= 6
#   • short and stout combo: !(hw70 < -3.09 & hw71 > 3.09)
#   • tall and skinny combo: !(hw70 > 3.09 & hw71 < -3.09)

# relabel stuff. ----------------------------------------------------------
ftfDist = 7:10
lat_thresh = 8 # only include hh with latitudes above the 8 North parallel, since that's how the ZOI is defined.

kids = left_join(kids, geo, by = c("v001" = "DHSCLUST"))

kids = kids %>% 
  mutate(psu = v021,
         strata = v022,
         sampleWeight = v005 / 1e6,
         age_months = hw1,
         weight = hw2/10,
         height = hw3/10,
         stuntingZ = hw70/100,
         underwtZ = hw71/100,
         wastingZ = hw72/100,
         
         stunted = as.numeric(stuntingZ < -2), 
         wasted = as.numeric(wastingZ < -2), 
         underwt = as.numeric(underwtZ < -2),
         ftfFlag = ifelse(v024 %in% ftfDist & LATNUM >= lat_thresh, 1, 0))

kids = kids %>% factorize(kids, 'v024', 'region') %>% factorize(kids, 'b4', 'sex')

# check how many DHS obs aren’t valid -------------------------------------

attr(kids$stuntingZ, 'labels')

# so 9996, 9997, 9998 are all tagged as being invalid.

kids %>% 
  filter(!is.na(hw70)) %>% 
  group_by(hw70 %in% c(9996, 9997, 9998)) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct = n / sum(n))

# 18 (!) out of 2738 invalid. 3146 NAs




# Filter out PBS regions and pull out stunting z-scores -------------------
stunted = kids %>% 
  filter(!is.na(hw70), # height/age measured
         hw70 < 9990, # zscore within bounds
         hw1 < 60, # age < 60 mo.
         stuntingZ <= 6,
         stuntingZ >= -6,
         !(stuntingZ < -3.09 & wastingZ > 3.09), # not unrealistically squatty
         !(stuntingZ > 3.09 & wastingZ < -3.09) # not unrealistically skinny
         ) %>% 
  select(caseid, midx, v024, region, ftfFlag,
         v001, v002, v003, strata, psu, sampleWeight,
         stuntingZ, wastingZ, underwtZ, 
         stunted, wasted, underwt,
         b4, sex,
         hw1, age_months, hw2, weight, hw3, height)



wasted = kids %>% 
  filter(!is.na(hw71), # height/age measured
         hw71 < 9990, # zscore within bounds
         hw1 < 60, # age < 60 mo.
         wastingZ <= 6,
         wastingZ >= -6,
         !(stuntingZ < -3.09 & wastingZ > 3.09), # not unrealistically squatty
         !(stuntingZ > 3.09 & wastingZ < -3.09) # not unrealistically skinny
  ) %>% 
  select(caseid, midx, v024, region, ftfFlag,
         v001, v002, v003, strata, psu, sampleWeight,
         stuntingZ, wastingZ, underwtZ, 
         stunted, wasted, underwt,
         b4, sex,
         hw1, age_months, hw2, weight, hw3, height)


underwt = kids %>% 
  filter(!is.na(hw72), # height/age measured
         hw72 < 9990, # zscore within bounds
         hw1 < 60, # age < 60 mo.
         underwtZ <= 6,
         underwtZ >= -4
  ) %>% 
  select(caseid, midx, v024, region, ftfFlag,
         v001, v002, v003, strata, psu, sampleWeight,
         stuntingZ, wastingZ, underwtZ, 
         stunted, wasted, underwt,
         b4, sex,
         hw1, age_months, hw2, weight, hw3, height)


# calculate stunting ------------------------------------------------------
# expected value: 2014-- 18.8%, N = 2,895; 2008: 28.0%

calcPtEst(df = stunted, var = 'stunted', use_weights = F, psu_var = 'psu', strata_var = 'strata', weight_var = 'sampleWeight')
calcPtEst(df = stunted, var = 'stunted', by_var = 'ftfFlag',
          use_weights = TRUE, psu_var = 'psu', strata_var = 'strata', weight_var = 'sampleWeight')

# calculate wasted --------------------------------------------------------
# expected value: 2014-- 4.7%; 2008: 8.5%
calcPtEst(df = wasted, var = 'wasted', use_weights = T, psu_var = 'psu', strata_var = 'strata', weight_var = 'sampleWeight')
calcPtEst(df = wasted, var = 'wasted', by_var = 'ftfFlag')

# calculate udnerweight --------------------------------------------------------
# expected value: 2014-- 4.7%; 2008: 8.5%
calcPtEst(df = underwt, var = 'underwt', use_weights = T, psu_var = 'psu', strata_var = 'strata', weight_var = 'sampleWeight')
calcPtEst(df = underwt, var = 'underwt', by_var = 'ftfFlag')
# plots -------------------------------------------------------------------

# By region in ZOI
ggplot(stunted %>% filter(ftfFlag == 1), aes(x = stuntingZ)) +
  geom_vline(xintercept = -2, colour = '#b2182b', size = 0.5) +
  geom_histogram(binwidth = 0.25, alpha = 0.4) +
  xlim(c(-6, 6)) +
  facet_wrap(~region) +
  ggtitle('stunting') +
  theme_ygrid()


ggplot(wasted %>% filter(ftfFlag == 1), aes(x = wastingZ)) +
  geom_vline(xintercept = -2, colour = '#b2182b', size = 0.5) +
  geom_histogram(binwidth = 0.25, alpha = 0.4) +
  xlim(c(-6, 6)) +
  facet_wrap(~region) +
  ggtitle('wasting') +
  theme_ygrid()

ggplot(underwt %>% filter(ftfFlag == 1), aes(x = underwtZ)) +
  geom_vline(xintercept = -2, colour = '#b2182b', size = 0.5) +
  geom_histogram(binwidth = 0.25, alpha = 0.4) +
  xlim(c(-6, 6)) +
  facet_wrap(~region) +
  ggtitle('underweight') +
  theme_ygrid()

# ZOI vs. outside
ggplot(stunted %>% filter(ftfFlag == 0), aes(x = stuntingZ)) +
  geom_vline(xintercept = -2, colour = '#b2182b', size = 0.5) +
  geom_density(fill = grey60K, size = 0.2, colour = grey80K, alpha = 0.3) +
  geom_density(fill = ftfBlue, alpha = 0.3,
               size = 0.2, colour = grey80K, 
               data = stunted %>% filter(ftfFlag == 1)) +
  annotate(geom = 'text', x = 1.8, y = 0.27, label = 'outside ZOI',
           family = 'Lato Light', colour = grey60K, size = 6) +
  annotate(geom = 'text', x = 1.8, y = 0.24, label = 'inside ZOI', 
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  xlim(c(-6, 6)) +
  ggtitle('stunting') +
  theme_ygrid()

# stunting vs. wasting
ggplot(stunted, aes(x = stuntingZ, y = wastingZ)) +
  geom_point(size = 3, alpha = 0.3, colour = ftfBlue) +
  xlim(c(-6, 6)) +
  coord_fixed() +
  theme_xygrid()

write.csv(stunted, '~/Documents/USAID/Ghana/processeddata/GH_filteredStuntingZ.csv')
write.csv(wasted, '~/Documents/USAID/Ghana/processeddata/GH_filteredWastingZ.csv')
write.csv(underwt, '~/Documents/USAID/Ghana/processeddata/GH_filteredUnderwtZ.csv')
