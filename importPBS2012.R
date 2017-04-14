# Import the 2012 baseline household survey data fro FTF Ghana
# Data downloaded from USAID's open data site on 7 April 2017
# https://www.usaid.gov/data/dataset/f722cc77-3e00-4a78-9211-90044db5740a


# setuup ------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(llamar)

baseDir = '~/Documents/USAID/Ghana/rawdata/GH_2012_PBS/'


# import data -------------------------------------------------------------

ch2012 = read.csv(paste0(baseDir, 'MODULE 8 - CHILDREN ANTHROPOMETRY.csv'))

# Assuming variable names haven't changed between survey mods, since there's no documentation...
ch2012 = ch2012 %>%          
  mutate(age_months = q8_4, sex = q8_5, weight = q8_6, height = q8_7, odema = q8_8)

# import DHS --------------------------------------------------------------
source("GH_DHS_stunting.R")



# ploting HEIGHT FOR AGE --------------------------------------------------



# Zooming in just on the most plausible data.
ggplot(ch2012, aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = dhs) +
  annotate(geom = 'text', x = 60, y = 20, label = 'PBS (2012 baseline) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 10, label = 'Ghana DHS data (all country)', 
           hjust = 1,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  annotate(geom = 'text', x = 6.5, y = 130, label = '6 months', 
           hjust = 0,
           family = 'Lato Light', colour = grey75K, size = 4) +
  ggtitle('The breadth of heights-for-ages in children is much wider in the PBS') +
  xlim(c(0, 60)) +
  ylim(c(0, max(stunted$height)*1.25)) +
  theme_xygrid()


# DISTRIBUTION plots -------------------------------------------------------
combMeas = bind_rows(ch2012 %>% 
                       select(age_months, height, weight) %>% 
                       mutate(dataset = 'PBS', ftfFlag = 1), 
                     stunted %>% 
                       select(age_months, height, weight, ftfFlag) %>% 
                       mutate(dataset = 'DHS')) %>% 
  mutate(HA = height/age_months,
         WA = weight/age_months,
         HW = height/weight)


ggplot(combMeas, aes(x = age_months, fill = dataset)) +
  geom_bar(alpha = 0.5) +
  facet_wrap(~dataset, scales = 'free_y') +
  xlim(c(0, 60)) +
  scale_fill_manual(values = c(ftfBlue, ftfOrange)) +
  ggtitle('Ages in the PBS snap to years, not months') +
  theme_xgrid()


# WHO stds ----------------------------------------------------------------

whoM = read.delim('~/Documents/USAID/WHO_growth_stds/lhfa_boys_z_exp.txt')
whoF = read.delim('~/Documents/USAID/WHO_growth_stds/lhfa_girls_z_exp.txt')

whoM = whoM %>% mutate(age_months = Day/30)
whoF = whoF %>% mutate(age_months = Day/30)

ggplot(ch2012, aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = dhs) +
  # geom_path(aes(x = age_months, y = SD4neg), 
  #           colour = grey90K, linetype = 2,
  #           data = whoM) +
  geom_path(aes(x = age_months, y = SD4neg), 
            colour = grey90K, linetype = 2,
            data = whoF) +
  geom_path(aes(x = age_months, y = SD4), 
            colour = grey90K, linetype = 2,
            data = whoM) +
  # geom_path(aes(x = age_months, y = SD4), 
  #           colour = grey90K, linetype = 2,
  #           data = whoF) +
  annotate(geom = 'segment', x = 35, xend = 39, y = 30, yend = 30, label = 'WHO standards, 4 SD',
           linetype = 2, colour = grey90K, size = 0.5) +
  annotate(geom = 'text', x = 60, y = 30, label = 'WHO standards, 4 SD',
           hjust = 1,
           family = 'Lato Light', colour = grey90K, size = 6) +
  annotate(geom = 'text', x = 60, y = 20, label = 'PBS (2012 baseline) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 10, label = 'Ghana DHS data (all country)', 
           hjust = 1,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  annotate(geom = 'text', x = 6.5, y = 130, label = '6 months', 
           hjust = 0,
           family = 'Lato Light', colour = grey75K, size = 4) +
  ggtitle('The breadth of heights-for-ages in children is much wider in the PBS') +
  xlim(c(0, 60)) +
  ylim(c(0, max(stunted$height)*1.25)) +
  theme_xygrid()
