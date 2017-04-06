# Read in Ghana DHS
library(haven)
library(dplyr)
library(ggplot2)
library(knitr)

baseDir = '~/Documents/USAID/Ghana/rawdata/GH_2015_PBS/'

# HW70 is the height/age std dev from new WHO

ch_raw = read_stata(paste0(baseDir, 'M8_Anthropometry_For_ChildrenAll.dta'))


# hh = read_stata(paste0(baseDir, 'M1_HH_IdentificationAll.dta'))
# hh = read_stata(paste0(baseDir, 'M1_HH_IdentificationAll(1).dta'))
# hh = read_stata(paste0(baseDir, 'M1_final.dta'))

test = read_stata('~/Documents/USAID/Ghana/rawdata/GH_2015_PBS/M2_merge.dta')

# rename vars for easier use ----------------------------------------------

ch = ch_raw %>% 
  mutate(ea = dense_rank(eacode)) %>% 
  select(hhserial, ChildID, mid, CareGiver_ID, 
         measured = q8_1, under5 = q8_2, age_months = q8_4,
         sex = q8_5, weight = q8_6, height = q8_7, odema = q8_8,
         dejure = q2_06a,
         baseline, region = reg, district = dist, ea, eacode, datasource) %>% 
  filter(!is.na(age_months), !is.na(weight), !is.na(height))


# import DHS --------------------------------------------------------------
source("GH_DHS_stunting.R")


# plotting “stunting” for DHS and PBS -------------------------------------

# Orange = raw, unfiltered PBS data.  Blue is all Ghana DHS
ggplot(ch, aes(y = height, x = age_months)) +
         geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = stunted) +
  annotate(geom = 'text', x = 80, y = 200, label = 'PBS (unfiltered) data',
           hjust = 0,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 80, y = 150, label = 'Ghana DHS data (all country)', 
           hjust = 0,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  ggtitle('The breadth of heights-for-ages in children is much wider in the PBS') +
  theme_xygrid()

# Zooming in just on the most plausible data.
# Note: facetting by sex, filtering out just the 4 FTF regions in DHS don't seem to give obvious differences.
ggplot(ch, aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = dhs) +
  annotate(geom = 'text', x = 60, y = 20, label = 'PBS (unfiltered) data',
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

# Including just the "baseline" PBS data:
ggplot(ch %>% filter(baseline == 1), aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = dhs) +
  annotate(geom = 'text', x = 60, y = 20, label = 'PBS (unfiltered baseline) data',
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


# coloring by baseline data:
ggplot(ch, aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(aes(colour = factor(baseline), alpha = factor(baseline)), size = 3) +
  annotate(geom = 'text', x = 60, y = 20, label = 'PBS (unfiltered baseline) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 10, label = 'PBS (unfiltered added in interim) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfGreen, size = 6) +
  annotate(geom = 'text', x = 6.5, y = 130, label = '6 months', 
           hjust = 0,
           family = 'Lato Light', colour = grey75K, size = 4) +
  scale_color_manual(values = c(ftfGreen, ftfOrange, grey60K)) +
  scale_alpha_manual(values = c(0.35, 0.2, 0.35)) +
  ggtitle('The breadth of heights-for-ages in children is much wider in the PBS') +
  xlim(c(0, 60)) +
  ylim(c(0, max(stunted$height)*1.25)) +
  theme_xygrid(legend.position = 'top')


# Including implausible values
ggplot(ch, aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(aes(colour = hw70 > 9000, alpha = hw70 > 9000), size = 3,
             data = dhs %>% filter(!is.na(hw70))) +
  annotate(geom = 'text', x = 60, y = 20, label = 'PBS (unfiltered) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 10, label = 'Ghana DHS data (all country)', 
           hjust = 1,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  annotate(geom = 'text', x = 60, y = 0, label = 'Ghana DHS data (implausible values)', 
           hjust = 1,
           family = 'Lato Light', colour = grey70K, size = 6) +
  annotate(geom = 'text', x = 6.5, y = 130, label = '6 months', 
           hjust = 0,
           family = 'Lato Light', colour = grey75K, size = 4) +
  scale_color_manual(values = c(ftfBlue, grey70K)) +
  scale_alpha_manual(values = c(0.25, 1)) +
  ggtitle('The breadth of heights-for-ages in children is much wider in the PBS') +
  xlim(c(0, 60)) +
  ylim(c(0, max(stunted$height)*1.25)) +
  theme_xygrid()

# just FTF regions of DHS
ggplot(ch, aes(y = height, x = age_months)) +
  geom_vline(xintercept = 6) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = stunted %>% filter(ftfFlag == 1)) +
  xlim(c(0, 60)) +
  ylim(c(0, max(stunted$height)*1.25)) +
  theme_xygrid()


# wasting plots -----------------------------------------------------------
ggplot(ch, aes(y = weight, x = age_months)) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = stunted) +
  annotate(geom = 'text', x = 70, y = 120, label = 'PBS (unfiltered) data',
           hjust = 0,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 70, y = 50, label = 'Ghana DHS data (all country)', 
           hjust = 0,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  ggtitle('The breadth of weights-for-ages in children is much wider in the PBS') +
  theme_xygrid()


# zoom
ggplot(ch, aes(y = weight, x = age_months)) +
  geom_vline(xintercept = 6, colour = grey75K, size = 0.5) +
  geom_point(size = 3, alpha = 0.25, colour = ftfOrange) +
  geom_point(size = 3, alpha = 0.25, colour = ftfBlue,
             data = stunted) +
  annotate(geom = 'text', x = 60, y = 2, label = 'PBS (unfiltered) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 5, label = 'Ghana DHS data (all country)', 
           hjust = 1,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  annotate(geom = 'text', x = 6.5, y = 25, label = '6 months', 
           hjust = 0,
           family = 'Lato Light', colour = grey75K, size = 4) +
  ggtitle('The breadth of weights-for-ages in children is much wider in the PBS') +
  xlim(c(0, 60)) +
  ylim(c(0, max(stunted$weight)*1.25)) +
  theme_xygrid()


# comparing distributions of height, age, weight --------------------------
combMeas = bind_rows(ch %>% 
                       select(age_months, height, weight) %>% 
                       mutate(dataset = 'PBS', ftfFlag = 1), 
                     stunted %>% 
                       select(age_months, height, weight, ftfFlag) %>% 
                       mutate(dataset = 'DHS')) %>% 
  mutate(HA = height/age_months,
         WA = weight/age_months,
         HW = height/weight)


# -- AGE --

# truncating at 60 months, even though there are a couple of random skewed obs.
ggplot(combMeas, aes(x = age_months, fill = dataset)) +
  geom_bar(alpha = 0.5) +
  facet_wrap(~dataset, scales = 'free_y') +
  xlim(c(0, 60)) +
  scale_fill_manual(values = c(ftfBlue, ftfOrange)) +
  ggtitle('Ages in the PBS have a peak < 6, suggesting they are years, not months') +
  theme_xgrid()

View(combMeas %>% 
  filter(ftfFlag == 1) %>% 
  group_by(dataset) %>% 
  summarise(N = n(), 
            min = min(age_months),
            mean = round(mean(age_months), 2), 
            median = median(age_months), 
            max = max(age_months),
            std = round(sd(age_months),2))
)

# -- HEIGHT --

View(combMeas %>% 
       filter(ftfFlag == 1) %>% 
       group_by(dataset) %>% 
       summarise(N = n(), 
                 min = min(height),
                 mean = round(mean(height), 2), 
                 median = median(height), 
                 max = max(height),
                 std = round(sd(height),2))
)


ggplot(combMeas, aes(x = height, fill = dataset, colour = dataset)) +
  geom_density(alpha = 0.5, size = 0.5) +
  # geom_histogram(alpha = 0.5, binwidth = 5) +
  # facet_wrap(~dataset, scales = 'free_y') +
  xlim(c(0, max(stunted$height)*1.25)) +
  scale_colour_manual(values = c(ftfBlue, ftfOrange)) +
  scale_fill_manual(values = c(ftfBlue, ftfOrange)) +
  annotate(geom = 'text', x = 60, y = 0.0175, label = 'PBS (unfiltered) data',
          hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 0.02, label = 'Ghana DHS data (all country)', 
           hjust = 1,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  xlab('height (cm)') +
  ggtitle("Heights in the PBS appear to have more values at the tails of the distribution") +
  theme_xgrid()


ggplot(combMeas %>% filter(ftfFlag == 1), aes(x = height, fill = dataset, colour = dataset)) +
  geom_density(alpha = 0.5, size = 0.5) +
  # geom_histogram(alpha = 0.5, binwidth = 5) +
  # facet_wrap(~dataset, scales = 'free_y') +
  xlim(c(0, max(stunted$height)*1.25)) +
  scale_colour_manual(values = c(ftfBlue, ftfOrange)) +
  scale_fill_manual(values = c(ftfBlue, ftfOrange)) +
  annotate(geom = 'text', x = 60, y = 0.0175, label = 'PBS (unfiltered) data',
           hjust = 1,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 60, y = 0.02, label = 'Ghana DHS data (FTF region)', 
           hjust = 1,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  xlab('height (cm)') +
  ggtitle("Heights in the PBS appear to have more values at the tails of the distribution") +
  theme_xgrid()

# -- WEIGHT --

View(combMeas %>% 
       filter(ftfFlag == 1) %>% 
       group_by(dataset) %>% 
       summarise(N = n(), 
                 min = min(weight),
                 mean = round(mean(weight), 2), 
                 median = median(weight), 
                 max = max(weight),
                 std = round(sd(weight),2))
)

# filtering out the 2,200 kg child:
View(combMeas %>% 
       filter(ftfFlag == 1, weight < 200) %>% 
       group_by(dataset) %>% 
       summarise(N = n(), 
                 min = min(weight),
                 mean = round(mean(weight), 2), 
                 median = median(weight), 
                 max = max(weight),
                 std = round(sd(weight),2))
)


ggplot(combMeas, aes(x = weight, fill = dataset, colour = dataset)) +
  geom_density(alpha = 0.5, size = 0.5) +
  # geom_histogram(alpha = 0.5, binwidth = 5) +
  # facet_wrap(~dataset, scales = 'free_y') +
  xlim(c(0, max(stunted$weight)*1.25)) +
  scale_colour_manual(values = c(ftfBlue, ftfOrange)) +
  scale_fill_manual(values = c(ftfBlue, ftfOrange)) +
  annotate(geom = 'text', x = 17, y = 0.075, label = 'PBS (unfiltered) data',
           hjust = 0,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 17, y = 0.065, label = 'Ghana DHS data (all country)', 
           hjust = 0,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  xlab('weight (kg)') +
  ggtitle("Weights in the PBS appear to be systemmatically heavier than the DHS") +
  theme_xgrid()

# Just FTF
ggplot(combMeas %>% filter(ftfFlag == 1), aes(x = weight, fill = dataset, colour = dataset)) +
  geom_density(alpha = 0.5, size = 0.5) +
  # geom_histogram(alpha = 0.5, binwidth = 5) +
  # facet_wrap(~dataset, scales = 'free_y') +
  xlim(c(0, max(stunted$weight)*1.25)) +
  scale_colour_manual(values = c(ftfBlue, ftfOrange)) +
  scale_fill_manual(values = c(ftfBlue, ftfOrange)) +
  annotate(geom = 'text', x = 17, y = 0.075, label = 'PBS (unfiltered) data',
           hjust = 0,
           family = 'Lato Light', colour = ftfOrange, size = 6) +
  annotate(geom = 'text', x = 17, y = 0.065, label = 'Ghana DHS data (FTF region)', 
           hjust = 0,
           family = 'Lato Light', colour = ftfBlue, size = 6) +
  xlab('weight (kg)') +
  ggtitle("Weights in the PBS appear to be systemmatically heavier than the DHS") +
  theme_xgrid()

# -- SEX --
ch %>% group_by(sex) %>% summarise(n = n()) %>% ungroup() %>% mutate(pct = percent(n/sum(n)))
stunted %>% group_by(sex) %>% summarise(n = n()) %>% ungroup() %>% mutate(pct = percent(n/sum(n)))





# quick t-tests: are distributions different? -----------------------------
# comparisons: PBS to DHS, all children w/ valid stunting data

t.test(stunted$age_months, ch$age_months)

# Welch Two Sample t-test
# 
# data:  stunted$age_months and ch$age_months
# t = 7.6955, df = 6002.8, p-value = 1.637e-14
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.415425 4.066675
# sample estimates:
#   mean of x mean of y 
# 28.05294  24.81189 


t.test(stunted$b4, ch$sex)

# Welch Two Sample t-test
# 
# data:  stunted$b4 and ch$sex
# t = 0.33344, df = 5608.6, p-value = 0.7388
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.01942952  0.02739359
# sample estimates:
#   mean of x mean of y 
# 1.482721  1.478739 


t.test(stunted$height, ch$height)
# Welch Two Sample t-test
# 
# data:  stunted$height and ch$height
# t = 1.8635, df = 7627.3, p-value = 0.06243
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.04154323  1.64187321
# sample estimates:
#   mean of x mean of y 
# 83.68772  82.88756 

t.test(stunted$weight, ch$weight)

# Welch Two Sample t-test
# 
# data:  stunted$weight and ch$weight
# t = -2.9141, df = 5142.5, p-value = 0.003583
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -2.2229873 -0.4349042
# sample estimates:
#   mean of x mean of y 
# 11.21360  12.54255 

st_dhs = combMeas %>% filter(dataset == 'DHS', !is.infinite(HA))
st_pbs = combMeas %>% filter(dataset == 'PBS', !is.infinite(HA))

# height for weight
t.test(ch_dhs$HW, ch_pbs$HW)

# height for weight -- breaks?

t.test(ch_dhs$WA, ch_pbs$WA)


# Kolmogorov-Smirnov tests ------------------------------------------------


ks.test(ch_dhs$HA, ch_pbs$HA, alternative = 'greater')
ks.test(ch_dhs$HA, ch_pbs$HA, alternative = 'less')
ks.test(ch_dhs$WA, ch_pbs$WA, alternative = 'greater')
ks.test(ch_dhs$WA, ch_pbs$WA, alternative = 'less')
ks.test(ch_dhs$HW, ch_pbs$HW, alternative = 'greater')
ks.test(ch_dhs$HW, ch_pbs$HW, alternative = 'less')
ks.test(ch_dhs$weight, ch_pbs$weight, alternative = 'greater')
ks.test(ch_dhs$height, ch_pbs$height, alternative = 'less')
ks.test(ch_dhs$height, ch_pbs$height, alternative = 'greater')
ks.test(ch_dhs$age_months, ch_pbs$age_months, alternative = 'less') # D^- = 0.12557, p-value < 2.2e-16
ks.test(ch$sex, stunted$b4, alternative = 'greater') # D^+ = 0.003982, p-value = 0.946
ks.test(ch$sex, stunted$b4, alternative = 'less') 

# Double-checking: is FTF region different than the sum total?
ftf_dhs = ch_dhs %>% filter(ftfFlag == 1)
notftf_dhs = ch_dhs %>% filter(ftfFlag == 0)

ks.test(ch_dhs$HA, ftf_dhs$HA, alternative = 'greater') # D^+ = 0.027115, p-value = 0.288
ks.test(ch_dhs$HA, ftf_dhs$HA, alternative = 'less') # D^- = 0.0094535, p-value = 0.8596
ks.test(ch_dhs$WA, ftf_dhs$WA, alternative = 'less') # D^- = 0.017922, p-value = 0.5805
ks.test(ch_dhs$WA, ftf_dhs$WA, alternative = 'greater') # D^+ = 0.014869, p-value = 0.6878
# FTF region has different height-for-weight distribution than total.
ks.test(ch_dhs$HW, ftf_dhs$HW, alternative = 'greater') 
ks.test(ch_dhs$HW, ftf_dhs$HW, alternative = 'less') 
ks.test(ch_dhs$weight, ftf_dhs$weight, alternative = 'greater') # D^+ = 1.507e-17, p-value = 1
# Weight is close to being lower.
ks.test(ch_dhs$weight, ftf_dhs$weight, alternative = 'less') # D^- = 0.040001, p-value = 0.06661
ks.test(ch_dhs$height, ftf_dhs$height, alternative = 'less') # D^- = 0.035225, p-value = 0.1224
ks.test(ch_dhs$height, ftf_dhs$height, alternative = 'greater') # D^+ = 0.00045739, p-value = 0.9996
ks.test(ch_dhs$age_months, ftf_dhs$age_months, alternative = 'less') # D^- = 0.026742, p-value = 0.298

# And last double check: comparing FTF zones to outside ftf
# HA, HW, weight is lower w/i.
ks.test(notftf_dhs$HA, ftf_dhs$HA, alternative = 'greater') # D^+ = 0.027115, p-value = 0.288
ks.test(notftf_dhs$HA, ftf_dhs$HA, alternative = 'less') # D^- = 0.0094535, p-value = 0.8596
ks.test(notftf_dhs$WA, ftf_dhs$WA, alternative = 'less') # D^- = 0.017922, p-value = 0.5805
ks.test(notftf_dhs$WA, ftf_dhs$WA, alternative = 'greater') # D^+ = 0.014869, p-value = 0.6878
ks.test(notftf_dhs$HW, ftf_dhs$HW, alternative = 'greater') # D^+ = 0.095134, p-value = 2.215e-07
ks.test(notftf_dhs$HW, ftf_dhs$HW, alternative = 'less') # D^- = 0.72492, p-value < 2.2e-16
ks.test(notftf_dhs$weight, ftf_dhs$weight, alternative = 'greater') # D^+ = 1.507e-17, p-value = 1
ks.test(notftf_dhs$weight, ftf_dhs$weight, alternative = 'less') # D^- = 0.040001, p-value = 0.06661
ks.test(notftf_dhs$height, ftf_dhs$height, alternative = 'less') # D^- = 0.035225, p-value = 0.1224
ks.test(notftf_dhs$height, ftf_dhs$height, alternative = 'greater') # D^+ = 0.00045739, p-value = 0.9996
ks.test(notftf_dhs$age_months, ftf_dhs$age_months, alternative = 'less') # D^- = 0.026742, p-value = 0.298


# OKAY.  To be fair, should probably just compare ZOI to ZOI.

ks.test(ftf_dhs$HA, ch_pbs$HA, alternative = 'greater')
ks.test(ftf_dhs$HA, ch_pbs$HA, alternative = 'less')
ks.test(ftf_dhs$WA, ch_pbs$WA, alternative = 'greater')
ks.test(ftf_dhs$WA, ch_pbs$WA, alternative = 'less')
ks.test(ftf_dhs$HW, ch_pbs$HW, alternative = 'greater')
ks.test(ftf_dhs$HW, ch_pbs$HW, alternative = 'less')
ks.test(ftf_dhs$weight, ch_pbs$weight, alternative = 'greater')
ks.test(ftf_dhs$height, ch_pbs$height, alternative = 'less')
ks.test(ftf_dhs$height, ch_pbs$height, alternative = 'greater')
ks.test(ftf_dhs$age_months, ch_pbs$age_months, alternative = 'less') # D^- = 0.12557, p-value < 2.2e-16
ks.test(ch$sex, stunted$b4, alternative = 'greater') # D^+ = 0.003982, p-value = 0.946
ks.test(ch$sex, stunted$b4, alternative = 'less') 

# AND still true.  everything is different, except sex.