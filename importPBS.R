# Read in Ghana DHS
library(haven)
library(dplyr)
library(ggplot2)


dhs = read_stata("~/Downloads/GH_2014_DHS_03162017_1034_89151/ghkr72dt/GHKR72FL.DTA")

# HW70 is the height/age std dev from new WHO

ch_raw = read_stata('~/Downloads/12 - Ghana Data and Docs/M8_Anthropometry_For_ChildrenAll.dta')


hh = read_stata('~/Downloads/12 - Ghana Data and Docs/M1_HH_IdentificationAll.dta')
hh = read_stata('~/Downloads/12 - Ghana Data and Docs/M1_final.dta')


# rename vars for easier use ----------------------------------------------

ch = ch_raw %>% 
  mutate(ea = dense_rank(eacode)) %>% 
  select(hhserial, ChildID, mid, CareGiver_ID, 
         measured = q8_1, under5 = q8_2, age_months = q8_4,
         sex = q8_5, weight = q8_6, height = q8_7, odema = q8_8,
         dejure = q2_06a,
         baseline, region = reg, district = dist, ea, eacode, datasource)
