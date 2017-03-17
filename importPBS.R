# Read in Ghana DHS
library(haven)
library(dplyr)
library(ggplot2)


dhs = read_stata("~/Downloads/GH_2014_DHS_03162017_1034_89151/ghkr72dt/GHKR72FL.DTA")

# HW70 is the height/age std dev from new WHO

pbs = read_stata('~/Downloads/12 - Ghana Data and Docs/M8_Anthropometry_For_ChildrenAll.dta')


hh = read_stata('~/Downloads/12 - Ghana Data and Docs/M1_HH_IdentificationAll.dta')
hh = read_stata('~/Downloads/12 - Ghana Data and Docs/M1_final.dta')
