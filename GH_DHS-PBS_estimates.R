
# Comparing Ghana PBS point estimates to DHS ------------------------------
# GH_DHS-PBS_estimates.R
# Laura Hughes, lhughes@usaid.gov
# 14 April 2017



# The data ----------------------------------------------------------------
# Baseline PBS data is from 2012; published report available at https://feedthefuture.gov/sites/default/files/resource/files/Feed_the_Future_Ghana_Baseline_Report_English.pdf


# 2012 Baseline PBS data --------------------------------------------------
# Stats were pulled by hand from report.  All stats reported at the "all ZOI" level
# https://feedthefuture.gov/sites/default/files/resource/files/Feed_the_Future_Ghana_Baseline_Report_English.pdf
# lower bound and upper bound for confidence limits reported as estimate +/- 2 SE

# Table C.5, Table D.3
underwtWomen_2012 =  data.frame(avg = 0.1201, lb = 0.1028, ub = 0.1373, n = 4120, metric = 'underwtWomen', year = 2012, survey = 'PBS')
# Table C.9, D.7
underwtKids_2012 = data.frame(avg = 0.1841, lb = 0.1583, ub = 0.2099, metric = 'underwtKids', year = 2012, survey = 'PBS', n = 2960)
# Table C.10, D.8
stuntedKids_2012 = data.frame(avg = 0.3608, lb = 0.3293, ub = 0.3923, metric = 'stuntedKids', year = 2012, survey = 'PBS', n = 2960)
# Table C.11, D.9
wastedKids_2012 = data.frame(avg = 0.1099, lb = 0.0940, ub = 0.1259, metric = 'wastedKids', year = 2012, survey = 'PBS', n = 2960)

pbs2012 = bind_rows(stuntedKids_2012, wastedKids_2012, underwtKids_2012, underwtWomen_2012)


# 2015 Baseline PBS data --------------------------------------------------
# Stats were pulled by hand from Draft report "Final Updated Report Ghana PBS 2015 JULY (+BFS comments)_Resubmitted Jan.2017.docx"
# Table 0.1
underwtWomen_2015 = data.frame(avg = 12.2, lb = 10.2, ub = 14.2, n = 3560, metric = 'underwtWomen', year = 2015, survey = 'PBS')
stuntedKids_2015 = data.frame(avg = 29.9, lb = 27.0, ub = 32.8, n = 2318, metric = 'stuntedKids', year = 2015, survey = 'PBS')
underwtKids_2015 = data.frame(avg = 19.0, lb = 16.4, ub = 21.6, n = 2318, metric = 'underwtKids', year = 2015, survey = 'PBS')
wastedKids_2015 = data.frame(avg = 13.8, lb = 10.9, ub = 16.7, n = 2281, metric = 'wastedKids', year = 2015, survey = 'PBS')

pbs2015 = bind_rows(stuntedKids_2015, wastedKids_2015, underwtKids_2015, underwtWomen_2015) %>% 
  mutate(avg = avg/100, lb = lb/100, ub = ub/100)



# Calculate DHS statistics ------------------------------------------------


