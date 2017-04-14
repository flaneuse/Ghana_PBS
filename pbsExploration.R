setwd('~/Downloads/PBS 2015 Data & Report(08222016)/')

library(dplyr)

parent_id = 0
folder_id = 1
all_files = NULL

dirs = list.dirs(recursive = FALSE)
all_files = bind_rows(all_files, data.frame(file = dirs, 
                                            type = rep('folder', length(dirs)),
                                            folder = rep('PBS 2015 Data & Report(08222016)', length(dirs)),
                                            folder_id = rep(folder_id, length(dirs)),
                                            parent = rep(parent_id, length(dirs))))

files = list.files()
all_files = bind_rows(all_files, data.frame(file = files, 
                                            type = rep('folder', length(dirs)),
                                            folder = rep('PBS 2015 Data & Report(08222016)', length(dirs)),
                                            folder_id = rep(folder_id, length(dirs)),
                                            parent = rep(parent_id, length(dirs))))

library(data.table)
library(readxl)


# look for anything that contains “date" ----------------------------------


setwd('~/Downloads/PBS_METSS (1)/')

dirs = list.dirs(recursive = TRUE)

files = NULL
# for(dir in dirs) {
#   files = c(files, list.files(path = dir))
# }

files = c(files, list.files(recursive = TRUE))


dta = files[grepl('.dta', files)]
xls = files[grepl('.xls', files)]

maybe = NULL

xls = setdiff(xls, "Crenstil_17_08_15/Anima_L/AccessToProductiveCapital7.xlsx")

for (file in dta) {
  # print(file)
  
  string = "ate"
  
  df = read_dta(file)
  
  if(sum(colnames(df) %like% string) > 0) {
    print(file)
    
    maybe = c(maybe, file)
    
    print(paste0('nrow: ', nrow(df)))
    
    sel = df %>% select(-contains('Birth')) %>%  select(contains(string))
    
    
    print(sel %>% summarise_each(funs(sum(!is.na(.)))))
  }
}


maybe = NULL

for (file in xls) {
  print(file)
  
  df = read_excel(file)
  
  if(sum(colnames(df) %like% "Date") > 0) {
    print(file)
    
    maybe = c(maybe, file)
    
    print(paste0('nrow: ', nrow(df)))
    
    sel = df %>% select(-contains('Birth')) %>%  select(contains('Date'))
    1279
    
    print(sel %>% summarise_each(funs(sum(!is.na(.)))))
  }
}
