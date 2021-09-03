# rescatar periodo bases de datos hypertension

library(dplyr) #para el join
library(tidyr)
library(stringr) #para buscar match en stringa

path_in="//epofs/apistillo/CHARYBDIS/data2/Results_"
path="//epofs/apistillo/CHARYBDIS/data2/Results_"
#output: excel file with the dataset

name_disease <- "with a COVID-19 diagnosis"
databases <- c(databases_primary, databases_hospit)
table <- tibble(n=1)
for (i in 1:length(databases)){
  base <- databases[i]
  path_in<-paste0(path, databases[i])
  writeLines(paste("Processing", databases[i]))
  cohort.st <- read.csv(unz(paste0(path_in,".zip"), "cohort_staging_count.csv"), stringsAsFactors = FALSE) %>%  
    filter(str_detect(tolower(name), "with index date"), str_detect(name, "with a COVID-19 diagnosis")) %>% 
    separate(name, sep="with Index date: ", into=c(NA, "month")) %>% 
    filter(!is.na(cohort_entries))  %>% 
    distinct(month)
  val <- tibble(duration=paste0(cohort.st %>% slice_head(n=1), "-", cohort.st %>% slice_tail(n=1))) %>% 
    rename(!!base:=duration) 
  
  table <- table %>% cbind(val)
}
library(htmlTable  )
table %>% htmlTable()  
  