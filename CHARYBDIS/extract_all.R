## Scritp for extracting data and filtering by required features comparing between two cohorts

library(dplyr) #para el join
library(tidyr)
library(stringr) #para buscar match en stringa

# For every database, it considers primary care, hospitalized and icu
# If no results for a particular combination it discard them
#it considers no prior for PREMIER, HM, HIRA influenza

# input path: folder where the zip files are, names have to be "Results_database"-like
path_in="//epofs/apistillo/CHARYBDIS/data2/Results_"
path="//epofs/apistillo/CHARYBDIS/data2/Results_"
#output: excel file with the dataset
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions<- read.csv(paste0(path_cond,"conditions_all.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
#conditions_cancer<-read.csv(paste0(path_out,"conditions_cancer.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())
#cohort  name_disease <- "with a COVID-19 diagnosis"
name_disease <- "influenza"
name_disease <- "with a COVID-19 diagnosis"

name<-""
short <- "All"
# file with database with no prior observations available
# set name of database you want to consider

databases<-(list.files(path="//epofs/apistillo/CHARYBDIS/data2")%>%
              str_sub(start=9, end=-5))
         


databases_noprior<-databases

databases_noprior<-""

#functions

fun.createtables <- function(cohort,covariate_value,covariate,
                             id_all, hospitalization,
                             selected_cohort, time, conditions){
  
  total_name <- cohort %>% filter(cohort_id %in% selected_cohort & cohort_id %in% {{id_all}} & cohort_id %in% hospitalization & cohort_id %in% time) %>%
    left_join(covariate_value, by=c("cohort_id","database_id")) %>%
    left_join(covariate, by="covariate_id")%>%
    select(cohort_name, covariate_name, covariate_id, cohort_subjects, mean, sd, cohort_id)
  
  
    total_name<-total_name %>%
    filter(covariate_id %in% {{conditions}})
    
  total=unique(total_name)
}
result.fun <- function(){
  
  merged <- tibble(covariate_id=numeric(), covariate_name=character())

  
  for (i in 1:length(databases)){
    path_in<-paste0(path, databases[i])
    temp = list.files(path=path_in)
    writeLines(paste("Processing", databases[i]))
    cohort<-read.csv(unz(paste0(path_in,".zip"), "cohort.csv"), stringsAsFactors = FALSE)  %>%
      select(cohort_name,cohort_full_name,cohort_id,cohort_type)
    cohort_count<-read.csv(unz(paste0(path_in,".zip"), "cohort_count.csv"), stringsAsFactors = FALSE) 
    cohort<- merge(cohort, cohort_count, by="cohort_id" )
    covariate<-read.csv(unz(paste0(path_in,".zip"), "covariate.csv"), stringsAsFactors = FALSE) 
    covariate_value<-read.csv(unz(paste0(path_in,".zip"), "covariate_value.csv"), stringsAsFactors = FALSE) 
    
    id_all<-(cohort %>% filter(str_detect(cohort_name, name)) %>%filter(str_detect(cohort_name," observation$")))$cohort_id
    
    
    id_cohort <- (cohort %>% filter(str_detect(cohort_name, regex(name_disease, ignore_case=T))&!str_detect(cohort_name, "tested")))$cohort_id
    
    id_Hospitalized<- (cohort %>% filter(str_detect(cohort_name, "hospitalized") & !str_detect(cohort_name,"requiring intensive services")))$cohort_id
    id_Diagnosed <- (cohort %>% filter(!str_detect(cohort_name, "hospitalized")))$cohort_id
    id_ICU<-(cohort %>% filter(str_detect(cohort_name, "requiring intensive services")))$cohort_id
    
    id_prior<- (cohort %>% filter(str_detect(cohort_name, "with at least 365d prior observation")))$cohort_id
    id_noprior <-  (cohort %>% filter(str_detect(cohort_name, "with no required prior observation")))$cohort_id
    
    # Conditions
  id_conditions<-unique(covariate %>% filter(str_detect(tolower(covariate_name), paste0(tolower(conditions$covariate_name),"$",collapse="|"))))$covariate_id
    #I took out > 100 years old
  
  
    
    hospit <- c("Diagnosed", "Hospitalized","ICU")
   #shospit="ICU"
    
    for (j in 1:length(hospit)){
      nome <- paste(short, databases[i], hospit[j], sep="_")
      nonome<-paste0("No_",nome)
      #nonome<-paste("All", databases[i], hospit[j], sep="_")
      sdnome <- paste0(nome, "_sd")
      sdnonome <- paste0(nonome, "_sd")
      smd <- paste0("SMD_",nome)
      writeLines(paste("Processing", hospit[j]))
      id_hospit<-get(paste0("id_",hospit[j]))
     if (databases[i]%in%databases_noprior){id_prior<-id_noprior}
      id_check<-intersect(id_cohort, id_hospit)%>%intersect(id_prior)%>%intersect(id_all)
      if (length(id_check)==0){next}
      total_all<- fun.createtables(cohort,covariate_value,covariate,id_all, id_hospit, id_cohort, id_prior,id_conditions)
      
      if(dim(total_all)[1]==0){next}
      total_pretty <-  total_all%>%select(covariate_name,!!nome:=mean, covariate_id)%>%unique()
      
      merged <-  unique(full_join(unique(merged), total_pretty, by= c("covariate_name", "covariate_id")))
      
     
    }#end for hospitalization
  }#end for
  
  
  merged_pretty<-merged%>%select(starts_with("covariate_name"),ends_with("Diagnosed"), ends_with("Hospitalized"), ends_with("ICU"), covariate_id)%>%
    left_join(conditions, by = c("covariate_name"))
  
}

#------
table<-result.fun()


save(table,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "all.noprior.Rda"))

load("//epofs/apistillo/CHARYBDIS/SavedData/result_all_noprior_covid_11.11_icu.Rda")

merged_pretty<-merged%>%#select(starts_with("covariate_name"),ends_with("primary"), ends_with("hospitalized"), ends_with("uci"), covariate_id, id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD")) %>%arrange(id)

path_out="//epofs/apistillo/CHARYBDIS/Tablas/"
#cargar environment con bases de datos
write.table(merged_pretty,paste0(path_out,"obesity_new_23-07.csv"), sep = ";", row.names = F, dec = ",", na="")



