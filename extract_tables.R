## Scritp for extracting data and filtering by required features comparing between two cohorts

library(dplyr) #para el join
library(tidyr)
library(stringr) #para buscar match en stringa

# For every database, it considers primary care, hospitalized and icu
# If no results for a particular combination it discard them
#it considers no prior for PREMIER, HM, HIRA influenza

# input path: folder where the zip files are, names have to be "Results_database"-like
path_in="//epofs/apistillo/CHARYBDIS/data/Results_"
#output: excel file with the dataset
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions<- read.csv(paste0(path_cond,"age_groups.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
#conditions_cancer<-read.csv(paste0(path_out,"conditions_cancer.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())
#cohort  name_disease <- "with a COVID-19 diagnosis"
#name_disease <- "influenza"
name_disease <- "with a COVID-19 diagnosis"
# name: name of the strata as in shiny app: i.e. "Prevalent obesity" i.e. name <- "Prevalent malignant neoplasm"
# short : name that will be printed on the columns i.e "Obese" short<-"Cancer"
 name<-"Prevalent obesity" 
 short<-"Obese"
#name <- "Prevalent malignant neoplasm
#short<-"Cancer"
# file with database with no prior observations available
# set name of database you want to consider

#
#databases<-c("SIDIAP","SIDIAP-H", "CPRD","HM-Hospitales", "HIRA","PREMIER", "CUIMC", "STARR-OMOP", "IPCI","IQVIA-OpenClaims","optum","VA-OMOP", "Health-verity")
databases<-c("SIDIAP", "STARR-OMOP","CPRD","CUIMC","IQVIA-OpenClaims","SIDIAP-H", "VA-OMOP","Health-verity" )

# include age, change to Y and N, if you want to include age groups like age<18, age>=65
include_age<-"Y"


#functions

fun.createtables <- function(cohort,covariate_value,covariate,
                             id_name, id_noname, hospitalization,
                             selected_cohort, time, conditions){
  
  total_name <- cohort %>% filter(cohort_id %in% selected_cohort & cohort_id %in% {{id_name}} & cohort_id %in% hospitalization & cohort_id %in% time) %>%
    left_join(covariate_value, by=c("cohort_id","database_id")) %>%
    left_join(covariate, by="covariate_id")%>%
    select(cohort_name, covariate_name, covariate_id, cohort_subjects, mean, sd, cohort_id)%>%
    filter(covariate_id %in% {{conditions}})
  
  total_noname<-cohort %>% filter(cohort_id %in% selected_cohort & cohort_id %in% {{id_noname}} & cohort_id %in% hospitalization & cohort_id %in% time) %>%
    left_join(covariate_value, by=c("cohort_id","database_id")) %>%
    left_join(covariate, by="covariate_id")%>%
    select(cohort_name, covariate_name, covariate_id, cohort_subjects, mean, sd, cohort_id)%>%
    filter(covariate_id %in% {{conditions}})
  
  total=unique(full_join(total_name, total_noname, by=c("covariate_name", "covariate_id")))
}

result.fun <- function(){
  ages <- tibble(age=c("Age < 18", "Age >= 65", "Age >= 18", "Age < 65"  ))
  
  merged <- tibble(covariate_id=numeric(), covariate_name=character())
  merge_edad <- tibble(covariate_name = character())
  base_noprior<-vector()
  
  for (i in 1:length(databases)){
    path_in<-paste0(path, databases[i])
    temp = list.files(path=path_in)
    writeLines(paste("Processing", databases[i]))
    cohort<-read.csv(unz(paste0(path_in,".zip"), "cohort.csv"), stringsAsFactors = FALSE) 
    cohort_count<-read.csv(unz(paste0(path_in,".zip"), "cohort_count.csv"), stringsAsFactors = FALSE) 
    cohort<- merge(cohort, cohort_count, by="cohort_id" )
    covariate<-read.csv(unz(paste0(path_in,".zip"), "covariate.csv"), stringsAsFactors = FALSE) 
    covariate_value<-read.csv(unz(paste0(path_in,".zip"), "covariate_value.csv"), stringsAsFactors = FALSE) 
    
    id_name <- (cohort %>% filter(str_detect(cohort_name, name)) %>% filter(str_detect(cohort_name,"with")&!str_detect(cohort_name,"without")))$cohort_id
    id_noname <- (cohort %>% filter(str_detect(cohort_name, name)) %>% filter(str_detect(cohort_name,"without")))$cohort_id
    id_edad <- (cohort %>% filter(str_detect(cohort_name,paste0(ages$age, collapse = "|"))))$cohort_id
    
    id_cohort <- (cohort %>% filter(str_detect(cohort_name, regex(name_disease, ignore_case=T))&!str_detect(cohort_name, "tested")))$cohort_id
    
    id_hospitalized<- (cohort %>% filter(str_detect(cohort_name, "hospitalized") & !str_detect(cohort_name,"requiring intensive services")))$cohort_id
    id_primary <- (cohort %>% filter(!str_detect(cohort_name, "hospitalized")))$cohort_id
    id_uci<-(cohort %>% filter(str_detect(cohort_name, "requiring intensive services")))$cohort_id
    
    id_prior<- (cohort %>% filter(str_detect(cohort_name, "with at least 365d prior observation")))$cohort_id
    id_noprior <-  (cohort %>% filter(str_detect(cohort_name, "with no required prior observation")))$cohort_id
    
    # Conditions
    
    id_conditions <- unique(covariate %>% filter(tolower(covariate_name) %in% trimws(tolower(conditions$covariate_name)) & !covariate_id %in% c(200031,210031,220031,230031), ignore.case=TRUE))$covariate_id
    #I took out > 100 years old
    
    # para cancer
    id_obedad<-(covariate %>% filter(str_detect(covariate_name,trimws("Cohort during day -365 through -1 days overlap the index: Prevalent obesity"))))$covariate_id
    
    hospit <- c("primary", "hospitalized", "uci")
    
    for (j in 1:3){
      nome <- paste(short, databases[i], hospit[j], sep="_")
      nonome<-paste0("No_",nome)
      #nonome<-paste("All", databases[i], hospit[j], sep="_")
      sdnome <- paste0(nome, "_sd")
      sdnonome <- paste0(nonome, "_sd")
      smd <- paste0("SMD_",nome)
      writeLines(paste("Processing", hospit[j]))
      id_hospit<-get(paste0("id_",hospit[j]))
      if (databases[i]=="PREMIER"||databases[i]=="HM-Hospitales"||(databases[i]=="HIRA"&&name_disease=="influenza")){id_prior<-id_noprior; base_noprior<-c(base_noprior, databases[i])}
      id_check<-intersect(id_cohort, id_hospit)%>%intersect(id_prior)%>%intersect(id_name)
      #id_noname<-(intersect(id_cohort, id_hospit)%>%intersect(id_prior))[1]
      if (length(id_check)==0){next}
      total <- fun.createtables(cohort,covariate_value,covariate,id_name, id_noname, id_hospit, id_cohort, id_prior,id_conditions)
      
      if(dim(total)[1]==0){next}
      total_pretty <- unique(total%>% mutate(!!smd:=(abs(mean.x)-abs(mean.y))/(sqrt(sd.x^2+sd.y^2)))%>% select(covariate_name,!!nome:=mean.x,!!sdnome:=sd.x, !!nonome:=mean.y,!!sdnonome:=sd.y,smd, covariate_id))
      
      merged <-  unique(full_join(unique(merged), total_pretty, by= c("covariate_id", "covariate_name")))
      
      if (include_age=="Y"){
        tabla_edad <-  fun.createtables(cohort,covariate_value,covariate,id_edad, "NA", id_hospit, id_cohort, id_prior,id_obedad)%>%separate(cohort_name.x, into = c(NA, "covariate_name"), sep = "observation with", remove = FALSE)
        edad_pretty <- unique(tabla_edad%>% mutate(!!smd:=(abs(mean.x)-abs(mean.y))/(sqrt(sd.x^2+sd.y^2)))%>% select(covariate_name,!!nome:=mean.x,!!sdnome:=sd.x, !!nonome:=mean.y,!!sdnonome:=sd.y,smd, covariate_id))
        merge_edad <- unique(full_join(unique(merge_edad), edad_pretty, by ="covariate_name"))
      }
    }#end for hospitalization
   
  }#end for
  
  merged_pretty<-merged%>%select(starts_with("covariate_name"),ends_with("primary"), ends_with("hospitalized"), ends_with("uci"), covariate_id)%>%
    left_join(conditions, by = "covariate_name")%>%arrange(id)
  
  if (include_age=="Y"){
    merge_edad <- merge_edad%>%select(covariate_name, ends_with("primary"), ends_with("hospitalized"), ends_with("uci"), -starts_with("SMD"))%>%mutate(covariate_id=NA, id = NA)
    merged_pretty<-add_row(merged_pretty, merge_edad)%>%arrange(id)
  }
}

#------
table<-result.fun()


save(table,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "result_cancer_flu_24-07.Rda"))

merged_pretty<-merged%>%select(starts_with("covariate_name"),ends_with("primary"), ends_with("hospitalized"), ends_with("uci"), covariate_id, id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD")) %>%arrange(id)


#cargar environment con bases de datos
write.table(merged_pretty,paste0(path_out,"obesity_new_23-07.csv"), sep = ";", row.names = F, dec = ",", na="")



