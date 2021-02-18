## Scritp for extracting data and filtering by required features comparing between two cohorts

library(dplyr) #para el join
library(tidyr)
library(stringr) #para buscar match en stringa

# For every database, it considers primary care, hospitalized and icu
# If no results for a particular combination it discard them


# input path: folder where the zip files are, names have to be "Results_database"-like
path_in="//epofs/apistillo/CHARYBDIS/data2/Results_"
path=path_in
#output: excel file with the dataset
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
#conditions<- read.csv(paste0(path_cond,"conditions_hypertension.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
conditions<- read.csv(paste0(path_cond,"conditions_all.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
conditions<- read.csv(paste0(path_cond,"conditions_pediatry.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
#conditions_cancer<-read.csv(paste0(path_cond,"conditions_cancer_last.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())
#cohort  name_disease <- "with a COVID-19 diagnosis"
name_disease_flu <- "influenza"
name_disease <- "with a COVID-19 diagnosis"
# name: name of the strata as in shiny app: i.e. "Prevalent obesity" i.e. name <- "Prevalent malignant neoplasm"
# short : name that will be printed on the columns i.e "Obese" short<-"Cancer"
#name<-"Prevalent obesity" 
# short<-"Obese"
name <- "Prevalent malignant neoplasm"
short<-"Cancer"
name<- "Prevalent Type 2 Diabetes Mellitus"
short<-"T2 Diabetes"
name <- "Age < 18"
name<- "Sex = Female"
short <- name
name<- "Sex = Male"
short <- name
name <- "Prevalent hypertension"
short<-"Hypertension"
name<-"Prevalent obesity" 
short<- "Obese"
name
# file with database with no prior observations available
# set name of database you want to consider

#
databases<-c("SIDIAP","SIDIAP-H", "CPRD","HM-Hospitales", "HIRA","PREMIER", "CUIMC", "STARR-OMOP", "IPCI","IQVIA-OpenClaims","optum","VA-OMOP", "Health-verity")
databases<-c("SIDIAP", "STARR-OMOP","CPRD","CUIMC","IQVIA-OpenClaims","SIDIAP-H", "VA-OMOP")

databases<-(list.files(path="//epofs/apistillo/CHARYBDIS/data2")%>%
              str_sub(start=9, end=-5))
[-c(1,4,9,16,17)]
databases_noprior<-c("Premier","HM-Hospitales" )

databases<-c("HIRA","IQVIA-LPDFrance","IQVIA-OpenClaims","SIDIAP","UCHealth-OMOP",
             "CUIMC","HEALTHVERITY","IQVIA-DAGermany","OptumEhr","Premier","STARR-OMOP")


databases<-c("HIRA","IQVIA-LPDFrance","IQVIA-OpenClaims","SIDIAP","UCHealth-OMOP",
             "OptumEhr")
#pediatry
databases<-c("SIDIAP", "IQVIA-LPDFrance", "CU-AMC-HDC", "IQVIA-OpenClaims", "OPTUM-EHR", "HIRA",
             "CUIMC","Premier","IQVIA-DAGermany","HEALTHVERITY","STARR-OMOP")
databases_noprior<-c("CUIMC","Premier","IQVIA-DAGermany","HEALTHVERITY","STARR-OMOP")
databases_noprior<-databases
  c("CUIMC","Premier","IQVIA-DAGermany","HEALTHVERITY","STARR-OMOP")
databases<-"OptumEhr"

databases<-databases[-c(3,6,7, 8,11,12,14,18,20)]
databases_noprior<-""
databases_noprior<-c("Premier","HM-Hospitales", "Nanfang" )

databases<-c("HIRA",	"DCMC"	,"Nanfang",	"HEALTHVERITY",	"Premier","OptumEhr",	"STARR-OMOP"	,
             "TRDW",	"VA-OMOP"	,"IQVIA-OpenClaims",	"CUIMC",	"UCHealth-OMOP"
             ,"UWM-CRD",	"HM-Hospitales",	"SIDIAP",	"SIDIAP-H",	"IPCI",	"CPRD",
             "IQVIA-LPDFrance",	"IQVIA-DAGermany",	"LPDItaly")

databases_noprior<-""
databases<-"SIDIAP-H"
databases_noprior<-databases

#cancer
databases <- c("SIDIAP","CU-AMC-HDC","CUIMC","HEALTHVERITY","IQVIA-OpenClaims","OPTUM-EHR" ,"STARR-OMOP","VA-OMOP")

# databases_noprior<-databases


#functions

fun.createtables <- function(cohort,covariate_value,covariate,
                             id_name, id_noname, hospitalization,
                             selected_cohort, time, conditions){
  
  total_name <- cohort %>% filter(cohort_id %in% selected_cohort & cohort_id %in% {{id_name}} & cohort_id %in% hospitalization & cohort_id %in% time)  %>%
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
 
  merged <- tibble(covariate_id=numeric(), covariate_name=character())
  merge_edad <- tibble(covariate_name = character())
  base_noprior<-vector()
  
  for (i in 1:length(databases)){
    path_in<-paste0(path, databases[i])
    writeLines(paste("Processing", databases[i]))
    cohort<-read.csv(unz(paste0(path_in,".zip"), "cohort.csv"), stringsAsFactors = FALSE)  %>%
      select(cohort_name,cohort_full_name,cohort_id,cohort_type)
    cohort_count<-read.csv(unz(paste0(path_in,".zip"), "cohort_count.csv"), stringsAsFactors = FALSE)
    
    cohort<- merge(cohort, cohort_count, by="cohort_id" )
    covariate<-read.csv(unz(paste0(path_in,".zip"), "covariate.csv"), stringsAsFactors = FALSE) 
    covariate_value<-read.csv(unz(paste0(path_in,".zip"), "covariate_value.csv"), stringsAsFactors = FALSE) 
    
    id_name <- (cohort %>% filter(str_detect(cohort_name, name)) %>% filter(str_detect(cohort_name,"with")&!str_detect(cohort_name,"without")))$cohort_id
    id_noname <- (cohort %>% filter(str_detect(cohort_name, name)) %>% filter(str_detect(cohort_name,"without")))$cohort_id
    
    id_cohort <- (cohort %>% filter(str_detect(cohort_name, regex(name_disease, ignore_case=T))&!str_detect(cohort_name, "tested")))$cohort_id
    id_cohort_flu <- (cohort %>% filter(str_detect(cohort_name, regex(name_disease_flu, ignore_case=T))&!str_detect(cohort_name, "tested")))$cohort_id
    
    
    id_Hospitalized<- (cohort %>% filter(str_detect(cohort_name, "hospitalized") & !str_detect(cohort_name,"requiring intensive services")))$cohort_id
    id_Diagnosed <- (cohort %>% filter(!str_detect(cohort_name, "hospitalized")))$cohort_id
    id_ICU<-(cohort %>% filter(str_detect(cohort_name, "requiring intensive services")))$cohort_id
    
    id_prior<- (cohort %>% filter(str_detect(cohort_name, "with at least 365d prior observation")))$cohort_id
    id_noprior <-  (cohort %>% filter(str_detect(cohort_name, "with no required prior observation")))$cohort_id
    
    # Conditions
    
    id_conditions <- unique(covariate %>% filter(tolower(covariate_name) %in% trimws(tolower(conditions$covariate_name)) & !covariate_id %in% c(200031,210031,220031,230031), ignore.case=TRUE))$covariate_id
    #I took out > 100 years old
    
   
    hospit <- c("Diagnosed", "Hospitalized", "ICU")
    
    for (j in 1:3){
      nome <- paste(databases[i], short, hospit[j],"covid", sep="_")
      nonome<-paste(databases[i], "No", short, hospit[j],"covid", sep="_")
      #nonome<-paste("All", databases[i], hospit[j], sep="_")
      sdnome <- paste0(nome, "_sd")
      sdnonome <- paste0(nonome, "_sd")
      nome_flu <- paste(databases[i], short, hospit[j], "influenza",sep="_")
      nonome_flu<-paste(databases[i], "No",short, hospit[j], "influenza",sep="_")
      sdnome_flu <- paste0(nome_flu, "_sd")
      sdnonome_flu <- paste0(nonome_flu, "_sd")
      smd <- paste0(nome, "SMD")
      writeLines(paste("Processing", hospit[j]))
      id_hospit<-get(paste0("id_",hospit[j]))
      if (databases[i]%in%databases_noprior){id_prior<-id_noprior; base_noprior<-c(base_noprior, databases[i])}
      id_check<-intersect(id_cohort, id_hospit)%>%intersect(id_prior)%>%intersect(id_name)
      #id_noname<-(intersect(id_cohort, id_hospit)%>%intersect(id_prior))[1]
      count<-0
      if (!length(id_check)==0){
      total <- fun.createtables(cohort,covariate_value,covariate,id_name, id_noname, id_hospit, id_cohort, id_prior,id_conditions)
      count<-count+1
      }
      id_check_flu<-intersect(id_cohort_flu, id_hospit)%>%intersect(id_prior)%>%intersect(id_name)
      if (!length(id_check_flu)==0){
      total_flu <- fun.createtables(cohort,covariate_value,covariate,id_name, id_noname, id_hospit, id_cohort_flu, id_prior,id_conditions)
      count<-count+1
      }
      if(!length(id_check)==0){
      total_pretty <- unique(total%>% mutate(!!smd:=(abs(mean.x)-abs(mean.y))/(sqrt(sd.x^2+sd.y^2)))%>% 
                               select(covariate_name,!!nome:=mean.x,!!sdnome:=sd.x, !!nonome:=mean.y,!!sdnonome:=sd.y,smd, covariate_id))
      count<-count+1
       }
      if(!length(id_check_flu)==0){
      total_pretty_flu <- unique(total_flu%>% 
                               select(covariate_name,!!nome_flu:=mean.x,!!sdnome_flu:=sd.x, !!nonome_flu:=mean.y,!!sdnonome_flu:=sd.y, covariate_id))
      
      if(!length(id_check)==0){
        total_pretty<-full_join(total_pretty, total_pretty_flu, by = c("covariate_id", "covariate_name"))
        count<-count+1
        }
      else{total_pretty<-total_pretty_flu; count<-count+1}
      }
      
      if(count==0){next}
      
      merged <-  unique(full_join(unique(merged), total_pretty, by= c("covariate_id", "covariate_name")))
      
    }#end for hospitalization
  }#end for
  
  
  merged_pretty<-merged%>%select(starts_with("covariate_name"),contains("Diagnosed"), contains("Hospitalized"), contains("ICU"),ends_with("sd"), covariate_id)%>%
    left_join(conditions, by = "covariate_name")%>%arrange(id)
  
}

#------
table<-result.fun()


save(table,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "pediatry.jan21.noprior.Rda"))

save(table,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "result_cancer.27.01.Rda"))

merged_pretty<-merged%>%#select(starts_with("covariate_name"),ends_with("primary"), ends_with("hospitalized"), ends_with("uci"), covariate_id, id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD")) %>%arrange(id)

path_out="//epofs/apistillo/CHARYBDIS/Tablas/"
#cargar environment con bases de datos
write.table(merged_pretty,paste0(path_out,"obesity_new_23-07.csv"), sep = ";", row.names = F, dec = ",", na="")


save(table,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "general_female_noprior.Rda"))
