
library(dplyr) #para el join
library(tidyr)
library(stringr) #para buscar match en stringa

path_out="//epofs/apistillo/CHARYBDIS/Conditions/"
path="//epofs/apistillo/CHARYBDIS/data2/Results_"

databases<-c("SIDIAP", "CPRD", "STARR-OMOP", "CUIMC","IQVIA-OpenClaims", "VA-OMOP" )
databases<-(list.files(path="//epofs/apistillo/CHARYBDIS/data2")%>%
  str_sub(start=9, end=-5))
[-c(1,16,17)]
[-c(1,4,9,16,17)]
databases<-"OptumEhr"

#%>%  str_replace_all("_","-")
  

name <- "Prevalent malignant neoplasm excluding non-melanoma skin cancer"
short = "Cancer"
name <- "Prevalent obesity"
short <- "Obese"
name<-"Prevalent Type 2 Diabetes Mellitus"
short <- "T2 Diabetes"
name<-"Age < 18"
short <-"Age < 18"
name<-"Prevalent hypertension"
short<-"Hypertension"
noshort <- paste0("No ", short)
conditions<-read.csv(paste0(path_out,"counts_conditions.csv"), header = F)
#%>%mutate(grupo=c("Diagnosed","Hospitalized", "Uci"))


condition<-conditions$V1
prior <- "with at least 365d prior observation"
noprior <- "with no required prior observation"

row <- tibble(cohort_name=character())
col<-vector()
mali<-tibble(Malignant=character())
dataset<-tibble(cohort=character(), N = numeric(), database=character())
for (i in 1:length(databases)){
  path_in<-paste0(path, databases[i])
  cohort<-read.csv(unz(paste0(path_in,".zip"), "cohort.csv"), stringsAsFactors = FALSE) %>%
    select(cohort_id, cohort_name) 
  cohort_counts<-read.csv(unz(paste0(path_in,".zip"), "cohort_count.csv"), stringsAsFactors = FALSE)%>% 
    select(cohort_id, cohort_subjects, database_id) 
  
  cohort <- merge(cohort, cohort_counts, by="cohort_id" )
  
  
  id_conditions_prior<-(cohort %>%  filter(grepl(paste0("^",conditions$V1,"$",collapse="|"),cohort_name) & str_detect(cohort_name,"with at least")))$cohort_id
  id_conditions_noprior<-(cohort %>%  filter(grepl(paste0("^",conditions$V1,"$",collapse="|"),cohort_name) & str_detect(cohort_name," with no required")))$cohort_id
  id_conditions_obese<-(cohort %>%  filter(grepl(paste(conditions$V1,collapse="|"),cohort_name) & str_detect(cohort_name,name)& str_detect(cohort_name, "with")&!str_detect(cohort_name,"without")))$cohort_id
  id_conditions_noobese<-(cohort %>%  filter(grepl(paste(conditions$V1, collapse="|"),cohort_name)& str_detect(cohort_name,name) & str_detect(cohort_name, "without")))$cohort_id
  
  nombre<-paste0(databases[i]," Total") 
  nombre_yes <- paste0(databases[i]," ",short) 
  nombre_no <- paste0(databases[i]," ", noshort) 
  tabla <- cohort %>% filter(cohort_id%in% id_conditions_prior | cohort_id%in%id_conditions_noprior )%>%
    select(cohort_name,cohort_subjects,-database_id)%>%rename(!!nombre:=cohort_subjects)%>%
    mutate(match=cohort_name)
  
  all<-cohort %>% filter(cohort_id%in% id_conditions_prior | cohort_id%in%id_conditions_noprior )
  dataset<-rbind(dataset, all%>%
    mutate(strata="All")%>%
      mutate(database=databases[i]))
  
  tabla_obese <- cohort %>% filter(cohort_id%in% id_conditions_obese)%>%
    separate(cohort_name,into = c("match",NA), sep = paste0(" with","\\s+",name), remove=FALSE)%>%
    select(cohort_subjects, match)%>%rename(!!nombre_yes:=cohort_subjects)
  
  strata<-cohort %>% filter(cohort_id%in% id_conditions_obese)
  dataset<-rbind(dataset, strata%>%
    mutate(strata = short)%>%
      mutate(database=databases[i]))
  
  tabla_intermediate<-tabla_obese%>%full_join(tabla,by="match")
  
  tabla_noobese <- cohort %>% filter(cohort_id%in% id_conditions_noobese) %>%
    separate(cohort_name,into = c("match",NA), sep = " without", remove = FALSE)%>%
    select(cohort_subjects,match)%>%rename(!!nombre_no:=cohort_subjects)
  
  nostrata<-cohort %>% filter(cohort_id%in% id_conditions_noobese)
  dataset<-rbind(dataset, nostrata%>%
                   mutate(strata=noshort)%>%
                   mutate(database=databases[i]))
  
  tabla_final<- tabla_noobese%>%full_join(tabla_intermediate,by="match")%>%
    select(4,3,1,5)
  
  #if (databases[i]=="PREMIER"){id_conditions_prior<-id_conditions_noprior}
  
  # tabla2<- cohort %>% filter(cohort_id%in% id_conditions_prior|cohort_id%in%id_conditions_noprior )%>% select(-cohort_id)%>%rename(Total:=cohort_subjects)%>%mutate(match=cohort_name)
  # tabla_2 <- cohort %>% filter(cohort_id%in% id_conditions_obese)%>%separate(cohort_name,into = c("match",NA), sep = paste0(" with","\\s+",name), remove=FALSE)%>%
  #   select(cohort_subjects, match)%>%rename(!!short:=cohort_subjects)
  # tabla_intermediate2<-tabla_2%>%full_join(tabla2,by="match")
  # tabla_noobese2 <- cohort %>% filter(cohort_id%in% id_conditions_noobese) %>% separate(cohort_name,into = c("match",NA), sep = " without", remove = FALSE)%>%
  #   select(cohort_subjects,match)%>%rename(!!noshort:=cohort_subjects)
  # tabla_final2<- tabla_noobese2%>%full_join(tabla_intermediate2,by="match")%>%
  #   select(4,3,1,5,6) 
  
  #col<-rbind(col, tabla_final2)
 
  row<- full_join(row, tabla_final,by="cohort_name")
  
  
  #mali <- full_join(mali,unique(covariate %>% filter(str_detect(covariate_name,"malignant"))%>%separate(covariate_name, into=c(NA, "Malignant"), sep = ":")%>%select(Malignant))%>%mutate(database=databases[i]), by = "Malignant")

}

row<-row

row1<-row%>%select(-contains("No Age"))%>%
  filter(!str_detect(cohort_name,"intensive services"))

dataset<-rbind(dataset%>%
  filter(!str_detect(cohort_name, "tested positive for SARS-CoV-2"))%>%
  filter(!str_detect(cohort_name, "hospitalized with a SARS-CoV-2 positive test"))%>%
  mutate(illness=ifelse(str_detect(tolower(cohort_name), "influenza"), "Influenza", "Covid"))%>%
  mutate(state=ifelse(str_detect(tolower(cohort_name), "requiring intensive services"), "ICU", 
                      ifelse(str_detect(tolower(cohort_name), "hospitalized"), "Hospitalized", "Diagnosed")))%>%
  mutate(prior=ifelse(str_detect(tolower(cohort_name), "365d"), "with prior", "without prior") )%>%
  select(-cohort_id, -cohort_name, -database_id)%>%
  rename(N=cohort_subjects)%>%
    filter(prior=="with prior"),
  dataset%>%
    filter(!str_detect(cohort_name, "tested positive for SARS-CoV-2"))%>%
    filter(!str_detect(cohort_name, "hospitalized with a SARS-CoV-2 positive test"))%>%
    mutate(illness=ifelse(str_detect(tolower(cohort_name), "influenza"), "Influenza", "Covid"))%>%
    mutate(state=ifelse(str_detect(tolower(cohort_name), "requiring intensive services"), "ICU", 
                        ifelse(str_detect(tolower(cohort_name), "hospitalized"), "Hospitalized", "Diagnosed")))%>%
    mutate(prior=ifelse(str_detect(tolower(cohort_name), "365d"), "with prior", "without prior") )%>%
    select(-cohort_id, -cohort_name, -database_id)%>%
    rename(N=cohort_subjects)%>%
    filter(prior=="without prior"))

#% pediatry
test <- dataset %>% filter(illness=="Covid", !state=="ICU") %>% pivot_wider(names_from = c(strata), values_from = c(N)) %>% 
  mutate(porc=`Age < 18`/`All`*100) %>% 
  pivot_wider(names_from = c( state, prior), values_from = c(5,6,7))
#
test<-pivot_wider(dataset, names_from = c(illness, state, strata), values_from = c(N))%>%
  select(1,contains(c("covid","influenza")))
test<-test%>%
  mutate_at(2:length(test),funs(as.character))%>%
  mutate_at(2:length(test),funs(str_replace(., "-", "<")))
test<-test%>%select(-contains("all"))
  

results.folder <- "//epofs/apistillo/CHARYBDIS/Tablas/"
write.table(row,paste0(results.folder,"count_cancer_7_10.csv"),  sep = ";", row.names = F, dec = ",", na="")
write.table(test,paste0(results.folder,"count.hypertension.13.01.21.csv"),  sep = ";", row.names = F, dec = ",", na="")

test1<-pivot_longer(row, cols = 2:ncol(row))
test<-test%>%pivot_wider(values_from = value, names_from=cohort_name)

test<-pivot_wider(na.omit(col) , names_from = cohort_name, values_from = c(short, noshort, Total),names_glue = "{cohort_name}_{.value}" )
%>%rename(Diagnosed_prior_Obese:=2)
write.table(row,paste0(path_out,"tabla_count_obe_new.csv"), sep = ";", row.names = F, dec = ",", na="-")



write.xlsx2(as.data.frame(row),paste0(path_out,"count_",short,".xlsx"), row.names = FALSE)
write.xlsx(as.data.frame(mali),paste0(path_out,"Malignant.xlsx"), row.names = FALSE)

