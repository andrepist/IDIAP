
library(dplyr) #para el join
library(tidyr)
library(stringr) #para buscar match en st
path_out="//epofs/apistillo/CHARYBDIS/Conditions/"
path="//epofs/apistillo/CHARYBDIS/data2/Results_"

databases<-c("CUIMC","HEALTHVERITY","HIRA", "IQVIA-OpenClaims","OptumEhr","SIDIAP", "CU-AMC-HDC", "UWM-CRD",  "VA-OMOP",
             "IQVIA-LPDFrance","STARR-OMOP","IQVIA-DAGermany","IPCI","TRDW","CPRD" ,"LPDItaly")

databases<-(list.files(path="//epofs/apistillo/CHARYBDIS/data2")%>%
              str_sub(start=9, end=-5))
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
conditions<-read.csv(paste0(path_out,"counts_diabetes.csv"), header = F)



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
  base<-databases[i]
  
  tabla <- cohort %>% filter(cohort_id%in% id_conditions_prior | cohort_id%in%id_conditions_noprior )%>%
    select(cohort_name,cohort_subjects,-database_id)%>%rename(N=cohort_subjects)%>%
    mutate(match=cohort_name)
  
  
  tabla_obese <- cohort %>% filter(cohort_id%in% id_conditions_obese)%>%
    separate(cohort_name,into = c("match",NA), sep = paste0(" with","\\s+",name), remove=FALSE)%>%
    select(cohort_subjects, match)%>%rename(n:=cohort_subjects)
  
  
  
  tabla_intermediate<-tabla_obese%>%full_join(tabla,by="match")%>%mutate(strata=short)
  
  tabla_noobese <- cohort %>% filter(cohort_id%in% id_conditions_noobese) %>%
    separate(cohort_name,into = c("match",NA), sep = " without", remove = FALSE)%>%
    select(cohort_subjects,match)%>%rename(n:=cohort_subjects)
  
  tabla_intermediate2<-tabla_noobese%>%full_join(tabla,by="match")%>%mutate(strata=noshort)
  
  
  tabla_final<- rbind(tabla_intermediate, tabla_intermediate2)%>%select(-match)%>%mutate(base=databases[i])
  
  row<- rbind(row, tabla_final)
  
  
  
}

ci<-row%>%mutate(mean=(n/N))%>%
  mutate(ic.low=(mean-1.96*sqrt((mean)*(1-mean)/N)), ic.high=(mean+1.96*sqrt((mean)*(1-mean)/N)))%>%
  mutate(mean=round(mean*100, 1), ic.low=round(ic.low*100, 1), ic.high=round(ic.high*100, 1))%>%
  mutate(mean=format(mean, nsmall=1), ic.low=format(ic.low, nsmall=1), ic.high=format(ic.high,nsmall=1)) %>% 
  mutate(mean = paste0(mean, " (", ic.low, "-", ic.high, ")"))%>%select(-ic.low,-ic.high, -N)
  

tabla<-ci%>%filter(strata=="T2 Diabetes")%>%select(-strata)%>%
  pivot_wider(names_from = "base", values_from="mean")

tabla<-ci%>%filter(strata=="Hypertension")%>%select(-strata)%>%
  filter(!str_detect(cohort_name, "Persons tested positive for SARS-CoV-2|Persons hospitalized with a SARS-CoV-2 positive test|nfluenza")) %>% 
  pivot_wider(names_from = "cohort_name", values_from=c("mean","n")) %>% 
  arrange(desc(`n_Persons with a COVID-19 diagnosis or a SARS-CoV-2 positive test with at least 365d prior observation`)) %>% 
  mutate_at(4:5, ~as.character(prettyNum(.,big.mark = ",")))


tabla <- tabla %>% filter(base%in%databases_primary)

path_out="//epofs/apistillo/CHARYBDIS/Hypertension/"
write.table(tabla,paste0(path_out,"prevalence.hiper.csv"), sep = "\t", row.names = F, dec = ",", na="-")



results.folder <- "//epofs/apistillo/CHARYBDIS/Diabetes/"
write.table(tabla,paste0(results.folder,"proportions.csv"),  sep = ";", row.names = F, dec = ",", na="-")


to.join<-row%>%filter(!cohort_name%in%c("Persons tested positive for SARS-CoV-2 with at least 365d prior observation","Persons hospitalized with a SARS-CoV-2 positive test with at least 365d prior observation"))%>%
  mutate(disease=ifelse(str_detect(tolower(cohort_name), "influenza"), "influenza", "covid"),
         hospit=ifelse(str_detect(tolower(cohort_name), "hospitalized"), "Hospitalized", "Diagnosed"),
         strata=recode(strata,"No T2 Diabetes"="No_T2_Diabetes")
         #strata=recode(strata,"No T2 Diabetes"="Without T2 Diabetes", "T2 Diabetes"="With T2 Diabetes")
         )%>%
  select(-cohort_name, -N)%>%rename(diab=strata)
data<-left_join(plot.data, to.join)

to.join<-row%>%filter(!cohort_name%in%c("Persons tested positive for SARS-CoV-2 with at least 365d prior observation","Persons hospitalized with a SARS-CoV-2 positive test with at least 365d prior observation"))%>%
  mutate(disease=ifelse(str_detect(tolower(cohort_name), "influenza"), "influenza", "covid"),
         hospit=ifelse(str_detect(tolower(cohort_name), "hospitalized"), "Hospitalized", "Diagnosed"),
         strata=ifelse(strata=="No Obese", "Non Obese", strata))%>%
  select(-cohort_name, -N)%>%rename(obese=strata)
data<-left_join(plot.data, to.join)


load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "hypertension.data.Rda"))
to.join<-row%>%filter(!cohort_name%in%c("Persons tested positive for SARS-CoV-2 with at least 365d prior observation","Persons hospitalized with a SARS-CoV-2 positive test with at least 365d prior observation"))%>%
  mutate(disease=ifelse(str_detect(tolower(cohort_name), "influenza"), "influenza", "covid"),
         hospit=ifelse(str_detect(tolower(cohort_name), "hospitalized"), "Hospitalized", "Diagnosed"),
         strata=recode(strata,"Hypertension"="With Hypertension","No Hypertension"="Without Hypertension"))%>%
  rename(diab=strata)
plot.data<-inner_join(to.join, plot.data,by =c("diab", "base", "hospit", "disease"))
save(plot.data,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "hypertension.data.Rda"))


# hipertension -----
to.join<-row%>%filter(!cohort_name%in%c("Persons tested positive for SARS-CoV-2 with at least 365d prior observation","Persons hospitalized with a SARS-CoV-2 positive test with at least 365d prior observation"))%>%
  mutate(disease=ifelse(str_detect(tolower(cohort_name), "influenza"), "influenza", "covid"),
         hospit=ifelse(str_detect(tolower(cohort_name), "hospitalized"), "Hospitalized", "Diagnosed"))%>%
  rename(hyper=strata)
data.clean<-inner_join(to.join, data.clean,by =c("hyper", "base", "hospit", "disease"))
