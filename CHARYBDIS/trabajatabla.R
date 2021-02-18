path_out="//epofs/apistillo/CHARYBDIS/Tablas/"
results.folder <- "//epofs/apistillo/CHARYBDIS/SavedData/"
# Pediatry -----
load(paste0(results.folder, "pediatry.jan21.noprior.Rda"), envir = globalenv())
library(kableExtra)
library(tidyverse)

merged<-table


#other way <- ordena por condiciones, queda pendiente: juntar nombres cohortes
merged<-merged%>%
  mutate(group=ifelse(str_detect(covariate_name, "Cohort during day -365 through -1 days|condition_era group during day -365 through -1 days|death|Hospitalization episodes|intensive services during hospitalization"),"Condition",
                            ifelse(str_detect(covariate_name, "drug_era group during day -30 through -1 days") ,"Medication",
                                              ifelse(str_detect(covariate_name, "Age|gender|age group") ,"Demographic", 
                                                     ifelse(str_detect(covariate_name, "Cohort during day 0 through 30 days"), "Events","Not Applicable")))))

rem<-c("condition_era group during day -365 through -1 days relative to index: Pneumonia",
       "Cohort during day -365 through -1 days overlap the index: Pneumonia episodes",
       #"0 through 0",
       #"-365",
       "Cohort during day 0 through 0 days start the index: Hospitalization episodes",
       "Cohort during day -365 through -1 days overlap the index: Hospitalization episodes",
       "Cohort during day -365 through -1 days overlap the index: Flu-like symptom episodes")
merged_pretty<-merged%>%
  #filter(!group=="Not Applicable") %>% 
  filter(!str_detect(covariate_name, paste0(rem, collapse="|")))%>%
 # select(covariate_name, contains("SIDIAP"),contains("LPD"),
 #                              contains("CU-AMC"),
 #                              contains("HIRA"),contains("OpenClaim"),-contains(c("HIRA_Age < 18_Diagnosed",
 #                                                                                 "SIDIAP_Age < 18_Hospitalized",
 #                                                                                 "CU-AMC-HDC_Age < 18_Hospitalized")),contains("Optum"))%>%
   select(covariate_name,contains("Diagnosed"), contains("Hospitalized"))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ": ", remove = FALSE)%>%
  select(-contains("SMD"), -contains("No_"), -ends_with("sd")) %>% 
  select(-contains("influenza"))
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider



keep<- read.csv(paste0(path_cond,"keep_pediatry.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)


merged_pretty[merged_pretty$covariate_name=="gender = FEMALE",]$Type<-"Female"
merged_pretty[merged_pretty$covariate_name=="gender = MALE",]$Type<-"Male"
merged_pretty<-merged_pretty%>%mutate(Type=recode(trimws(Type), 
                                                  'Prevalent heart disease'='Heart disease',
                                                  'Prevalent obesity'='Obesity',
                                                  'Prevalent hypertension' = 'Hypertension',
                                                  'Prevalent malignant neoplasm excluding non-melanoma skin cancer'='Malignant neoplasm excluding non-melanoma skin cancer',
                                                  'Prevalent Asthma without COPD'='Asthma',
                                                  'Hospitalization episodes'= 'Hospitalisation episodes',
                                                  'intensive services during hospitalization'='Intensive services',
                                                  'Sepsis during hospitalization'='Sepsis',
                                                  'Acute Respiratory Distress syndrome (ARDS) during hospitalization'='Acute respiratory distress syndrome (ARDS)',
                                                  'Cardiac arrhythmia during hospitalization'= 'Cardiac arrhythmia',
                                                  'Bleeding during hospitalization'='Bleeding',
                                                  'death'='Death',
                                                  'Pneumonia during hospitalization'='Pneumonia'))


pediatry<-merged_pretty %>% left_join(keep %>% rename(Type=covariate_name)) %>% arrange(id) %>% 
  mutate_at(3:length(merged_pretty),funs(ifelse(.<0, NA, .)))%>%
#[match(tolower(keep$covariate_name),tolower(merged_pretty$Type)),]%>%
  mutate_at(3:length(merged_pretty),funs(.*100))%>%
  mutate_at(3:length(merged_pretty),funs(prettyNum(.,nsmall=1)))%>%
  mutate_at(3:length(merged_pretty),funs(str_replace(., "-", "<")))

pediatry<-pediatry


# kable(pediatry%>%select(-1),
#   col.names = c("", "SIDIAP",
#                 "LPD","UCHealth-OMOP", "IQVIA-OpenClaims","Optum", "IQVIA-OpenClaims", "Optum", "HIRA"))%>%
#   add_header_above(c(" " = 1, "Diagnosed" = 5,
#                      "Hospitalized" = 3))%>%
#   pack_rows("Age category", 1, 4)%>%
#   pack_rows("Gender", 5, 6)%>%
#   pack_rows("Comorbidities", 7, 20)%>%
#   pack_rows("30-days outcomes", 21, 25)%>%
#   pack_rows("30-days outcomes during hospitalisation", 26, 30)%>%
#   pack_rows("30-days symptomes", 31, 36)%>%
#   kable_styling(bootstrap_options = c("striped", "bordered")) 

write.table(pediatry,paste0(path_out,"pediatry_noprior_new.csv"), sep = ";", row.names = F, dec = ",", na="-")


merged_pretty<-merged%>%select(group,starts_with("covariate_name"),ends_with("Diagnosed"), ends_with("Hospitalized"), covariate_id, id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD")) %>%arrange(id)%>%
  filter(!group=="Not Applicable")%>%
  mutate(Type= recode(trimws(Type), 'Prevalent malignant neoplasm excluding non-melanoma skin cancer'='History of cancer','Prevalent chronic obstructive pulmonary disease (COPD) without asthma'='COPD',
                      'Incident depression with no prior treatment and no mania/psychoses'='Depression','Prevalent pre-existing condition of COVID risk factor'='Pre-existing condition of COVID risk factor',
                      'Prevalent Human immunodeficiency virus infection'='Human immunodeficiency virus infection',
                      'Prevalent hypertension'='Hypertension',
                      'Prevalent heart disease'='Heart disease',
                      'Prevalent Type 2 Diabetes Mellitus'='Type 2 Diabetes Mellitus',
                      'Prevalent Autoimmune condition'='Autoimmune condition',
                      'Prevalent Asthma without COPD'='Asthma',
                      'Prevalent chronic kidney disease broad'='Chronich kidney disease',
                      'Prevalent Dementia'='Dementia',
                      'death'='Death',
                      'Pregnant women'='Pregnancy',
                      'Venous thromboembolic (pulmonary embolism and deep vein thrombosis) events'='Venous thromboembolic',
                      'Prevalent Asthma or Chronic obstructive pulmonary disease (COPD)'='Asthma or COPD',
                      'Human immunodeficiency virus infection'='HIV'
  ))

ord_cat<-c("Respiratory", "Metabolic", "Cardiovascular", "Mental health and neurologic", "Others", "Outcomes")

comorbidities<-merged_pretty%>%filter(group=="Condition")%>%
  mutate(category = ifelse(str_detect(Type, "Apnea|Asthma|COPD|Idiopatic"), "Respiratory", 
                           ifelse(str_detect(Type, "obesity|Hyperlipidemia"), "Metabolic",
                                  ifelse(str_detect(Type, "Atrial|Cerebrovascular|Heart|Hypertension|Ischemic|Peripheral vascular"), "Cardiovascular",
                                         ifelse(str_detect(Type, "Anxiety|Dementia|Depression"), "Mental health and neurologic", 
                                                ifelse(str_detect(Type, "Death|intensive services during hospitalization|Hospitalization episodes"), "Outcomes","Others"))))))%>%
  mutate(category = factor(category, levels=ord_cat))%>%
  group_by(category)%>% 
  arrange(category, Type)


comorbidities.diagnosed<-comorbidities%>%select(Type,category, contains("Diagnosed"))
comorbidities.diagnosed<-comorbidities.diagnosed%>%
  mutate_at(3:length(comorbidities.diagnosed),funs(.*100))%>%
  mutate_at(3:length(comorbidities.diagnosed),funs(as.character))%>%
  mutate_at(3:length(comorbidities.diagnosed),funs(str_replace(., "-", "<")))

comorbidities.hospitalized<-comorbidities%>%select(Type, category,contains("Hospitalized"))
comorbidities.hospitalized<-comorbidities.hospitalized%>%
  mutate_at(3:length(comorbidities.hospitalized),funs(.*100))%>%
  mutate_at(3:length(comorbidities.hospitalized),funs(as.character))%>%
  mutate_at(3:length(comorbidities.hospitalized),funs(str_replace(., "-", "<")))

comorbidities.ICU<-comorbidities%>%select(Type, category,contains("ICU"))
comorbidities.ICU<-comorbidities.ICU%>%
  mutate_at(3:length(comorbidities.ICU),funs(.*100))%>%
  mutate_at(3:length(comorbidities.ICU),funs(as.character))%>%
  mutate_at(3:length(comorbidities.ICU),funs(str_replace(., "-", "<")))

write.table(comorbidities.diagnosed,paste0(path_out,"comorbidities.diagnosed.csv"), sep = ";", row.names = F, dec = ",", na="")


demographic<-merged_pretty%>%filter(group=="Demographic")%>%
  select(-Type,-contains("UCH"))
demographic<-demographic%>%
  mutate_at(3:length(demographic),funs(.*100))%>%
  mutate_at(3:length(demographic),funs(as.character))%>%
  mutate_at(3:length(demographic),funs(str_replace(., "-", "<")))%>%
  unique()



write.table(demographic,paste0(path_out,"demographic_flu.csv"), sep = ";", row.names = F, dec = ",", na="")



write.table(c_diag,paste0(path_out,"try.csv"), sep = ";", row.names = F, dec = ",", na="")

# all -------

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "result_all_covid_prior_13_10.Rda"), envir = globalenv())

merged<-table

merged_pretty<-merged%>%select(starts_with("covariate_name"), starts_with("All"),covariate_id)%>%
  mutate(covariate_name=ifelse(covariate_id==200031, "age group: 100-104", 
                               ifelse(covariate_id==210031, "age group: 105-109",
                                      ifelse(covariate_id==220031, "age group: 110-114",covariate_name))))%>%
  filter(!covariate_id%in%c(230031,240031))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD"), -covariate_id)

merged_pretty<-merged_pretty%>%
  mutate_at(3:length(merged_pretty),funs(.*100))%>%
mutate_at(3:length(merged_pretty),funs(as.character))%>%
  mutate_at(3:length(merged_pretty),funs(str_replace(., "-", "<")))

conditions_tabla2<-conditions
merged_pretty<-merged_pretty[match(tolower(conditions_tabla2$covariate_name),tolower(merged_pretty$covariate_name)),]
#cargar environment con bases de datos
path_out="//epofs/apistillo/CHARYBDIS/General/"
write.table(merged_pretty,paste0(path_out,"covid_prior.csv"), sep = ";", row.names = F, dec = ",", na="")

test<-merged_pretty%>%select(-ends_with("sd"))%>%pivot_longer(cols=starts_with("All"), names_to=c("all","base", "hospit"), names_sep="_")%>%
  mutate(disease="COVID-19")%>%rename(mean=value)

test2<-merged_pretty%>%select(-ends_with("sd"))%>%pivot_longer(cols=starts_with("All"), names_to=c("all","base", "hospit"), names_sep="_")%>%
  mutate(disease="Influenza")%>%rename(mean=value)

final<-rbind(test, test2)
save(final,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "data_all_8_10.Rda"))



#save(final,file="data_1507.Rda")
#dir.create(tempdir())

# cancer -----

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "cancer.allfeature.28.01.optum.Rda"), envir = globalenv())
merged<-table
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions2<- read.csv(paste0(path_cond,"cancer_orden.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
merged_pretty<-merged%>%select(-contains("No_Cancer"))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-contains(c("SMD", "_sd")))%>%
  select( -covariate_id)




conditions<- read.csv(paste0(path_cond,"conditions_cancer_last.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
merged_pretty<-merged_pretty[match(tolower(conditions2$covariate_name),tolower(merged_pretty$covariate_name)),]
  
merged_pretty <- merged_pretty %>% select(-contains("influenza")) 
merged_pretty<-merged_pretty%>%
  mutate_at(3:length(merged_pretty),funs(prettyNum(.*100, nsmall=1)))%>%
  #mutate_at(3:length(merged_pretty),funs(as.character))%>%
  mutate_at(3:length(merged_pretty),funs(str_replace(., "-", "<"))) %>% 
  mutate_at(3:length(merged_pretty),funs(str_replace(., "NA", NA_character_)))

path_out="//epofs/apistillo/CHARYBDIS/Cancer/"
write.table(merged_pretty,paste0(path_out,"cancer.covid.2021.csv"), sep = ";", row.names = F, dec = ",", na="-")

## HYPERTENSION -----
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "hypertension_14_10.Rda"), envir = globalenv())
library(dplyr)
library(tidyr)
merged<-table

merged_pretty<-merged%>%select(-ends_with("sd"))%>%
  mutate(covariate_name=ifelse(covariate_id==200031, "age group: 100-104", 
                               ifelse(covariate_id==210031, "age group: 105-109",
                                      ifelse(covariate_id==220031, "age group: 110-114",covariate_name))))%>%
  filter(!covariate_id%in%c(230031,240031))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-ends_with("SMD"), -covariate_id, -id, -contains("Hospitalizedinfluenza"), -contains("NoHypertensionDiagnosedinfluenza"),
         -contains("Hypertension_Hospitalized_influenza"))

conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","conditions_hypertension.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]


final<-merged_pretty%>%select(1,2,contains(c(databases,"NoH")))%>%
  filter(!str_detect(covariate_name, "drug_era group during day 0 through 0 days relative to index|drug_era group during day 0 through 30 days relative to index"))%>%
  unique()

final<-merged_pretty%>%select(-contains(c("NoH", "influenza")))%>%
  filter(!str_detect(covariate_name, "drug_era group during day 0 through 0 days relative to index|drug_era group during day 0 through 30 days relative to index"),
         str_detect(covariate_name, "0 through 30"))%>%
  unique()

final<-final%>%
  mutate_at(3:length(final),funs(.*100))%>%
  mutate_at(3:length(final),funs(as.character))%>%
  mutate_at(3:length(final),funs(str_replace(., "-", "<")))

path_out="//epofs/apistillo/CHARYBDIS/Tablas/"
write.table(final,paste0(path_out,"hypertension_out.csv"), sep = ";", row.names = F, dec = ",", na="-")

colnames(final)

##### outcomes
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "hypertension.15.10.Rda"), envir = globalenv())

merged<-table

merged_pretty<-merged%>%select(-ends_with("sd"))%>%
  filter(str_detect(covariate_name, "Cohort during day 0 through 30 days"))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-ends_with("SMD"), -covariate_id, -id)

conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","conditions_hypertension.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]

databases <- c("DCMC","TRDW","CPRD", "IQVIA-DAGermany","IPCI", "IQVIA-LPDFrance", "LPDItaly")


final<-merged_pretty%>%select(1,2,contains(c(databases)))%>%
  select(1,2,contains(c("_covid")))%>%
  unique()


final<-final%>%
  mutate_at(3:length(final),funs(.*100))%>%
  mutate_at(3:length(final),funs(as.character))%>%
  mutate_at(3:length(final),funs(str_replace(., "-", "<")))

final<-merged_pretty%>%select(-contains(c(databases)))%>%
  select(1,2,contains(c("_covid")))%>%
  unique()

diag <- final%>%select(-contains("Hospitalized"))
hosp <- final%>%select(-contains("Diagnosed"))

names(diag) <- gsub("_Diagnosed_covid", "", names(diag))
names(hosp) <- gsub("_Hospitalized_covid", "", names(hosp))
final <- rbind(diag, hosp)

final<-final%>%
  mutate_at(3:length(final),funs(ifelse(.<0, NA, .)))%>%
  mutate_at(3:length(final),funs(.*100))%>%
  mutate_at(3:ncol(final), funs(format(round(.,digits = 1), nsmall = 1)))%>%
  mutate_at(3:ncol(final), funs(str_replace_all(.,"NA", "-")))
names(final) <- gsub("_Diagnosed_covid", "", names(final))
path_out="//epofs/apistillo/CHARYBDIS/Tablas/"
write.table(final,paste0(path_out,"hypertension_outcomesDiag.csv"), sep = ";", row.names = F, dec = ",", na="-")


## ALL--------
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "all.noprior.Rda"), envir = globalenv())
library(dplyr)
library(tidyr)
library(stringr)
bases<-c("HIRA","DCMC" ,"NFHCRD","HEALTHVERITY",     "Premier" ,         "OPTUM-EHR" ,    
      "OPTUM-SES"   ,
      "STARR-OMOP",       "TRDW" ,            "VA-OMOP" ,        
      "IQVIA-OpenClaims", "CUIMC"  ,          "CU-AMC-HDC" ,  
      "UWM-CRD"    ,"IQVIAHospitalCDM"  ,"OHSU"  ,"HM-Hospitals" ,   "SIDIAP"   ,       
      "SIDIAP-H" ,        "IPCI"  ,           "CPRD" ,           
      "IQVIA-LPDFrance",  "IQVIA-DAGermany",  "IQVIA-LPDItaly")
merged<-table

merged_pretty<-merged%>%select(-ends_with("sd"))%>%
  mutate(covariate_name=ifelse(covariate_id==200031, "age group: 100-104", 
                               ifelse(covariate_id==210031, "age group: 105-109",
                                      ifelse(covariate_id==220031, "age group: 110-114",covariate_name))))%>%
  filter(!covariate_id%in%c(230031,240031))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-ends_with("SMD"), -covariate_id, -id, -contains("Hospitalizedinfluenza"), -contains("NoHypertensionDiagnosedinfluenza"),
         -contains("Hypertension_Hospitalized_influenza"))

conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","conditions_all.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]


final<-merged_pretty%>%filter(!str_detect(covariate_name, "age group|gender")) %>% 
  select(1,2,contains("Diagnosed"))


final<-final%>%
  mutate_at(3:length(final),funs(ifelse(.<0, NA, .)))%>%
  mutate_at(3:length(final),funs(.*100))%>%
  mutate_at(3:ncol(final), funs(format(round(.,digits = 1), nsmall = 1)))%>%
  mutate_at(3:ncol(final), funs(str_replace_all(.,"NA", "-"))) %>% 
  select(Type, contains(bases))

#final<-final%>%filter(covariate_name=="Cohort during day 0 through 30 days start the index: death")
path_out="//epofs/apistillo/CHARYBDIS/General/"
write.table(final,paste0(path_out,"outcome.dx.28.01.csv"), sep = ";", row.names = F, dec = ".", na="-")

colnames(final)



## DIABETES----------
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "diabetes.29.10.Rda"), envir = globalenv())
library(dplyr)
library(tidyr)
merged<-table

merged_pretty<-merged%>%select(-ends_with("sd"))%>%
  mutate(covariate_name=ifelse(covariate_id==200031, "age group: 100-104", 
                               ifelse(covariate_id==210031, "age group: 105-109",
                                      ifelse(covariate_id==220031, "age group: 110-114",covariate_name))))%>%
  filter(!covariate_id%in%c(230031,240031))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-ends_with("SMD"),-contains("ICU"), -covariate_id, -id, -contains("Hospitalized_influenza"), -contains("No_T2 Diabetes_Diagnosed_influenza"),
         -contains("Hypertension_Hospitalized_influenza"))

conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","conditions_diabetes.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]


final<-merged_pretty%>%select(1,2,contains(c(databases,"No_T2")))%>%
  unique()

final<-final%>%
  mutate_at(3:length(merged_pretty),funs(.*100))%>%
  mutate_at(3:length(merged_pretty),funs(as.character))%>%
  mutate_at(3:length(merged_pretty),funs(str_replace(., "-", "<")))

path_out="//epofs/apistillo/CHARYBDIS/Tablas/"
write.table(final,paste0(path_out,"diabetes_new.csv"), sep = ";", row.names = F, dec = ",", na="")

colnames(final)

