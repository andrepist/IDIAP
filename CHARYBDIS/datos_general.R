# dataset de general
library(tidyverse)
#output: excel file with the dataset
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions<- read.csv(paste0(path_cond,"grupos_edad.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "general_male_noprior.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(-contains(c("_No_Sex", "SMD","_sd")),-covariate_id,-id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]%>%
  filter(!is.na(Type))
test<-merged_pretty%>%pivot_longer(cols=contains("Sex"), names_to=c("base","sex", "hospit", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo") %>% 
  mutate(prior="no prior")


load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "general_female_noprior.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(-contains(c("_No_Sex", "SMD","_sd")),-covariate_id,-id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]%>%
  filter(!is.na(Type))
test2<-merged_pretty%>%pivot_longer(cols=contains("Sex"), names_to=c("base","sex", "hospit", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo")%>% 
  mutate(prior="no prior")



final<-rbind(test, test2) %>% select(-influenza)

#final <- left_join(final, row2 %>% select(-cohort_name, -disease), by=c("hospit", "prior", "base"))

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "all.noprior.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(-contains(c("_No_Sex", "SMD","_sd")),-covariate_id,-id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)
merged_pretty<-merged_pretty %>% filter(str_detect(covariate_name, "gender")) %>%
  distinct(covariate_name, .keep_all = TRUE)
test<-merged_pretty%>%pivot_longer(cols=-c(1,2), names_to=c("sex","base", "hospit", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo") %>% 
  mutate(prior="no prior")


final2<-test %>% 
  separate(covariate_name, into=c(NA, "sex"), sep="= " ) %>% 
  mutate(mean=format(mean*100,nsmall=1)) %>% 
  pivot_wider(values_from = "mean", names_from="sex")

final <- left_join(final, final2 %>% select(-Type), by=c("hospit", "prior", "base"))

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "count.all.Rda"), envir = globalenv())
final <- inner_join(final, row2 %>% select(-c(1,2)), by=c("prior", "base", "hospit"))

final <- rename(final,N=value)

save(final,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "data_age.29.01.Rda"))

#load(paste0("//epofs/RWCancerEpi/CHARYBDIS/Cancer/", "datacancer.Rda"), envir = globalenv())


# dataset de general
#output: excel file with the dataset
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions<- read.csv(paste0(path_cond,"grupos_edad.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "edades_male_covid_uci.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(starts_with("covariate_name"), starts_with("Sex"),covariate_id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD"))%>%
  select( -covariate_id)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]%>%
  filter(!is.na(Type))
test<-merged_pretty%>%pivot_longer(cols=starts_with("Sex"), names_to=c("sex","base", "hospit", "tipo"), names_sep="_")%>%
  mutate(disease="COVID-19")%>%mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo")

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "edades_male_flu_uci.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(starts_with("covariate_name"), starts_with("Sex"),covariate_id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD"))%>%
  select( -covariate_id)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]%>%
  filter(!is.na(Type))

test2<-merged_pretty%>%pivot_longer(cols=starts_with("Sex"), names_to=c("sex","base", "hospit", "tipo"), names_sep="_")%>%
  mutate(disease="Influenza")%>%mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo")

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "edades_female_covid_uci.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(starts_with("covariate_name"), starts_with("Sex"),covariate_id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD"))%>%
  select( -covariate_id)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]%>%
  filter(!is.na(Type))

test3<-merged_pretty%>%pivot_longer(cols=starts_with("Sex"), names_to=c("sex","base", "hospit", "tipo"), names_sep="_")%>%
  mutate(disease="COVID-19")%>%mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo")

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "edades_female_flu_uci.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%select(starts_with("covariate_name"), starts_with("Sex"),covariate_id)%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ":", remove = FALSE)%>%
  select(-starts_with("SMD"))%>%
  select( -covariate_id)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]%>%
  filter(!is.na(Type))

test4<-merged_pretty%>%pivot_longer(cols=starts_with("Sex"), names_to=c("sex","base", "hospit", "tipo"), names_sep="_")%>%
  mutate(disease="Influenza")%>%mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%
  pivot_wider(values_from = "value", names_from="tipo")

final1<-rbind(test, test2, test3, test4)

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "data_age.Rda"), envir = globalenv())
final2<-rbind(final, final1)

save(final2,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "data_age.Rda"))

#add icu flu sidiap-h
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "data_age.Rda"), envir = globalenv())

#############
#dataset for scatter : demografic conditions symptomes
library(dplyr)
library(tidyr)
library(stringr)
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions<- read.csv(paste0(path_cond,"conditions_all.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)
conditions<-conditions[c(1:22),]

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "result.all.noprior.covid.18.11.Rda"), envir = globalenv())
merged<-table
merged_pretty<-merged%>%
  select(-starts_with("SMD"))%>%
  select( -covariate_id)
merged_pretty<-merged_pretty[match(tolower(conditions$covariate_name),tolower(merged_pretty$covariate_name)),]
test<-merged_pretty%>%pivot_longer(cols=2:ncol(merged_pretty), names_to=c("all","base", "hospit"), names_sep="_")%>%
  select(-all)%>%mutate(strata="Demographics")%>%rename(type=covariate_name)

data<- read.csv2(paste0("//epofs/apistillo/CHARYBDIS/General/","data.scatter.csv"), sep=";", stringsAsFactors = FALSE)
test2<-data%>%mutate_at(2:(ncol(data)-2), funs(as.numeric(.)))
test2<-test2%>%pivot_longer(cols=2:(ncol(data)-2), names_to="base")
test2<-test2%>%mutate(base=str_replace_all(base,"\\.","-"), value=value/100)

data<-rbind(test,test2)
data<-na.omit(data%>%mutate(sd=sqrt(value*(1-value))))

diag<-data%>%filter(hospit=="Diagnosed")
hosp<-data%>%filter(hospit=="Hospitalized")
data<-left_join(diag, hosp, by = c("type", "base", "strata"))  
data<-data%>%mutate(smd=((abs(value.x)-abs(value.y))/(sqrt(sd.x^2+sd.y^2))))

save(data,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "general.toscatter.Rda"))
