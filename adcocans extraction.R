library(lubridate)
library(tidyverse)

fun.age<-function(date1,date2){
  #date1 birth
  output<-year(date2)-year(date1)
  ifelse(month(date2)<month(date1)|(month(date2)==month(date1)&day(date2)<day(date1)),output-1,
         output)
}

pobl1 <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20200309 [338] COMOCANS/Bases de datos/COMOCANS_entregable_poblacio_20200210_105524.rds")

summary(pobl1)

pobl1 <- transform(pobl1, dnaix_date = as.Date(as.character(dnaix), "%Y%m%d"))

pobl1<- transform(pobl1, entrada_date = as.Date(as.character(entrada), "%Y%m%d"))

pobl1 <- transform(pobl1, sortida_date = as.Date(as.character(sortida), "%Y%m%d"))

start.date <- as.Date("2010-01-01")
end.date <- as.Date("2019-12-31")

# age at entry
pobl1 <- pobl1 %>% mutate(age.entry=fun.age(dnaix_date, start.date))
# averiguar si está bien que a la entrada al estudio haya gente con menos de 40 años

names(pobl1)

#cardiovascular disease:
#angina

diagnostics <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20200309 [338] COMOCANS/Bases de datos/COMOCANS_entregable_diagnostics_20200220_194940.rds")
View(head(diagnostics, 50))



card.disease <- diagnostics %>% filter(cod%in%c("I20","I21","I22","I23","I24","I25","I60","I61","I63","I64","I62","I65","I66",
                                                "I67","I68","I69"))   %>% 
  mutate(date.card.disease=as.Date(as.character(dat), "%Y%m%d"),
         dx.card.disease=1) %>% 
  select(idp, dx.card.disease,date.card.disease ) %>% 
  filter(date.card.disease>=start.date, date.card.disease<=end.date) %>% 
  arrange(date.card.disease) %>% 
  distinct(idp, .keep_all=TRUE)

nrow(card.disease)


pobl1 <- pobl1 %>% left_join(card.disease, by = "idp")

