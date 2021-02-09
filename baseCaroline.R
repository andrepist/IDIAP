## Base de datos Caroline - green spaces and mental health

library(lubridate)
library(tidyverse)
library(tableone)
library(kableExtra)

fun.age<-function(date1,date2){
  #date1 birth
  output<-year(date2)-year(date1)
  ifelse(month(date2)<month(date1)|(month(date2)==month(date1)&day(date2)<day(date1)),output-1,
         output)
}


pobl <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/poblacio.rds")
var <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/variables_cliniques.rds")
socio <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/variables_socioeconomiques.rds")
probl<- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/problemes.rds")


index.date<-as.Date("2009-01-01")

n0 <- nrow(pobl %>% distinct(id)) 

#poblacion que no se va antes del index date
pop.new <- pobl%>%filter(sortida>=index.date)%>%
  select(-idp,-idup)

#filter quita NA cuidado! hay que poner filter(condicion|is.na(variable))


n1 <- nrow(pop.new %>% distinct(id))

#poblacion que entra y se va el mismo dia
pop.new<-pop.new%>%
  filter(!sortida==entrada)

n2 <- nrow(pop.new %>% distinct(id))

# saco diagnosticos de mental diseases
dep <- probl%>%filter(str_detect(cod,"F32"))%>%
  mutate(group="Depression", general="Depression")

recdep <- probl%>%filter(str_detect(cod,"F33"))%>%
  mutate(group="Recurrent depression", general="Depression")

anx <- probl%>%filter(str_detect(cod,"F41"))%>%
  mutate(group="Anxiety",general="Anxiety")

phob <- probl%>%filter(str_detect(cod,"F40"))%>%
  mutate(group="Phobic anxiety",general="Anxiety")

# junto y miro solo los de mi poblacion 
mental<-rbind(dep, recdep, anx, phob) %>% 
  mutate(group=as.factor(group)) %>% 
  filter(id %in% pop.new$id)

#creo variables de depresion/ansiedad

# selecciono  primeros dx post index date y pongo variables en columnas
test <- mental %>% filter(dat >=index.date)%>%select(-agr,-cod,-dbaixa) %>% 
  arrange(dat)%>% distinct(id, general,.keep_all=TRUE ) %>% 
  pivot_wider(names_from = "general", values_from =c("dat","group"))

# pongo fecha de depresion o ansiedad, la que viene antes y quito la otra
test <- test %>% 
  mutate(dat_Depression=if_else(!is.na(dat_Depression)&!is.na(dat_Anxiety)&dat_Depression >dat_Anxiety,NA_Date_, dat_Depression),
         group_Depression=if_else(!is.na(dat_Depression),group_Depression,as.factor(NA))) %>% 
  mutate(dat_Anxiety=if_else(!is.na(dat_Anxiety)&!is.na(dat_Depression) &dat_Depression<dat_Anxiety,NA_Date_, dat_Anxiety),
         group_Anxiety=if_else(!is.na(dat_Anxiety),group_Anxiety,as.factor(NA)))

test <- test %>% mutate(dat=if_else(!is.na(dat_Depression), dat_Depression, dat_Anxiety)) %>% 
  select(-dat_Depression, -dat_Anxiety) %>% 
  mutate(depression=ifelse(group_Depression=="Depression", 1, NA_integer_),
         recurrentdepression=ifelse(group_Depression=="Recurrent depression", 1, NA_integer_),
         anxiety=ifelse(group_Anxiety=="Anxiety", 1, NA_integer_),
         phobicanxiety=ifelse(group_Anxiety=="Phobic anxiety", 1, NA_integer_)
         ) %>% 
  rename(date.diagnosis=dat)  %>% 
  mutate(group=ifelse(!is.na(group_Depression) & !is.na(group_Anxiety), "Dep/Anx",
                      ifelse(!is.na(group_Depression) & is.na(group_Anxiety), "Dep",
                             ifelse(is.na(group_Depression) & !is.na(group_Anxiety),"Anx","Missing"))))%>% 
  select(-group_Depression, -group_Anxiety)

pop.new <- pop.new %>% left_join(test,by="id")

# indices de gente con diagnostico previo a index.date

index.previous<-mental%>%filter(dat<index.date)%>% inner_join(pop.new %>% select(id,sexe),by="id") %>% 
  mutate(sexe=recode(sexe, "D"="F", "H"="M"), sexe=as.factor(sexe))%>%rename(sex=sexe)%>%
  distinct(id,.keep_all = TRUE)

summary(index.previous)
n3f <- nrow(index.previous %>% filter(sex=="F"))
n3m <- nrow(index.previous %>% filter(sex=="M"))

rm(dep, recdep, anx, phob)

#  quita los que tienen diagnostico previo
pop.new<-pop.new%>%
  filter(!id%in%index.previous$id)

n3 <- nrow(pop.new %>% distinct(id))

# mayores de edad solo
pop.new<-pop.new%>%
  mutate(age.start=fun.age(dnaix, index.date)) %>% 
  filter(age.start>=18)

n4 <- nrow(pop.new %>% distinct(id))

# pon situacion al final

pop.new <- pop.new %>%
  mutate(situacio=if_else(!is.na(date.diagnosis)&date.diagnosis < sortida, group, as.character(situacio)),
         situacio=as.factor(situacio))

#if_else para fechas
#ifelse para otras cosas

#mutate: 1) crear nuevas variables 2) modifica variable existente

# Covariates and exposure


# add covariates

#sex (female, male)
pop.new<-pop.new%>%mutate(sexe=recode(sexe, "D"="F", "H"="M"))%>%rename(sex=sexe)
#pop.new<-pop.new%>%mutate(sexe=recode(sexe, "D"="1", "H"="2"))%>%mutate(sexe=as.numeric(sexe))

#nationality (Spanish, non-Spanish),
pop.new<-pop.new%>%mutate(spanish=ifelse(agr_pais==1, "Spanish", "Non-Spanish"))

#and BMI using categories of the World Health Organization (underweight or normal weight [BMI <18.5kg/m2 and between ???18·5 and <25kg/m2], overweight [BMI ???25 and <30kg/m2] and obesity [BMI ???30kg/m2]). 

#select only >10 <60 
bmi<-var%>%filter(agr=="IMC")%>%
  select(-cod, -agr)%>%rename(dat.bmi=dat,val.bmi=val)

bmi<-bmi%>%filter(val.bmi>=10)%>%
  mutate(bmi.gr=ifelse(val.bmi<25, "Underweight or normal weight",
                       ifelse(val.bmi>=25&val.bmi<30, "Overweight","Obesity")))
#solo una medida, si mas, pillo la más cercana a la fecha indice y que no sea más que un año de follow up
bmi<-bmi%>%filter(dat.bmi<=index.date+years(1))%>%
  arrange(abs(dat.bmi-index.date))%>%distinct(id, .keep_all = TRUE)
#as.Date("2010-01-01")

# registered at age>=18?
#left_join(bmi,pop.new)
bmi<-bmi%>%right_join(pop.new%>%select(id,dnaix), by="id")%>%
  mutate(age.bmi=fun.age(dnaix,dat.bmi))

bmi<-bmi%>%
  filter(age.bmi>=18)

pop.new<-pop.new%>%left_join(bmi%>%select(id,val.bmi,bmi.gr), by="id")
rm(bmi)
#census socioeconomic status using the Mortality in small Spanish areas and Socioeconomic and Environmental Inequalities (MEDEA) deprivation index, urbanity/rurality, 
#missings are ""
socio<-socio%>%select(-ind_aquas)%>%mutate_at(c("ruralitat","qmedea"),
                                              funs(ifelse(.=="",NA,.)))
pop.new<-pop.new%>%left_join(socio,by="id")%>%distinct()
rm(socio)


# add exposure variable

#NDVI y GS_perc for every periode
urban <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/urban_orbecan.rds")
urb<-na.omit(urban%>%select(id, NDVI, GS_perc,periode, changes, IP2011, found))%>%distinct()
urb<-urb %>% filter(id%in%pop.new$id)
urb<-urb %>%pivot_wider(names_from="periode", values_from=c("NDVI", "GS_perc","found","IP2011"))

pop.new<-pop.new%>%
  left_join(urb,by=c("id"))

# año de entrada o si es del 2010 la info del 2011 (si tampoco tiene del 2011, es missing)

urba <- na.omit(urban%>%select(id, NDVI, GS_perc,periode)) %>% 
  distinct()%>% 
  filter(id%in%pop.new$id) 

pop.new<-pop.new%>%mutate(periode=ifelse(year(entrada)<=2010, 2011, year(entrada)))%>%
  left_join(urba%>%rename(NDVI.entry=NDVI, GS_perc.entry=GS_perc),by=c("id","periode"))


rm(urb)
# pop.new<-pop.new%>%
#   mutate(NDVI.gr=ifelse(is.na(NDVI_entry),NA,"NDVI"),
#          GS.gr=ifelse(is.na(GS_perc_entry),NA, "GS_perc"))

#exclusion: those without info on NDVI nor GS%
#pop.new1<-pop.new%>%filter(!(is.na(NDVI)&is.na(GS_perc)))
#summary(pop.new1)

# 
# prop.table(table(person$medea, useNA = "always"))


# years of follow up
pop.new<-pop.new%>%mutate(entry=if_else(entrada<index.date, index.date, entrada ))

pop.new<-pop.new%>%mutate(follow=as.numeric(trunc((sortida-entry)/365.25)))

# fumador Fecha más cercana a la fecha índice: cualquier info previa, 1 año despúes fecha Índice
tabac <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/ECHOCAT - Talita/Bases de datos/tabac.rds")
tabac <- tabac%>%
  filter(dat<=index.date+years(1))%>%
  arrange(abs(dat-index.date))%>%
  distinct(id, .keep_all = TRUE)%>%
  mutate(tabac=ifelse(val==0, "Never smoker", 
                      ifelse(val==1, "Smoker",
                             ifelse(val==2, "Previous smoker", "Missing"))))%>%
  select(-dbaixa,-dat,-val)
pop.new<-pop.new%>%left_join(tabac, by="id")
rm(tabac)

#0=No consumo de alcohol, 1= Consumo de Alcohol de bajo riesgo, 2 y 3= Consumo de Alcohol de alto riesgo
#Fecha más cercana a la fecha índice: cualquier info previa, 1 año despúes fecha Índice
# está el 3 tambien?

alc <- var%>%filter(agr=="ALCOHOL")
alc<-alc%>%
  filter(dat<=index.date+years(1))%>%
  arrange(abs(dat-index.date))%>%
  distinct(id, .keep_all = TRUE)%>%
  mutate(alcohol=ifelse(val==0, "No alcohol", 
                                         ifelse(val==1, "Alcohol small risk",
                                                ifelse(val==2, "Alcohol high risk", "Missing"))))%>%
  select(id,alcohol)%>%
  mutate(alcohol=as.factor(alcohol))

pop.new<-pop.new%>%left_join(alc, by="id")
rm(alc)

#charlson index
charl <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/charlson.rds")
charl <- charl%>%
  filter(dat<=index.date+years(1))%>%
  arrange(abs(dat-index.date))%>%
  distinct(id, .keep_all = TRUE)%>%
  select(id, index_ch) %>% 
  mutate(index_ch=ifelse(index_ch >=3, "3+", as.character(index_ch)),
         index_ch=as.factor(index_ch)) 
  
pop.new<-pop.new%>%left_join(charl, by="id")

rm(charl)

#numero visites per any (l'any previ)

visites <- readRDS("//epofs/apistillo/EPOFS/SIDIAP/20180514 OBECAN - Talita/Bases de datos/visites.rds")
visites1 <- visites %>% 
  filter(id%in%pop.new$id) 

visites1 <- visites1 %>% 
  filter(agr=="INF"|agr=="GINE"|agr=="MG")

visites1 <- visites1%>%
  select(id,dat) %>% 
  left_join(pop.new %>% select(id, entry),by="id")  %>% 
  filter(dat>=entry-years(1),dat<entry)

visites2 <- visites1 %>% select(id, dat) %>%
  distinct(id, dat, .keep_all = TRUE)%>%
  group_by(id) %>% 
  mutate(n.visits.prior=n()) %>% 
  select(-dat) %>% 
  distinct(id,.keep_all = TRUE)


pop.new<-pop.new%>%left_join(visites2, by="id")
rm(visites1,visites2)

###

# tidy and rename
pop.new <- pop.new %>% rename(exit.status=situacio, date.status=sortida) %>% 
  select(-entrada)


# descriptiva ---------
vars<-c("sex", 
        "age.start",
        "spanish",
        "val.bmi",
        "bmi.gr",
        "exit.status",
        "qmedea",
        "depression",
        "recurrentdepression",
        "anxiety",
        "phobicanxiety",
        "group",
        "NDVI.entry",
        "GS_perc.entry",
        "follow",
        "tabac",
        "alcohol",
        "index_ch",
        "n.visits.prior",
        "changes"
       )
factor.vars<- c("sex",
                "spanish", 
                "bmi.gr",
                "exit.status",
                "qmedea",
                "depression",
                "recurrentdepression",
                "anxiety",
                "phobicanxiety",
                "group",
                "tabac",
                "alcohol",
                "index_ch"
                )

summary<-cbind(
  
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = pop.new,
    test = F), 
    showAllLevels=T,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE)
  # ,
  # 
  # print(CreateTableOne(
  #   vars =  vars,
  #   factorVars = factor.vars,
  #   includeNA=T,
  #   data = pop.new%>%filter(depression==1),
  #   test = F), 
  #   showAllLevels=F,smd=F,
  #   nonnormal = vars, 
  #   noSpaces = TRUE,
  #   contDigits = 1,
  #   printToggle=FALSE),
  # 
  # print(CreateTableOne(
  #   vars =  vars,
  #   factorVars = factor.vars,
  #   includeNA=T,
  #   data = pop.new%>%filter(recurrentdepression==1),
  #   test = F), 
  #   showAllLevels=F,smd=F,
  #   nonnormal = vars, 
  #   noSpaces = TRUE,
  #   contDigits = 1,
  #   printToggle=FALSE),
  # 
  # print(CreateTableOne(
  #   vars =  vars,
  #   factorVars = factor.vars,
  #   includeNA=T,
  #   data = pop.new%>%filter(anxiety==1),
  #   test = F), 
  #   showAllLevels=F,smd=F,
  #   nonnormal = vars, 
  #   noSpaces = TRUE,
  #   contDigits = 1,
  #   printToggle=FALSE),
  # 
  # print(CreateTableOne(
  #   vars =  vars,
  #   factorVars = factor.vars,
  #   includeNA=T,
  #   data = pop.new%>%filter(phobic==1),
  #   test = F), 
  #   showAllLevels=F,smd=F,
  #   nonnormal = vars, 
  #   noSpaces = TRUE,
  #   contDigits = 1,
  #   printToggle=FALSE)
)

nice.num<-function(x){
  prettyNum(x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE)}
for(i in 1:ncol(summary)) {
  # tidy up 
  cur_column <- summary[, i]
  cur_column <- str_extract(cur_column, '[0-9.]+\\b') %>% 
    as.numeric() 
  cur_column <-nice.num(cur_column)
  # add back in
  summary[, i] <- str_replace(string=summary[, i], 
                              pattern='[0-9.]+\\b', 
                              replacement=cur_column)    
}
library(kableExtra)
kable(summary) %>% 
# kable(summary,
#       col.names=c( "Overall", "Depression", "Recurrent depression", "Anxiety", "Phobic anxiety"))%>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 



# flowchart -----------
library(DT)
library(DiagrammeR)
library(DiagrammeRsvg)
library(stringr)
library(rsvg)

flow <- grViz("
digraph a_nice_graph
      {
      
      node [fontname = Helvetica, shape = box, color = black, penwidth = 1.5, style = lisrel]
      '@@1';'@@2';'@@3';'@@4';'@@5';'@@6';'@@7';'@@8';
       node [fontname = Helvetica, shape = box, color = '#FF8700', penwidth = 1.5, style = lisrel]
      '@@9';
      
      blank1[label = '', width = 0.01, height = 0.01]
      blank2[label = '', width = 0.01, height = 0.01]
      blank3[label = '', width = 0.01, height = 0.01]
      blank4[label = '', width = 0.01, height = 0.01]

     

    
      '@@1' -> blank1[ dir = none ];
      blank1 -> '@@2'[ minlen = 0 ,arrowhead = none, tailport = T];
      blank1 -> '@@3' ;  
      '@@3' -> blank2[dir = none];
      blank2 -> '@@4'[ minlen = 0 ,arrowhead = none, tailport = T];
      blank2 -> '@@5';
      '@@5' -> blank3[dir = none];
      blank3 -> '@@6'[ minlen = 0 ,arrowhead = none, tailport = T];
      blank3 -> '@@7'
      '@@7' -> blank4[dir = none];
      blank4 -> '@@8'[ minlen = 0 ,arrowhead = none, tailport = T];
      blank4 -> '@@9'
      
    
      
      }
      [1]: paste0('SIDIAP general population','\\n','N = ', format(n0,big.mark = ',', decimal.mark='.'))
      [2]:  paste0('Salida antes del index date', '\\n','N = ', format(n0-n1,big.mark = ',', decimal.mark='.'))
      [3]:  paste0('Remaining population', '\\n','N = ', format(n1,big.mark = ',', decimal.mark='.')) 
      [4]:  paste0('Entrada y salida el mismo dia', ', N = ', format(n1-n2, big.mark = ',',decimal.mark='.'))
      [5]:  paste0('Remaining population', '\\n','N = ', format(n2,big.mark = ',', decimal.mark='.')) 
       [6]:  paste0('Diagnostico de ansiedad o depresión previo','\\n','N = ', format(n2-n3,big.mark = ',', decimal.mark='.'),'\\n',format(n3f,big.mark = ',', decimal.mark='.'), ' women, ', format(n3m,big.mark = ',', decimal.mark='.'), ' men')
       [7]:  paste0('Remaining population ', '\\n','N = ', format(n3,big.mark = ',', decimal.mark='.')) 
       [8]:  paste0('Menores de edad', ', N = ', format(n3-n4,big.mark = ',', decimal.mark='.'))
      [9]:  paste0('Remaining population', '\\n','N = ', format(n4,big.mark = ',', decimal.mark='.')) 
     
      ", width = 400,height = 800)
flow
flow%>% export_svg %>% charToRaw %>% rsvg %>% png::writePNG("flow_caroline.png")

# save --------


saveRDS(pop.new, file = "databaseCaroline.rds")
pop.new <- readRDS("databaseCaroline.rds")
# export data frame to Stata binary format
library(foreign)
write.dta(pop.new, "databaseCaroline.dta")
