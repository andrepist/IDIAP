library(tidyverse)
library(ggrepel)
library(viridis)
library(forcats)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

databases_primary<-c( "IQVIA-OpenClaims","HEALTHVERITY", "SIDIAP",
                      "IQVIA-LPDFrance",	"IQVIA-DAGermany","IQVIA-LPDItaly",	"CPRD","IPCI")


databases_hospit<-c("IQVIA-OpenClaims","OPTUM-EHR",	"VA-OMOP",
                    "HEALTHVERITY","HIRA",
                    "CUIMC",	"CU-AMC-HDC","HDM", "STARR-OMOP")


# database cleaning at the end

#save(data.clean,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "hypertension.allfeatures.290121.Rda"))
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "hypertension.allfeatures.290121.Rda"))

test <- data.clean %>% filter(disease=="covid", hospit=="Diagnosed", hyper=="Hypertension") %>% 
  select(base, n) %>% distinct() %>% 
  filter(base%in%databases_primary|base%in%databases_hospit)
sum(test$n)

#COVID-19-------

data.clean.covid <- data.clean %>% filter(disease=="covid",
                              !hospit=="ICU") %>% 
  mutate(hyper=recode(hyper, "Hypertension"="With Hypertension",
                      "No Hypertension"= "Without Hypertension")) %>% distinct() %>% 
  mutate(base=recode(base, "IQVIA-OpenClaims"="IQVIA-OpenClaims-US",
                     "HEALTHVERITY"="HEALTHVERITY-US", 
                     "SIDIAP"= "SIDIAP-Spain",
                     "IQVIA-LPDFrance"="IQVIA-LPD-France",
                     "IQVIA-DAGermany"="IQVIA-DA-Germany",
                     "IQVIA-LPDItaly"="IQVIA-LPD-Italy",
                     "CPRD"="CPRD-UK",
                     "IPCI"="IPCI-The Netherlands",
                     "OPTUM-EHR"="OPTUM-EHR-US",
                     "VA-OMOP"="VA-OMOP-US",
                     "HIRA"="HIRA-South Korea",
                     "CUIMC"="CUIMC-US",
                     "CU-AMC-HDC"="CU-AMC-HDC-US",
                     "HDM"="HDM-Spain",
                     "STARR-OMOP"="STARR-OMOP-US"))

rm(data.clean)
# tabla farmacos --------
conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","Blood pressure lowering drugs.csv"), header = FALSE, sep='\t')%>%
  mutate(id = row_number())%>%rename(covariate_name = V1)%>% 
  mutate(covariate_name=trimws(covariate_name),
         covariate_name=as.factor(covariate_name))

plot.data <- data.clean.covid %>% mutate(covariate_name=tolower(covariate_name))%>% 
  inner_join(conditions%>% mutate(covariate_name=tolower(covariate_name)),by="covariate_name") %>% 
  select(base, hospit, covariate_name, value,id, hyper) %>% 
  distinct()
plot.data1 <-plot.data %>%  
  arrange(desc(id)) %>% 
  distinct(hyper,base, hospit,covariate_name,.keep_all = TRUE ) %>% 
  pivot_wider(names_from=c("base", "hospit", "hyper"), values_from=value)
tabla <- plot.data1 %>% arrange(id) %>% 
  mutate_at(3:length(plot.data1),funs(.*100))%>%
  mutate_at(3:length(plot.data1),funs(prettyNum(.,nsmall=1)))%>%
  mutate_at(3:length(plot.data1),funs(str_replace(., "-", "<"))) %>% 
  separate(covariate_name, into=c(NA,"Farmaco"), sep="index: ")

#dx
tabla.dx <- tabla %>% select(1,2,contains(databases_primary)) %>% select(1,2,contains("Diagnosed"))
write.table(tabla.dx,paste0("//epofs/apistillo/CHARYBDIS/Hypertension/","farmacs.dx.csv"), sep = ";", row.names = F, dec = ",", na="-")
#hosp
tabla.h <- tabla %>% select(1,2,contains(databases_hospit)) %>% select(1,2,contains("Hospitalized"))
write.table(tabla.h,paste0("//epofs/apistillo/CHARYBDIS/Hypertension/","farmacs.h.csv"), sep = ";", row.names = F, dec = ",", na="-")

# age histograms ----
library(gridExtra)

sex<- data.clean.covid %>% filter(str_detect(covariate_name, "gender = FEMALE")) %>% 
  separate(covariate_name, into=c(NA, "Type"),remove=FALSE,sep=": ") %>% rename(gender=value) %>% 
  select(-covariate_name,-Type) %>% 
  mutate(gender=prettyNum(gender*100, nsmall = 1)) 
#cu-amc has several values for female, i choose one
sex <- sex[-c(5,7,9),]

plot.data <- data.clean.covid %>% filter(str_detect(covariate_name, "age group")) %>% 
  separate(covariate_name, into=c(NA, "Type"),remove=FALSE,sep=": ") %>% left_join(sex)



data<-inner_join(filter(plot.data%>%filter(disease=="covid"),hyper=="With Hypertension"),
                 filter(plot.data%>%filter(disease=="covid"),hyper=="Without Hypertension"),
                 by=c("Type", "base", "hospit", "disease")) %>% 
  mutate(var=paste0(base, "\nHypertension, N = ",
                    prettyNum(n.x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE),
                    ", % female, ", 
                    gender.x, "%",
                    "\nNo Hypertension, N = ",
                    prettyNum(n.y, big.mark=",", nsmall = 0, digits=0,scientific = FALSE),
                     ", % female, ", 
                    gender.y, "%"))%>%
  select(-contains("x"), -contains("y"))%>%unique()

to.plot<-left_join(plot.data%>%filter(disease=="covid"),data)%>%
  filter(str_detect(covariate_name,"age group"),
         value >= 0, !(base=="HIRA"&hospit=="Diagnosed"))%>%
  select(-cohort_name,-N,-covariate_name)%>%
  rbind(tibble(Type=NA,base="CPRD", hyper="With Hypertension",hospit="Hospitalized", disease="covid", value=0, n=0, var="CPRD", gender=NA),
        tibble(Type=NA,base="IQVIA-LPDItaly", hyper="With Hypertension",hospit="Hospitalized", disease="covid", value=0, n=0, var="IQVIA-LPDItaly", gender=NA),
        tibble(Type=NA,base="TRDW", hyper="With Hypertension",hospit="Hospitalized", disease="covid",value=0,  n=0, var="TRDW", gender=NA),
        tibble(Type=NA,base="IQVIA-DAGermany", hyper="With Hypertension",hospit="Hospitalized", disease="covid", value=0, n=0, var="IQVIA-DAGermany", gender=NA),
        tibble(Type=NA,base="IQVIA-LPDFrance", hyper="With Hypertension",hospit="Hospitalized", disease="covid", value=0, n=0, var="IQVIA-LPDFrance", gender=NA),
        tibble(Type=NA,base="IPCI", hyper="With Hypertension",hospit="Hospitalized", disease="covid", value=0, n=0, var="IPCI", gender=NA),
        tibble(Type=NA,base="HIRA", hyper="With Hypertension",hospit="Diagnosed", disease="covid",value=0,  n=0, var="HIRA", gender=NA),
        tibble(Type=NA,base="DCMC", hyper="With Hypertension",hospit="Hospitalized", disease="covid",value=0,  n=0, var="DCMC", gender=NA))%>% distinct()

#update database name
databases_primary<-c("IQVIA-OpenClaims-US",
                     "HEALTHVERITY-US", 
                     "SIDIAP-Spain",
                     "IQVIA-LPD-France",
                     "IQVIA-DA-Germany",
                     "IQVIA-LPD-Italy",
                     "CPRD-UK",
                     "IPCI-The Netherlands")
databases_hospit<-c("IQVIA-OpenClaims-US",
                    "OPTUM-EHR-US",
                    "VA-OMOP-US",
                    "HEALTHVERITY-US",
                    "HIRA-South Korea",
                    "CUIMC-US",
                    "CU-AMC-HDC-US",
                    "HDM-Spain",
                    "STARR-OMOP-US")
# primary care
to.plot.d<-to.plot%>%filter(hospit=="Diagnosed", base %in%databases_primary) %>% mutate(base=factor(base, levels=databases_primary))

p.diag<-ggplot(to.plot.d, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base ,nrow=4, drop=FALSE, labeller=labeller(base=setNames(to.plot.d$var,to.plot.d$base)))+
  geom_bar(data=to.plot.d%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.d%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        text = element_text(size=14),
        strip.text = element_text(size=14),#face = "bold"),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  # scale_fill_manual(breaks=c("With Obesity","Without Obesity"),
  #                   values=c("With Obesity"="#FF1E00", "Without Obesity"="#0776A0"))+
  coord_cartesian(ylim = c(0,15))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  #ggtitle("Diagnosed")+
  ylab("%")+
  xlab("Age")
p.diag


ggsave(p.diag, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.hist.primary3.png"),
       dpi=300,
       width = 11.3, height = 10)

# hispital



to.plot.h<-to.plot%>%filter(hospit=="Hospitalized", base %in% databases_hospit)%>% mutate(base=factor(base, levels=databases_hospit))
p.hosp<-ggplot(to.plot.h, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base, nrow=5,  drop=FALSE,labeller=labeller(base=setNames(to.plot.h$var,to.plot.h$base)))+
  geom_bar(data=to.plot.h%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.h%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+ 
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size=14),
        text = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  coord_cartesian(ylim = c(0,25))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
 # ggtitle("Hospitalized")+
  ylab(NULL)+
  xlab("Age")
p.hosp


ggsave(p.hosp, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.hist.hospit3.png"),
       dpi=300,
       width = 11.3, height = 13)

# claims
to.plot.d<-to.plot%>%filter(hospit=="Diagnosed", base %in% databases_claims)
p.diag<-ggplot(to.plot.d, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base , nrow=16, dir="v", drop=FALSE, labeller=labeller(base=setNames(to.plot.d$var,to.plot.d$base)))+
  geom_bar(data=to.plot.d%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.d%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        text = element_text(size=14),
        strip.text = element_text(size=14),#face = "bold"),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  # scale_fill_manual(breaks=c("With Obesity","Without Obesity"),
  #                   values=c("With Obesity"="#FF1E00", "Without Obesity"="#0776A0"))+
  coord_cartesian(ylim = c(0,20))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Diagnosed")+
  ylab("%")+
  xlab("Age")
p.diag


to.plot.h<-to.plot%>%filter(hospit=="Hospitalized", base %in% databases_claims)
p.hosp<-ggplot(to.plot.h, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base, nrow=16, dir="v", drop=FALSE,labeller=labeller(base=setNames(to.plot.h$var,to.plot.h$base)))+
  geom_bar(data=to.plot.h%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.h%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+ 
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size=14),
        text = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  coord_cartesian(ylim = c(0,20))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Hospitalized")+
  ylab(NULL)+
  xlab("Age")
p.hosp

leg<-get_legend(p.hosp)
fore <- grid.arrange(p.diag+theme(legend.position = "none"),p.hosp+theme(legend.position = "none"), ncol = 2, widths=c(20,20))
forest<-grid.arrange(fore,leg, nrow=2, heights=c(20,1))

ggsave(forest, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.hist.claims2.png"),
       dpi=300,
       width = 11.3, height = 8)


# Influenza ---------


data.clean.covid <- data.clean %>% filter(disease=="influenza",
                                          !hospit=="ICU") %>% 
  mutate(hyper=recode(hyper, "Hypertension"="With Hypertension",
                      "No Hypertension"= "Without Hypertension")) %>% distinct()


# age histograms ----
library(gridExtra)

sex<- data.clean.covid %>% filter(str_detect(covariate_name, "gender = FEMALE")) %>% 
  separate(covariate_name, into=c(NA, "Type"),remove=FALSE,sep=": ") %>% rename(gender=value) %>% 
  select(-covariate_name,-Type) %>% 
  mutate(gender=prettyNum(gender*100, nsmall = 1)) 


plot.data <- data.clean.covid %>% filter(str_detect(covariate_name, "age group")) %>% 
  separate(covariate_name, into=c(NA, "Type"),remove=FALSE,sep=": ") %>% 
  left_join(sex)



data<-inner_join(filter(plot.data%>%filter(disease=="influenza"),hyper=="With Hypertension"),
                 filter(plot.data%>%filter(disease=="influenza"),hyper=="Without Hypertension"),
                 by=c("Type", "base", "hospit", "disease")) %>% 
  mutate(var=paste0(base, "\nHypertension, N = ",
                    prettyNum(n.x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE),
                    ", % female, ", 
                    gender.x, "%",
                    "\nNo Hypertension, N = ",
                    prettyNum(n.y, big.mark=",", nsmall = 0, digits=0,scientific = FALSE),
                    ", % female, ", 
                    gender.y, "%"))%>%
  select(-contains("x"), -contains("y"))%>%unique()

to.plot<-left_join(plot.data%>%filter(disease=="influenza"),data)%>%
  filter(str_detect(covariate_name,"age group"),
         value >= 0, !(base=="HIRA"&hospit=="Diagnosed"))%>%
  select(-cohort_name,-N,-covariate_name)%>%
  rbind(tibble(Type=NA,base="CPRD", hyper="With Hypertension",hospit="Hospitalized", disease="influenza", value=0, n=0, var="CPRD", gender=NA),
        tibble(Type=NA,base="IQVIA-LPDItaly", hyper="With Hypertension",hospit="Hospitalized", disease="influenza", value=0, n=0, var="IQVIA-LPDItaly", gender=NA),
        tibble(Type=NA,base="TRDW", hyper="With Hypertension",hospit="Hospitalized", disease="influenza",value=0,  n=0, var="TRDW", gender=NA),
        tibble(Type=NA,base="IQVIA-DAGermany", hyper="With Hypertension",hospit="Hospitalized", disease="influenza", value=0, n=0, var="IQVIA-DAGermany", gender=NA),
        tibble(Type=NA,base="IQVIA-LPDFrance", hyper="With Hypertension",hospit="Hospitalized", disease="influenza", value=0, n=0, var="IQVIA-LPDFrance", gender=NA),
        tibble(Type=NA,base="IPCI", hyper="With Hypertension",hospit="Hospitalized", disease="influenza", value=0, n=0, var="IPCI", gender=NA),
        tibble(Type=NA,base="HIRA", hyper="With Hypertension",hospit="Diagnosed", disease="influenza",value=0,  n=0, var="HIRA", gender=NA))%>% distinct()


# primary care
to.plot.d<-to.plot%>%filter(hospit=="Diagnosed", base %in% databases_primary)
p.diag<-ggplot(to.plot.d, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base , nrow=16, dir="v", drop=FALSE, labeller=labeller(base=setNames(to.plot.d$var,to.plot.d$base)))+
  geom_bar(data=to.plot.d%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.d%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        text = element_text(size=14),
        strip.text = element_text(size=14),#face = "bold"),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  # scale_fill_manual(breaks=c("With Obesity","Without Obesity"),
  #                   values=c("With Obesity"="#FF1E00", "Without Obesity"="#0776A0"))+
  coord_cartesian(ylim = c(0,20))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Diagnosed")+
  ylab("%")+
  xlab("Age")
p.diag



ggsave(p.diag, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.hist.primary.flu.png"),
       dpi=300,
       width = 6, height = 11)



# hispital
to.plot.d<-to.plot%>%filter(hospit=="Diagnosed", base %in% databases_hospit) 
p.diag<-ggplot(to.plot.d, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base , nrow=16, dir="v", drop=FALSE, labeller=labeller(base=setNames(to.plot.d$var,to.plot.d$base)))+
  geom_bar(data=to.plot.d%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.d%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        text = element_text(size=14),
        strip.text = element_text(size=14),#face = "bold"),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  # scale_fill_manual(breaks=c("With Obesity","Without Obesity"),
  #                   values=c("With Obesity"="#FF1E00", "Without Obesity"="#0776A0"))+
  coord_cartesian(ylim = c(0,30))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Diagnosed")+
  ylab("%")+
  xlab("Age")
p.diag




to.plot.h<-to.plot%>%filter(hospit=="Hospitalized", base %in% databases_hospit)
p.hosp<-ggplot(to.plot.h, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base, nrow=16, dir="v", drop=FALSE,labeller=labeller(base=setNames(to.plot.h$var,to.plot.h$base)))+
  geom_bar(data=to.plot.h%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.h%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+ 
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size=14),
        text = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  coord_cartesian(ylim = c(0,30))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Hospitalized")+
  ylab(NULL)+
  xlab("Age")
p.hosp


leg<-get_legend(p.hosp)
fore <- grid.arrange(p.diag+theme(legend.position = "none"),p.hosp+theme(legend.position = "none"), ncol = 2, widths=c(20,20))
forest<-grid.arrange(fore,leg, nrow=2, heights=c(20,1))

ggsave(forest, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.hist.hospit.flu.png"),
       dpi=300,
       width = 11.3, height = 18)

# claims
to.plot.d<-to.plot%>%filter(hospit=="Diagnosed", base %in% databases_claims)
p.diag<-ggplot(to.plot.d, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base , nrow=16, dir="v", drop=FALSE, labeller=labeller(base=setNames(to.plot.d$var,to.plot.d$base)))+
  geom_bar(data=to.plot.d%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.d%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        text = element_text(size=14),
        strip.text = element_text(size=14),#face = "bold"),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  # scale_fill_manual(breaks=c("With Obesity","Without Obesity"),
  #                   values=c("With Obesity"="#FF1E00", "Without Obesity"="#0776A0"))+
  coord_cartesian(ylim = c(0,35))+
  scale_x_discrete(drop = FALSE,breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Diagnosed")+
  ylab("%")+
  xlab("Age")
p.diag


to.plot.h<-to.plot%>%filter(hospit=="Hospitalized", base %in% databases_claims)
p.hosp<-ggplot(to.plot.h, aes(x=Type, y=value*100, fill=hyper))+
  facet_wrap(~ base, nrow=16, dir="v", drop=FALSE,labeller=labeller(base=setNames(to.plot.h$var,to.plot.h$base)))+
  geom_bar(data=to.plot.h%>%filter(hyper=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.h%>%filter(hyper=="With Hypertension"), width=0.4,stat="identity")+ 
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(size=14),
        text = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5),
        plot.title = element_text(hjust = 0.5,face = "bold"))+
  coord_cartesian(ylim = c(0,35))+
  scale_x_discrete(breaks=c( "10-14" ,"30-34","50-54","70-74","90-94"), labels=c("10-14" ,"30-34","50-54","70-74","90-94"))+
  ggtitle("Hospitalized")+
  ylab(NULL)+
  xlab("Age")
p.hosp


leg<-get_legend(p.hosp)
fore <- grid.arrange(p.diag+theme(legend.position = "none"),p.hosp+theme(legend.position = "none"), ncol = 2, widths=c(20,20))
forest<-grid.arrange(fore,leg, nrow=2, heights=c(20,1))

ggsave(forest, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.hist.claimsflu.png"),
       dpi=300,
       width = 12, height = 6)


# comorbidities -------------
conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","conditions_hypertension.csv"), header = FALSE, sep='\t')%>%
  mutate(id = row_number())%>%rename(covariate_name = V1)%>% 
  filter(!str_detect(covariate_name, "age group|drug"),str_detect(covariate_name, "-365 through -1"))%>% 
  mutate(covariate_name=as.factor(covariate_name))
                                         
plot.data <- data.clean.covid %>% 
  filter(value >=0,
         tolower(trimws(covariate_name))%in%tolower(trimws(conditions$covariate_name)))%>% 
  separate(covariate_name, into=c(NA, "Type"),remove=FALSE,sep=": ")%>%
  mutate(Type=recode(trimws(Type), 
                     'Prevalent heart disease'='Heart disease',
                     'Prevalent obesity'='Obesity',
                     'Prevalent hypertension' = 'Hypertension',
                     'Prevalent malignant neoplasm excluding non-melanoma skin cancer'='Cancer',
                     'Prevalent Asthma without COPD'='Asthma',
                     "Prevalent Asthma or Chronic obstructive pulmonary disease (COPD)"= "Asthma or COPD",
                     "Prevalent chronic obstructive pulmonary disease (COPD) without asthma"= "COPD",
                     "Incident depression with no prior treatment and no mania/psychoses"="Depression",
                     'Hospitalization episodes'= 'Hospitalisation episodes',
                     'intensive services during hospitalization'='Intensive services',
                     'Sepsis during hospitalization'='Sepsis',
                     'Acute Respiratory Distress syndrome (ARDS) during hospitalization'='Acute respiratory distress syndrome (ARDS)',
                     'Cardiac arrhythmia during hospitalization'= 'Cardiac arrhythmia',
                     'Bleeding during hospitalization'='Bleeding',
                     'death'='Death',
                     'Pneumonia during hospitalization'='Pneumonia',
                     'Malignant neoplasm excluding non-melanoma skin cancer'='Cancer',
                     'Multi-system inflammatory syndrome (Kawasaki disease or toxic shock syndrome)'='Multi-system inflammatory syndrome \n (Kawasaki disease or toxic shock syndrome)',
                     "Prevalent Autoimmune condition"="Autoimmune condition",
                     "Prevalent chronic kidney disease broad"="Chronic kidney disease broad",
                     "Prevalent Dementia"="Dementia",
                     "Prevalent Human immunodeficiency virus infection"="HIV",
                     "Prevalent Type 2 Diabetes Mellitus"="Type 2 Diabetes Mellitus"
                     
  )) %>% 
  filter(!Type%in%c("HIV", "Autoimmune condition","Pregnant women", "Anxiety", "Idiopathic pulmonary fibrosis","Anemia"))
#barplot y faceting

#primary care
#hospit
to.plot <- plot.data %>% filter(base%in%databases_primary) %>% arrange(value) %>% mutate(Type=as.factor(Type))
factors<-to.plot%>%filter(base=="SIDIAP", hospit=="Diagnosed",hyper=="With Hypertension") %>% arrange(value)
to.plot$Type<-factor(to.plot$Type, levels(reorder(factors$Type, factors$value)))
to.plot <- to.plot %>% mutate(variable=paste(hyper, hospit))


g.comor3<-ggplot(to.plot,aes(x=Type,fill=hyper,y=value*100))+
  geom_col(data=to.plot%>%filter(hyper=="Without Hypertension"),size = 2, alpha=.8) +
  geom_col(data=to.plot%>%filter(hyper=="With Hypertension"),size = 2,alpha=.8,width=0.6) +
  
  #geom_col(data=to.plot%>%filter(hyper=="With Hypertension", hospit=="Hospitalized"),size = 2,alpha=.8,width=0.6) +
  #geom_hline(yintercept = 0,colour="black", linetype="dashed")+
  #facet_wrap(.~base, ncol=2)+
  facet_grid(hospit~base)+
  theme(panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=14),
        strip.text.x = element_text(size=9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  )+
  
  scale_y_continuous(expand = c(0, 0.3),limits = c(0,62))+
  coord_flip()+
  ylab("%")+
  xlab(NULL)

g.comor3
ggsave(g.comor3,file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "comorbidities.primary.png"),
       dpi=300,
       width = 11, height = 7)
#hospit
to.plot <- plot.data %>% filter(base%in%databases_hospit) %>% arrange(value) %>% mutate(Type=as.factor(Type))
factors<-to.plot%>%filter(base=="STARR-OMOP", hospit=="Diagnosed",hyper=="With Hypertension") %>% arrange(value)
to.plot$Type<-factor(to.plot$Type, levels(reorder(factors$Type, factors$value)))
to.plot <- to.plot %>% mutate(variable=paste(hyper, hospit))


g.comor3<-ggplot(to.plot,aes(x=Type,fill=hyper,y=value*100))+
  geom_col(data=to.plot%>%filter(hyper=="Without Hypertension"),size = 2, alpha=.8) +
  geom_col(data=to.plot%>%filter(hyper=="With Hypertension"),size = 2,alpha=.8,width=0.6) +

  #geom_col(data=to.plot%>%filter(hyper=="With Hypertension", hospit=="Hospitalized"),size = 2,alpha=.8,width=0.6) +
  #geom_hline(yintercept = 0,colour="black", linetype="dashed")+
  #facet_wrap(.~base, ncol=2)+
  facet_grid(hospit~base)+
  theme(panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=14),
        strip.text.x = element_text(size=9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  )+
  
 scale_y_continuous(expand = c(0, 0.3),limits = c(0,76))+
  coord_flip()+
  ylab("%")+
  xlab(NULL)

g.comor3
ggsave(g.comor3,file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.comorbidities.hospit.png"),
       dpi=300,
       width = 10, height = 7)

# claims

#hospit
to.plot <- plot.data %>% filter(base%in%databases_claims) %>% arrange(value) %>% mutate(Type=as.factor(Type))
factors<-to.plot%>%filter(base=="IQVIA-OpenClaims", hospit=="Diagnosed",hyper=="With Hypertension") %>% arrange(value)
to.plot$Type<-factor(to.plot$Type, levels(reorder(factors$Type, factors$value)))
to.plot <- to.plot %>% mutate(variable=paste(hyper, hospit))


g.comor3<-ggplot(to.plot,aes(x=Type,fill=hyper,y=value*100))+
  geom_col(data=to.plot%>%filter(hyper=="Without Hypertension"),size = 2, alpha=.8) +
  geom_col(data=to.plot%>%filter(hyper=="With Hypertension"),size = 2,alpha=.8,width=0.6) +
  
  #geom_col(data=to.plot%>%filter(hyper=="With Hypertension", hospit=="Hospitalized"),size = 2,alpha=.8,width=0.6) +
  #geom_hline(yintercept = 0,colour="black", linetype="dashed")+
  #facet_wrap(.~base, ncol=2)+
  facet_grid(hospit~base)+
  theme(panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=14),
        strip.text.x = element_text(size=9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  )+
  
  scale_y_continuous(expand = c(0, 0.3),limits = c(0,76))+
  coord_flip()+
  ylab("%")+
  xlab(NULL)

g.comor3
ggsave(g.comor3,file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "new.comorbidities.claims.png"),
       dpi=300,
       width = 10, height = 7)


## table outcomes -----------------
conditions<- read.csv(paste0("//epofs/apistillo/CHARYBDIS/Conditions/","conditions_hypertension.csv"), header = FALSE, sep='\t')%>%
  mutate(id = row_number())%>%rename(covariate_name = V1)%>% 
  filter(!str_detect(covariate_name, "age group|rug"),str_detect(covariate_name, "0"))%>% 
  mutate(covariate_name=as.factor(covariate_name))
plot.data <- data.clean.covid %>% 
  filter(value >=0,
         tolower(trimws(covariate_name))%in%tolower(trimws(conditions$covariate_name)))%>% 
  separate(covariate_name, into=c(NA, "Type"),remove=FALSE,sep=": ")%>%
  mutate(Type=recode(trimws(Type), 
                     'Prevalent heart disease'='Heart disease',
                     'Prevalent obesity'='Obesity',
                     'Prevalent hypertension' = 'Hypertension',
                     'Prevalent malignant neoplasm excluding non-melanoma skin cancer'='Cancer',
                     'Prevalent Asthma without COPD'='Asthma',
                     "Prevalent Asthma or Chronic obstructive pulmonary disease (COPD)"= "Asthma or COPD",
                     "Prevalent chronic obstructive pulmonary disease (COPD) without asthma"= "COPD",
                     "Incident depression with no prior treatment and no mania/psychoses"="Depression",
                     'Hospitalization episodes'= 'Hospitalisation episodes',
                     'intensive services during hospitalization'='Intensive services',
                     'Sepsis during hospitalization'='Sepsis',
                     'Acute Respiratory Distress syndrome (ARDS) during hospitalization'='Acute respiratory distress syndrome (ARDS)',
                     'Cardiac arrhythmia during hospitalization'= 'Cardiac arrhythmia',
                     'Bleeding during hospitalization'='Bleeding',
                     'death'='Death',
                     'Pneumonia during hospitalization'='Pneumonia',
                     'Malignant neoplasm excluding non-melanoma skin cancer'='Cancer',
                     'Multi-system inflammatory syndrome (Kawasaki disease or toxic shock syndrome)'='Multi-system inflammatory syndrome \n (Kawasaki disease or toxic shock syndrome)',
                     "Prevalent Autoimmune condition"="Autoimmune condition",
                     "Prevalent chronic kidney disease broad"="Chronic kidney disease broad",
                     "Prevalent Dementia"="Dementia",
                     "Prevalent Human immunodeficiency virus infection"="HIV",
                     "Prevalent Type 2 Diabetes Mellitus"="Type 2 Diabetes Mellitus"
                     
  ))
to.plot<-plot.data%>%filter( disease=="covid", !hospit=="ICU", value>=0)%>%
  mutate( value=ifelse((hospit=="Hospitalized"& Type==c("Hospitalization episodes")), NA, value),
          value=ifelse((hospit=="Diagnosed"& base=="HIRA"), NA, value)) %>% 
  mutate(value=(paste0(format(value*100,nsmall=1)," (", format(value*100-round(100*1.96*sqrt((value*(1-value))/n),1),nsmall=1),
                      "-",format(value*100+round(100*1.96*sqrt((value*(1-value))/n),1),nsmall=1),")"))) %>% 
  select(base, hospit, hyper,Type,  value) %>% 
  pivot_wider(values_from = "value", names_from="hyper")
  
primary<- to.plot %>% filter(base%in%databases_primary) %>% 
  pivot_wider(values_from=c(4,5), names_from="base") %>% 
  select(1,2,contains(databases_primary))
write.table(primary,paste0("//epofs/apistillo/CHARYBDIS/Hypertension/","outcome.primary.csv"), sep = ";", row.names = F, dec = ",",na="-")
hospit<- to.plot %>% filter(base%in%databases_hospit) %>% 
  pivot_wider(values_from=c(4,5), names_from="base") %>% 
  select(1,2,contains(databases_hospit))
write.table(hospit,paste0("//epofs/apistillo/CHARYBDIS/Hypertension/","outcome.hospit.csv"), sep = ";", row.names = F, dec = ",",na="-")
claims<- to.plot %>% filter(base%in%databases_claims) %>% 
  pivot_wider(values_from=c(4,5), names_from="base") %>% 
  select(1,2,contains(databases_claims))
write.table(claims,paste0("//epofs/apistillo/CHARYBDIS/Hypertension/","outcome.claims.csv"), sep = ";", row.names = F, dec = ",",na="-")

#---------


path_out="//epofs/apistillo/CHARYBDIS/Hypertension/"
results.folder <- "//epofs/apistillo/CHARYBDIS/SavedData/"
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
conditions<- read.csv(paste0(path_cond,"conditions_hypertension.csv"), header = FALSE, sep='\t')%>%mutate(id = row_number())%>%rename(covariate_name = V1)


load(paste0(results.folder, "hypertension.15.10.RDa"), envir = globalenv())
merged<-table


merged<-merged[match(tolower(conditions$covariate_name), tolower(merged$covariate_name)),]

merged_pretty<-merged%>%
  filter(!str_detect(covariate_name, "drug"))%>%
  select(covariate_name,contains("Diagnosed"), contains("Hospitalized"))%>%
  separate(covariate_name, into=c(NA, "Type"), sep = ": ", remove = FALSE)%>%
  select(-contains("SMD"),-contains("HIRA_2020"))



merged_pretty<-merged_pretty%>%mutate(Type=recode(trimws(Type), 
                                                  'Prevalent heart disease'='Heart disease',
                                                  'Prevalent obesity'='Obesity',
                                                  'Prevalent hypertension' = 'Hypertension',
                                                  'Prevalent malignant neoplasm excluding non-melanoma skin cancer'='Cancer',
                                                  'Prevalent Asthma without COPD'='Asthma',
                                                  "Prevalent Asthma or Chronic obstructive pulmonary disease (COPD)"= "Asthma or COPD",
                                                  "Prevalent chronic obstructive pulmonary disease (COPD) without asthma"= "COPD",
                                                  "Incident depression with no prior treatment and no mania/psychoses"="Depression",
                                                  'Hospitalization episodes'= 'Hospitalisation episodes',
                                                  'intensive services during hospitalization'='Intensive services',
                                                  'Sepsis during hospitalization'='Sepsis',
                                                  'Acute Respiratory Distress syndrome (ARDS) during hospitalization'='Acute respiratory distress syndrome (ARDS)',
                                                  'Cardiac arrhythmia during hospitalization'= 'Cardiac arrhythmia',
                                                  'Bleeding during hospitalization'='Bleeding',
                                                  'death'='Death',
                                                  'Pneumonia during hospitalization'='Pneumonia',
                                                  'Malignant neoplasm excluding non-melanoma skin cancer'='Cancer',
                                                  'Multi-system inflammatory syndrome (Kawasaki disease or toxic shock syndrome)'='Multi-system inflammatory syndrome \n (Kawasaki disease or toxic shock syndrome)',
                                                  "Prevalent Autoimmune condition"="Autoimmune condition",
                                                  "Prevalent chronic kidney disease broad"="Chronic kidney disease broad",
                                                  "Prevalent Dementia"="Dementia",
                                                  "Prevalent Human immunodeficiency virus infection"="HIV",
                                                  "Prevalent Type 2 Diabetes Mellitus"="Type 2 Diabetes Mellitus"
                                                  
))

data<-merged_pretty%>%select(-ends_with("sd"))
diab<-merged_pretty%>%select(-contains("No_Hypertension"))
nodiab<-merged_pretty%>%select(1,2,contains("No_Hypertension"))

data1<-diab%>%pivot_longer(cols=3:ncol(diab), names_to=c("base", "diab","hospit", "disease", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo),diab="With Hypertension")%>%distinct(across(1:7), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")


data2<-nodiab%>%pivot_longer(cols=3:ncol(nodiab), names_to=c("base", "no","diab", "hospit", "disease", "tipo"), names_sep="_")%>%select(-no)%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo), diab="Without Hypertension")%>%distinct(across(1:7), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")
plot.data<-rbind(data1, data2)
plot.data<-plot.data%>%mutate(base=recode(base, "UCHealth-OMOP"="CU-AMC-HDC"))

save(plot.data,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "hypertension.data.Rda"))
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "hypertension.data.Rda"))



##################
## demographic hospitalized
##################

bases.hosp<-c("CU-AMC-HDC",
              "CUIMC",
              "HEALTHVERITY",
              "HIRA",
              "IQVIA-OpenClaims",
              "OptumEhr",
              "SIDIAP",
              "STARR-OMOP",
              "UWM-CRD",
              "VA-OMOP")

plot.data1<-na.omit(plot.data)%>%
  filter(str_detect(covariate_name, "age group"),
         mean>=0,
         disease=="covid",
         base%in%bases.hosp,
         !(base=="HIRA"&hospit=="Diagnosed"))%>%
  select(-cohort_name)

# plot.data1<-plot.data1%>%mutate(base=factor(base, levels=c("CUIMC",
#                                                            "HEALTHVERITY",
#                                                            "HIRA",
#                                                            "IQVIA-OpenClaims",
#                                                            "OptumEhr",
#                                                            "SIDIAP",
#                                                            "CU-AMC-HDC",
#                                                            "UWM-CRD",
#                                                            "VA-OMOP")), base=recode(base, 
#                                                                                     "SIDIAP"="SIDIAP (ES)",
#                                                                                     "CUIMC" ="CUIMC (US)",
#                                                                                     "IQVIA-OpenClaims"="IQVIA-OpenClaims (US)",
#                                                                                     "OptumEhr"="OPTUM EHR (US)",
#                                                                                     "VA-OMOP"="VA-OMOP (US)",
#                                                                                     "HEALTHVERITY"="HEALTHVERITY (US)",
#                                                                                     "HIRA"="HIRA (SK)",
#                                                                                     "CU-AMC-HDC"="CU-AMC-HDC (US)",
#                                                                                     "UWM-CRD"="UWM-CRD (US)"
#                                                                                     
#                                                            ))
# 

plot.data1$Type<-fct_rev(plot.data1$Type)
plot.data1<-plot.data1%>%mutate(n=prettyNum(n, big.mark=",", nsmall = 0, digits=0,scientific = FALSE))

ggplot(plot.data1, aes(Type, fill=diab))+#aes(factor(covariate_name, levels=order)))+
  facet_wrap(.~ base, nrow=4 )+
  
  geom_bar(data=filter(plot.data1, diab=="With Hypertension",hospit=="Diagnosed"),aes(y=-mean*100), stat="identity")+
  geom_bar(data=filter(plot.data1, diab=="Without Hypertension", hospit=="Diagnosed"),aes(y=-mean*100), stat="identity", width=0.4)+
  
  geom_bar(data=filter(plot.data1, diab=="With Hypertension",hospit=="Hospitalized"),aes(y=mean*100), stat="identity")+
  geom_bar(data=filter(plot.data1, diab=="Without Hypertension", hospit=="Hospitalized"),aes(y=mean*100), stat="identity", width=0.4)+
  
  geom_hline(yintercept = 0, colour = "grey20", linetype="dashed") +
  theme(legend.position="bottom",
        legend.key.height = unit(0.05, "cm"),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=14),
        legend.title = element_blank(),
        strip.text = element_text(size=14),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5) )+
  scale_y_continuous(breaks = c(-50,-40,-30,-20,-10, 0,10, 20 , 30,40,50),
                     labels = c("50%","40%", "30%", "20%","10%", "0%","10%", "20%", "30%","40%","50%"), lim=c(-55, 55))+
  # scale_fill_manual(breaks=c("COVID-19","Influenza"),
  #                   values=c("COVID-19"="#F8766D", "Influenza"="#00BA38"))+
  # 
  xlab(NULL)+
  ylab(NULL)+
  coord_flip()+
  geom_label(data=inner_join(filter(plot.data1,diab=="With Hypertension",hospit=="Diagnosed"),
                             filter(plot.data1,diab=="Without Hypertension",hospit=="Diagnosed"),
                             by=c("covariate_name", "Type", "base", "hospit", "disease","N")),
             mapping = aes(x = 16, y = -40, label = paste0("Diagnosed\nHypertension,\nN = ",n.x, "\nNo Hypertension,\nN = ", n.y), fill=NULL), 
             size=4,
             show.legend=FALSE, inherit.aes=T)+
  
  geom_label(data=inner_join(filter(plot.data1,diab=="With Hypertension",hospit=="Hospitalized"),
                             filter(plot.data1,diab=="Without Hypertension",hospit=="Hospitalized"),
                             by=c("covariate_name", "Type", "base", "hospit", "disease","N")),
             mapping = aes(x = 16, y = 40, label = paste0("Hospitalized\nHypertension,\nN = ",n.x, "\nNo Hypertension,\nN = ", n.y), fill=NULL), 
             size=4,
             show.legend=FALSE, inherit.aes=T)

ggsave(paste0(path_out,"demographic.hospit.png"),
       dpi=300,
       width = 12.5, height = 15)


########
#age histograms
###
library(gridExtra)

# to.plot<-plot.data%>%separate(covariate_name, into=c(NA, "type"), sep=": ")%>%
#   filter(disease=="covid", !hospit=="ICU", base%in%bases, mean>=0)%>%
#   mutate(type=factor(type), base=factor(base, levels=bases))%>%
#   mutate(n = ifelse(n>=10000, prettyNum(n, big.mark=" ", nsmall = 0, digits=0,scientific = FALSE), n))



data<-inner_join(filter(plot.data%>%filter(disease=="covid"),diab=="With Hypertension"),
                 filter(plot.data%>%filter(disease=="covid"),diab=="Without Hypertension"),
                 by=c("Type", "base", "hospit", "disease"))%>%
  mutate(var=paste0(base, "\nHypertension, N = ",
                    prettyNum(n.x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE),
                    ", No Hypertension, N = ",
                    prettyNum(n.y, big.mark=",", nsmall = 0, digits=0,scientific = FALSE)))%>%
  select(-contains("x"), -contains("y"))%>%unique()

to.plot<-left_join(plot.data%>%filter(disease=="covid"),data)%>%
  filter(str_detect(covariate_name,"age group"),
         mean>=0,
         !(base=="HIRA"&hospit=="Diagnosed"))%>%
  select(-cohort_name,-N,-covariate_name)%>%
  rbind(tibble(Type=NA,base="CPRD", diab="With Hypertension",hospit="Hospitalized", disease="covid",mean=0, sd=0,  n=0, var="CPRD"),
        tibble(Type=NA,base="LPDItaly", diab="With Hypertension",hospit="Hospitalized", disease="covid",mean=0, sd=0,  n=0, var="LPDItaly"),
        tibble(Type=NA,base="TRDW", diab="With Hypertension",hospit="Hospitalized", disease="covid",mean=0, sd=0,  n=0, var="TRDW"),
        tibble(Type=NA,base="IQVIA-DAGermany", diab="With Hypertension",hospit="Hospitalized", disease="covid",mean=0, sd=0,  n=0, var="IQVIA-DAGermany"),
        tibble(Type=NA,base="IQVIA-LPDFrance", diab="With Hypertension",hospit="Hospitalized", disease="covid",mean=0, sd=0,  n=0, var="IQVIA-LPDFrance"),
        tibble(Type=NA,base="IPCI", diab="With Hypertension",hospit="Hospitalized", disease="covid",mean=0, sd=0,  n=0, var="IPCI"),
        tibble(Type=NA,base="HIRA", diab="With Hypertension",hospit="Diagnosed", disease="covid",mean=0, sd=0,  n=0, var="HIRA"))



to.plot.d<-to.plot%>%filter(hospit=="Diagnosed")
p.diag<-ggplot(to.plot.d, aes(x=Type, y=mean*100, fill=diab))+
  facet_wrap(~ base , nrow=16, dir="v", drop=FALSE)+#, labeller=labeller(base=setNames(to.plot.d$var,to.plot.d$base)))+
  geom_bar(data=to.plot.d%>%filter(diab=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.d%>%filter(diab=="With Hypertension"), width=0.4,stat="identity")+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.background = element_blank(),
        text = element_text(size=13),
        strip.text = element_text(face = "bold"),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))+
  # scale_fill_manual(breaks=c("With Obesity","Without Obesity"),
  #                   values=c("With Obesity"="#FF1E00", "Without Obesity"="#0776A0"))+
  coord_cartesian(ylim = c(0,30))+
  scale_x_discrete(breaks=c("05-09","25-29","45-49","65-69","85-89"), labels=c("05-09","25-29","45-49","65-69","85-89"))+
  ggtitle("Diagnosed")+
  ylab("%")+
  xlab("Age")
p.diag

to.plot.h<-to.plot%>%filter(hospit=="Hospitalized")
p.hosp<-ggplot(to.plot.h, aes(x=Type, y=mean*100, fill=diab))+
  facet_wrap(~ base, nrow=16, dir="v", drop=FALSE)+#,labeller=labeller(base=setNames(to.plot.h$var,to.plot.h$base)))+
  geom_bar(data=to.plot.h%>%filter(diab=="Without Hypertension"), stat="identity")+
  geom_bar(data=to.plot.h%>%filter(diab=="With Hypertension"), width=0.4,stat="identity")+ 
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.position = "right",
        #axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.text = element_text(face = "bold"),
        text = element_text(size=13),
        panel.background = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))+
  coord_cartesian(ylim = c(0,30))+
  scale_x_discrete(breaks=c("05-09","25-29","45-49","65-69","85-89"), labels=c("05-09","25-29","45-49","65-69","85-89"))+
  ggtitle("Hospitalized")+
  ylab(NULL)+
  xlab("Age")
p.hosp

forest<-grid.arrange(p.diag+theme(legend.position = "none"),p.hosp+theme(legend.position = "none"),leg, ncol = 3, widths=c(10,10,5))

ggsave(forest, file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "histogram.age.v2.png"),
       dpi=300,
       width = 10, height = 15)





leg<-get_legend(p.hosp)

#######
# comorbidities
######

to.plot<-plot.data%>%filter(disease=="covid")%>%
  filter(!str_detect(covariate_name,"age group|0 through 30"),
         mean>=0,
         !(base=="HIRA"&hospit=="Diagnosed"),
         !base=="SIDIAP-H",
         !base%in%c("Premier","UWM-CRD","DCMC"))%>%
  select(-covariate_name)%>%
  na.omit()

reorder<-(to.plot%>%filter(base=="VA-OMOP", hospit=="Diagnosed", diab=="With Hypertension")%>%arrange(mean))$Type

to.plot<-to.plot%>%
  mutate(Type=factor(Type,levels=reorder))%>%
  rename(Database=base)

g.comor<-ggplot(to.plot,aes(x=Type))+
  geom_segment(data=to.plot%>%filter(diab=="With Hypertension"),aes(x = Type, y = 0, xend = Type, yend = mean*100, col=Database)) +
  geom_point(data=to.plot%>%filter(diab=="With Hypertension"),size = 2, aes(y=mean*100, col=Database,shape=Database),alpha=.8) +
  
  geom_segment(data=to.plot%>%filter(diab=="Without Hypertension"),aes(x = Type, y = 0, xend = Type, yend = -mean*100, col=Database)) +
  geom_point(data=to.plot%>%filter(diab=="Without Hypertension"),size = 2, aes(y=-mean*100, col=Database,shape=Database),alpha=.8) +
  geom_hline(yintercept = 0,colour="black", linetype="dashed")+
  facet_wrap(hospit~.,nrow=2)+
  theme(panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=16),
        
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  )+
  scale_shape_manual(values=65:80)+
  ylim(c(-76,76))+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  geom_label(mapping = aes(x = 5, y = -50, label = "Without Hypertension", fill=NULL), 
             size=4,
             show.legend=FALSE, inherit.aes=T)+
  
  geom_label(mapping = aes(x = 5, y = 50, label = "With Hypertension", fill=NULL), 
             size=4,
             show.legend=FALSE, inherit.aes=T)

# scale_y_continuous(breaks = c(-30,-20,-10, 0,10, 20 , 30),
#                    labels = c( "30%", "20%","10%", "0%","10%", "20%", "30%"), lim=c(-30, 30))+
g.comor
ggsave(g.comor,file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "comorbidities.v1.png"),
       dpi=300,
       width = 10, height = 12)


#or grouping by comorbidity
g.comor2<-ggplot(to.plot,aes(x=Type))+
  geom_linerange(data=to.plot%>%filter(diab=="With Hypertension"),aes(xmin = Type, ymin = 0, xmax = Type, ymax = mean*100, col=Database), position= position_dodge(width=0.8),alpha=.7) +
  geom_point(data=to.plot%>%filter(diab=="With Hypertension"),aes(fill=Database,col=Database, y = mean*100, shape=Database),stat="identity", position_dodge(width=0.8),shape=21,alpha=.7) + 
  geom_linerange(data=to.plot%>%filter(diab=="Without Hypertension"),aes(xmin = Type, ymin = 0, xmax = Type, ymax = -mean*100, col=Database) ,position=position_dodge(width=0.8),alpha=.7) +
  geom_point(data=to.plot%>%filter(diab=="Without Hypertension"),aes(fill=Database,col=Database,  y = -mean*100, shape=Database),stat="identity", position_dodge(width=0.8),shape=21,alpha=.7) + 
  
  geom_hline(yintercept = 0,colour="black", linetype="dashed")+
  facet_wrap(hospit~.,nrow=2)+
  
  theme(panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=14),
        legend.position = "top",
        legend.text = element_text(size=10),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  )+
  scale_shape_manual(values=1:16)+
  ylim(c(-76,76))+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)+
  geom_label(mapping = aes(x = 5, y = -50, label = "Without Hypertension", fill=NULL), 
             size=4,
             show.legend=FALSE, inherit.aes=T)+
  
  geom_label(mapping = aes(x = 5, y = 50, label = "With Hypertension", fill=NULL), 
             size=4,
             show.legend=FALSE, inherit.aes=T)

g.comor2

ggsave(g.comor2,file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "comorbidities.v2.png"),
       dpi=300,
       width = 10, height = 14)

#barplot y faceting
g.comor3<-ggplot(to.plot,aes(x=Type,fill=diab,y=mean*100))+
  geom_col(data=to.plot%>%filter(diab=="Without Hypertension"),size = 2, alpha=.8) +
  
  geom_col(data=to.plot%>%filter(diab=="With Hypertension"),size = 2,alpha=.8,width=0.6) +
  geom_hline(yintercept = 0,colour="black", linetype="dashed")+
  facet_wrap(.~Database+hospit,ncol=4)+
  theme(panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        text=element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5)  )+
  scale_y_continuous(expand = c(0, 0.3),limits = c(0,76))+
  coord_flip()+
  xlab(NULL)+
  ylab(NULL)

g.comor3
ggsave(g.comor3,file=paste0("//epofs/apistillo/CHARYBDIS/Hypertension/", "comorbidities.v3.png"),
       dpi=300,
       width = 9, height = 16)
###
# data scatter:
###
data <- left_join(data1, data2, by=c("covariate_name","Type","base", "hospit", "disease"))%>%
  mutate(smd=((abs(mean.x)-abs(mean.y))/(sqrt(sd.x^2+sd.y^2))), smd_type="diabVSnodiab")%>%
  select(-diab.x, -diab.y)

toplot.diagvshosp<-inner_join(data1%>%filter(hospit=="Diagnosed"),data1%>%filter(hospit=="Hospitalized"), 
                              by=c("covariate_name", "Type" ,  "base" ,"diab" ,"disease"  ))%>%
  mutate(smd=((abs(mean.x)-abs(mean.y))/(sqrt(sd.x^2+sd.y^2))), smd_type="diagVShospit")%>%
  select(-hospit.x, -hospit.y)


diab<-data%>%filter(tipo=="T2")%>%rename(diab=value)%>%select(-tipo)
nodiab<-data%>%filter(tipo=="No_T2")%>%rename(nodiab=value)%>%select(-tipo)
smd<-data%>%filter(tipo=="SMD_T2")%>%rename(smd=value)%>%select(-tipo)
data2<-left_join(diab, nodiab)%>%
  left_join(smd)

data2<-data2%>%
  filter(abs(smd)>0.3)%>%
  filter(abs(smd)<1)

data2<-data2%>%filter(hospit=="Diagnosed")%>%
  filter(!str_detect(covariate_name, "age group"))%>%
  filter(!base=="Premier")


### scatterplot
bases<-c(
  "CUIMC",
  "HEALTHVERITY",
  "HIRA",
  "IQVIA-OpenClaims",
  "OptumEhr",
  "SIDIAP",
  "UCHealth-OMOP",
  "UWM-CRD",
  "VA-OMOP")

remove <-c("age group","gender =","Cohort during day 0 through 30 days start the index", "episodes", 
           "Prevalent pre-existing condition of COVID risk factor", "events",
           "Pregnant women", "Prevalent Human immunodeficiency", "Low back pain",
           "Osteoarthritis of hip", "Osteoarthritis of knee", "Viral hepatitis","Dependence on supplemental oxygen", 
           "Idiopathic pulmonary fibrosis")
data2<-data%>%
  filter(abs(smd)>0.1,
         base%in%bases, 
         disease=="covid",
         hospit=="Diagnosed",
         !(base=="HIRA"&hospit=="Diagnosed"),
         !(base=="STARR-OMOP"&hospit=="Hospitalized"),
         !base%in%c("Premier","HM-Hospitales", "SIDIAP-H"))%>%
  filter(!str_detect(covariate_name, paste0(remove, collapse="|")))

data2<-data2%>%mutate(base=recode(base, 
                                  "SIDIAP"="SIDIAP (ES)",
                                  "CUIMC" ="CUIMC (US)",
                                  "IQVIA-OpenClaims"="IQVIA-OpenClaims (US)",
                                  "OptumEhr"="OPTUM EHR (US)",
                                  "VA-OMOP"="VA-OMOP (US)",
                                  "HEALTHVERITY"="HEALTHVERITY (US)",
                                  "HIRA"="HIRA (SK)",
                                  "UCHealth-OMOP"="UCHealth-OMOP (US)",
                                  "UWM-CRD"="UWM-CRD (US)"
                                  
))

ggplot(data2, aes(y = mean.x*100, x=mean.y*100, label = Type))+ 
  geom_point(aes(fill = smd), colour="grey",shape=21, size=2,alpha=.8)+
  xlim(-20, 95) +
  ylim(-20, 95) +
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), panel.background = element_blank(), 
         axis.line = element_line(colour = "black")) +
  facet_wrap(~base, nrow = 3)+
  theme(legend.position = c(.95,0.2), 
        legend.box="vertical", legend.title = element_text(size=14, color = "black", face="bold"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        legend.key = element_blank(),
        legend.background = element_rect(fill = NULL, color=NULL),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))+
  geom_abline(intercept = 0, slope = 1,
              color = "gray", size = 0.5, alpha = 0.8)+
  
  labs(col="SMD")+
  ggtitle("") + 
  ylab("Diagnosed COVID-19 with Diabetes(%)")+
  xlab("Diagnosed COVID-19 without Diabetes(%)")+
  geom_text_repel(size=2,position ='jitter', force=10, segment.alpha = .8, segment.colour="grey")+
  scale_fill_viridis(option="D")+#, limits = c(0.09, 0.41))+
  guides(fill=guide_colourbar(order = 1), size = guide_legend(order = 2))

ggsave(paste0(path_out,"scatter_covid_sino.png"),
       dpi=300,
       width = 12, height = 12)

data_sd<-merged_pretty%>%select(1,2,ends_with("sd"))

data_sd<-data_sd%>%pivot_longer(cols=3:ncol(data_sd), names_to=c("tipo", "resto"), names_sep=" ")%>%
  separate(resto, into=c("diab", "base", "hospit"), sep="_")%>%select(-diab)%>%
  unique()





bases<-c(
  "CUIMC",
  "HEALTHVERITY",
  "HIRA",
  "IQVIA-OpenClaims",
  "OptumEhr",
  "SIDIAP",
  "UCHealth-OMOP",
  "UWM-CRD",
  "VA-OMOP")

remove <-c("age group","gender =","Cohort during day 0 through 30 days start the index", "episodes", 
           "Prevalent pre-existing condition of COVID risk factor", "events",
           "Pregnant women", "Prevalent Human immunodeficiency", "Low back pain",
           "Osteoarthritis of hip", "Osteoarthritis of knee", "Viral hepatitis","Dependence on supplemental oxygen", 
           "Idiopathic pulmonary fibrosis")
data2<-toplot.diagvshosp%>%
  filter(abs(smd)>0.1,
         base%in%bases, 
         disease=="covid",
         !base%in%c("Premier","HM-Hospitales", "SIDIAP-H"))%>%
  filter(!str_detect(covariate_name, paste0(remove, collapse="|")))

data2<-data2%>%mutate(base=recode(base, 
                                  "SIDIAP"="SIDIAP (ES)",
                                  "CUIMC" ="CUIMC (US)",
                                  "IQVIA-OpenClaims"="IQVIA-OpenClaims (US)",
                                  "OptumEhr"="OPTUM EHR (US)",
                                  "VA-OMOP"="VA-OMOP (US)",
                                  "HEALTHVERITY"="HEALTHVERITY (US)",
                                  "HIRA"="HIRA (SK)",
                                  "UCHealth-OMOP"="UCHealth-OMOP (US)",
                                  "UWM-CRD"="UWM-CRD (US)"))



ggplot(data2, aes(x = mean.x*100, y=mean.y*100, label = Type))+ 
  geom_point(alpha = 1, aes(color = -smd),position = "jitter", size=2)+
  xlim(0, 97) +
  ylim(0, 97) +
  theme( panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), panel.background = element_blank(), 
         axis.line = element_line(colour = "black")) +
  facet_wrap(~base, nrow = 2)+
  theme(legend.position = c(.95,0.2), 
        legend.box="vertical", legend.title = element_text(size=14, color = "black", face="bold"),
        legend.text = element_text(colour="black", size = 12, face = "bold"),
        legend.key = element_blank(),
        text = element_text(size=14),
        legend.background = element_rect(fill = NULL, color=NULL),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5))+
  geom_abline(intercept = 0, slope = 1,
              color = "gray", size = 0.5, alpha = 0.8)+
  labs(col="SMD")+
  ggtitle("") + 
  xlab("Diagnosed COVID-19 with Diabetes(%)")+
  ylab("Hospitalized COVID-19 with Diabetes(%)")+
  geom_text_repel(size=3,position ='jitter', force=4, segment.colour = "grey")+
  scale_colour_viridis(option="D")+#, limits = c(0.09, 0.41))+
  guides(colour=guide_colourbar(order = 1), size = guide_legend(order = 2))

ggsave(paste0(path_out,"scatter_covid_DIAGHOSP.png"),
       dpi=300,
       width = 10, height = 7)


##################
## demographic without hospitalization
##################

plot.data1<-data%>%
  filter(str_detect(covariate_name, "age group"),
         mean>=0,
         !base%in%c(
           "CUIMC",
           "HEALTHVERITY",
           "HIRA",
           "IQVIA-OpenClaims",
           "OptumEhr",
           "SIDIAP",
           "CU-AMC-HDC",
           "UWM-CRD",
           "VA-OMOP"), 
         disease=="covid",
         !(base=="HIRA"&hospit=="Diagnosed"),
         !(base=="STARR-OMOP"&hospit=="Hospitalized"),
         !base%in%c("Premier","HM-Hospitales", "SIDIAP-H", "Nanfang", "DCMC"))

plot.data1<-plot.data1%>%mutate(base=recode(base, 
                                            "CPRD"="CPRD (UK)",
                                            "IPCI" ="IPCI (NL)",
                                            "IQVIA-DAGermany"="IQVIA-DAGermany (DE)",
                                            "IQVIA-LPDFrance"="IQVIA-LPDFrance (FR)",
                                            "LPDItaly"="IQVIA-LPDItaly (IT)",
                                            "STARR-OMOP"="STARR-OMOP (US)",
                                            "TRDW"="TRDW (US)"
                                            
))

plot.data1$Type<-factor(plot.data1$Type)
plot.data1<-plot.data1%>%mutate(n=prettyNum(n, big.mark=",", nsmall = 0, digits=0,scientific = FALSE))

ggplot(plot.data1, aes(Type, fill=diab))+#aes(factor(covariate_name, levels=order)))+
  facet_wrap(.~ base, nrow=3 )+
  
  geom_bar(data=filter(plot.data1, diab=="With Hypertension",hospit=="Diagnosed"),aes(y=mean*100), stat="identity")+
  geom_bar(data=filter(plot.data1, diab=="Without Hypertension", hospit=="Diagnosed"),aes(y=mean*100), stat="identity", width=0.4)+
  
  geom_hline(yintercept = 0, colour = "grey", linetype="dashed") +
  theme(legend.position="bottom",
        legend.key.height = unit(0.05, "cm"),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        legend.text=element_text(size=10),
        strip.text = element_text(size=12),
        legend.title = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5) )+
  scale_x_discrete(limits = rev(levels(plot.data1$Type)))+
  scale_y_continuous(breaks = c(0,10, 20 , 30),
                     labels = c("0%","10%", "20%", "30%"), lim=c(0, 20))+
  xlab(NULL)+
  ylab(NULL)+
  coord_flip()+
  geom_label(data=inner_join(filter(plot.data1,diab=="With Hypertension",hospit=="Diagnosed"),
                             filter(plot.data1,diab=="Without Hypertension",hospit=="Diagnosed"),
                             by=c("covariate_name", "Type", "base", "hospit", "disease")),
             mapping = aes(x = 18, y = 15, label = paste0("Diagnosed\nWith Hypertension, N = ",n.x, "\nWihtout Hypertension, N = ", n.y), fill=NULL), size=3,
             show.legend=FALSE, inherit.aes=T)

ggsave(paste0(path_out,"demo_diab_nohospit.png"),
       dpi=300,
       width = 10, height = 10)
##################


dat_text <- data.frame(
  #label = c("Diagnosed\N = 9,916","Diagnosed\N = 1,903",
  # "Diagnosed\N = 169,810","Diagnosed\N = 24,248",
  # "Diagnosed\N = 18,939","Diagnosed\N = 14,900",
  # "Diagnosed\N = 1,765"),
  #label="With Hypertension",
  label="Diagnosed",
  # base = c( "SIDIAP (ES)",
  # "CUIMC (US)",
  # "IQVIA-OpenClaims (US)",
  # "OPTUM EHR (US)",
  # "VA-OMOP (US)",
  # "HEALTHVERITY (US)",
  # "HIRA (SK)"),
  x     = 17,
  y     = -25
)
dat_text2 <- data.frame(
  # label = c("Hospitalized\N = 3,286","Hospitalized\N = 1,065",
  # "Hospitalized\N = 70,212","Hospitalized\N = 24,248",
  # "Hospitalized\N = 18,939","Hospitalized\N = 14,900",
  # "Hospitalized\N = 1,765"),
  label="Hospitalized",
  #base = c( "SIDIAP (ES)",
  # "CUIMC (US)",
  # "IQVIA-OpenClaims (US)",
  # "OPTUM EHR (US)",
  # "VA-OMOP (US)",
  # "HEALTHVERITY (US)",
  # "HIRA (SK)"),
  x     = 17,
  y     = 25
)

ggplot(plot.data1, aes(Type, fill=diab))+#aes(factor(covariate_name, levels=order)))+
  facet_wrap(.~ base, nrow=3)+
  geom_bar(data=filter(plot.data1, diab=="With Hypertension",hospit=="Diagnosed"),aes(y=-mean*100), stat="identity")+
  geom_bar(data=filter(plot.data1, diab=="Without Hypertension", hospit=="Diagnosed"),aes(y=-mean*100), stat="identity", width=0.4)+
  geom_bar(data=filter(plot.data1, diab=="With Hypertension",hospit=="Hospitalized"),aes(y=mean*100), stat="identity")+
  geom_bar(data=filter(plot.data1, diab=="Without Hypertension", hospit=="Hospitalized"),aes(y=mean*100), stat="identity", width=0.4)+
  
  geom_hline(yintercept = 0, colour = "grey", linetype="dashed") +
  theme(legend.position="bottom",
        legend.key.height = unit(0.05, "cm"),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5) )+
  scale_y_continuous(breaks = c(-30,-20,-10, 0,10, 20 , 30),
                     labels = c( "30%", "20%","10%", "0%","10%", "20%", "30%"), lim=c(-30, 30))+
  # scale_fill_manual(breaks=c("COVID-19","Influenza"),
  #                   values=c("COVID-19"="#F8766D", "Influenza"="#00BA38"))+
  # 
  xlab(NULL)+
  ylab(NULL)+
  coord_flip()+
  geom_label( data    = dat_text,
              mapping = aes(x = x, y = y, label = label, fill=NULL), size=2,
              show.legend=FALSE, inherit.aes=T)+ 
  geom_label( data    = dat_text2,
              mapping = aes(x = x, y = y, label = label, fill=NULL), size=2,
              show.legend=FALSE, inherit.aes=T)

ggsave(paste0(path_out,"demographic_unselectedBases.png"),
       dpi=300,
       width = 10, height = 10)

#lollipop smd

data2<-data%>%
  filter(base%in%bases, 
         disease=="covid",
         !(base=="HIRA"&hospit=="Diagnosed"),
         !(base=="STARR-OMOP"&hospit=="Hospitalized"),
         !base%in%c("Premier","HM-Hospitales", "SIDIAP-H"))%>%
  filter(!str_detect(covariate_name, paste0(remove, collapse="|")))

reorder<-(data2%>%filter(base=="IQVIA-OpenClaims", hospit=="Diagnosed")%>%arrange(smd))$Type

data2<-data2%>%mutate(base=recode(base, 
                                  "SIDIAP"="SIDIAP (ES)",
                                  "CUIMC" ="CUIMC (US)",
                                  "IQVIA-OpenClaims"="IQVIA-OpenClaims (US)",
                                  "OptumEhr"="OPTUM EHR (US)",
                                  "VA-OMOP"="VA-OMOP (US)",
                                  "HEALTHVERITY"="HEALTHVERITY (US)",
                                  "HIRA"="HIRA (SK)",
                                  "UCHealth-OMOP"="UCHealth-OMOP (US)",
                                  "UWM-CRD"="UWM-CRD (US)"
                                  
))%>%mutate(Type=factor(Type,levels=reorder))

ggplot(data2, aes(x=smd, y=Type)) +
  geom_segment(aes(x = 0, y = Type, xend = smd, yend = Type)) +
  geom_point(size = 2.5, aes(fill=hospit, col=hospit), shape=21, alpha=.9) +
  coord_cartesian(xlim = c(-1.1, 1.1)) +
  facet_wrap(.~base , nrow=3) +
  theme_bw () +
  theme(axis.title.y = element_blank(), axis.text.y = element_text(size=10)) +
  theme(axis.title.x = element_text(size=10), axis.text.x = element_text(size=10)) +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=10),
        legend.position = "bottom")+
  ylab("Comorbidities")+
  xlab("SMD")



ggsave(paste0("//epofs/apistillo/CHARYBDIS/Diabetes/","lolli_smd.png"),
       dpi=300,
       width = 12, height = 12)

################
# barplot comorbidities con overlap with hospit
############

bases<-c(
  "CUIMC",
  "HEALTHVERITY",
  "HIRA",
  "IQVIA-OpenClaims",
  "OptumEhr",
  "SIDIAP",
  "CU-AMC-HDC",
  "UWM-CRD",
  "VA-OMOP")
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "diabetes.tobarplot.Rda"))

data3<-plot.data%>%
  filter(!str_detect(covariate_name, paste0(remove, collapse="|")),
         !base%in%bases, disease=="covid")%>%
  mutate(base=recode(base, 
                     "SIDIAP"="SIDIAP (ES)",
                     "CUIMC" ="CUIMC (US)",
                     "IQVIA-OpenClaims"="IQVIA-OpenClaims (US)",
                     "OptumEhr"="OPTUM EHR (US)",
                     "VA-OMOP"="VA-OMOP (US)",
                     "HEALTHVERITY"="HEALTHVERITY (US)",
                     "HIRA"="HIRA (SK)",
                     "CU-AMC-HDC"="CU-AMC-HDC (US)",
                     "UWM-CRD"="UWM-CRD (US)"
  ))%>%
  filter(mean>=0)
data3<-plot.data%>%
  filter(!str_detect(covariate_name, paste0(remove, collapse="|")),
         !base%in%bases, disease=="covid")%>%
  mutate(base=recode(base, 
                     "CPRD"="CPRD (UK)",
                     "IPCI" ="IPCI (NL)",
                     "IQVIA-DAGermany"="IQVIA-DAGermany (DE)",
                     "IQVIA-LPDFrance"="IQVIA-LPDFrance (FR)",
                     "LPDItaly"="IQVIA-LPDItaly (IT)",
                     "STARR-OMOP"="STARR-OMOP (US)",
                     "TRDW"="TRDW (US)"
  ))%>%
  filter(mean>=0)



try<-data3%>%
  select(-sd)%>%mutate(diab=ifelse(diab=="With Hypertension", "Diabetes", "NoDiabetes"))%>%
  pivot_wider(names_from = diab, values_from = mean)%>%
  mutate(difference=ifelse(Diabetes<NoDiabetes, Diabetes, NoDiabetes))

reorder<-(try%>%filter(base=="IQVIA-OpenClaims (US)", hospit=="Diagnosed")%>%arrange(Diabetes))$Type

try<-try%>%
  mutate(Type=factor(Type,levels=reorder))

ggplot(try, aes(x=Type))  +
  facet_wrap(~ base, ncol=3)+
  #diag
  geom_bar(data=try%>%filter(hospit=="Diagnosed"),aes(y=-Diabetes*100,fill="With Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(data=try%>%filter(hospit=="Diagnosed"),aes(y=-NoDiabetes*100, fill="Without Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(data=try%>%filter(hospit=="Diagnosed"),aes(y =-difference*100,fill="Overlap") , stat = "identity")+
  #hospit
  geom_bar(data=try%>%filter(hospit=="Hospitalized"),aes(y=Diabetes*100,fill="With Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(data=try%>%filter(hospit=="Hospitalized"),aes(y=NoDiabetes*100, fill="Without Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(data=try%>%filter(hospit=="Hospitalized"),aes(y =difference*100,fill="Overlap") , stat = "identity")+
  
  geom_hline(yintercept = 0, colour = "grey20", linetype="dashed") +
  theme(legend.position="bottom",
        #axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.key.height = unit(0.05, "cm"),
        text=element_text(size=14),
        legend.title = element_blank(),
        
        strip.text.y = element_text(face = "bold"),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        panel.border = element_rect(color = "grey", fill = NA, size = .5) 
  )+
  scale_y_continuous(breaks = c(-100,-75,-50,-25,0, 25, 50, 75, 100), labels = c("100%","75%" , "50%","25%","0%","25%", "50%","75%", "100%"), limits = c(-100,100))+
  
  xlab(NULL)+
  ylab(NULL)+
  
  #edit legends
  scale_fill_manual(breaks=c("Overlap","With Hypertension","Without Hypertension"),
                    values=c("Overlap"="grey60","With Hypertension"="#F8766D", "Without Hypertension"="#00BFC4"))+
  coord_flip()+
  geom_label( data    = dat_text,
              mapping = aes(x = x, y = y, label = label, fill=NULL), size=4,
              show.legend=FALSE, inherit.aes=T)+ 
  geom_label( data    = dat_text2,
              mapping = aes(x = x, y = y, label = label, fill=NULL), size=4,
              show.legend=FALSE, inherit.aes=T)


dat_text <- data.frame(
  
  label="Diagnosed",
  
  y     = -70,
  x     = 5
)
dat_text2 <- data.frame(
  
  label="Hospitalized",
  
  y    = 70,
  x     = 5
)

ggsave(paste0(path_out,"bardiab1.png"),
       dpi=300,
       width = 12, height = 13)

################
# barplot comorbidities con overlap WITHOUT hospit
############

bases<-c(
  "CPRD",
  "IPCI" ,
  "IQVIA-DAGermany",
  "IQVIA-LPDFrance",
  "LPDItaly",
  "STARR-OMOP",
  "TRDW")
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "diabetes.tobarplot.Rda"))

data3<-plot.data%>%
  filter(!str_detect(covariate_name, paste0(remove, collapse="|")),
         base%in%bases, disease=="covid")%>%
  mutate(base=recode(base, 
                     "CPRD"="CPRD (UK)",
                     "IPCI" ="IPCI (NL)",
                     "IQVIA-DAGermany"="IQVIA-DAGermany (DE)",
                     "IQVIA-LPDFrance"="IQVIA-LPDFrance (FR)",
                     "LPDItaly"="IQVIA-LPDItaly (IT)",
                     "STARR-OMOP"="STARR-OMOP (US)",
                     "TRDW"="TRDW (US)"
  ))%>%
  filter(mean>=0)



try<-data3%>%
  select(-sd)%>%mutate(diab=ifelse(diab=="With Hypertension", "Diabetes", "NoDiabetes"))%>%
  pivot_wider(names_from = diab, values_from = mean)%>%
  mutate(difference=ifelse(Diabetes<NoDiabetes, Diabetes, NoDiabetes))

reorder<-(try%>%filter(base=="STARR-OMOP (US)", hospit=="Diagnosed")%>%arrange(Diabetes))$Type

try<-try%>%
  mutate(Type=factor(Type,levels=reorder))

ggplot(try, aes(x=Type))  +
  facet_wrap(~ base, ncol=3)+
  #diag
  geom_bar(data=try%>%filter(hospit=="Diagnosed"),aes(y=Diabetes*100,fill="With Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(data=try%>%filter(hospit=="Diagnosed"),aes(y=NoDiabetes*100, fill="Without Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(data=try%>%filter(hospit=="Diagnosed"),aes(y =difference*100,fill="Overlap") , stat = "identity")+
  
  geom_hline(yintercept = 0, colour = "grey20", linetype="dashed") +
  theme(legend.position="bottom",
        #axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.key.height = unit(0.05, "cm"),
        text=element_text(size=14),
        legend.title = element_blank(),
        
        strip.text.y = element_text(face = "bold"),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        panel.border = element_rect(color = "grey", fill = NA, size = .5) 
  )+
  scale_y_continuous(breaks = c(-100,-75,-50,-25,0, 25, 50, 75, 100), 
                     labels = c("100%","75%" , "50%","25%","0%","25%", "50%","75%", "100%"), 
                     limits = c(0,100))+
  
  xlab(NULL)+
  ylab(NULL)+
  
  #edit legends
  scale_fill_manual(breaks=c("Overlap","With Hypertension","Without Hypertension"),
                    values=c("Overlap"="grey60","With Hypertension"="#F8766D", "Without Hypertension"="#00BFC4"))+
  coord_flip()+
  geom_label( data    = dat_text,
              mapping = aes(x = x, y = y, label = label, fill=NULL), size=4,
              show.legend=FALSE, inherit.aes=T)


dat_text <- data.frame(
  
  label="Diagnosed",
  
  y     = 70,
  x     = 5
)


ggsave(paste0(path_out,"bardiab.nohosp.png"),
       dpi=300,
       width = 12, height = 13)

#############
##MANHATTAN PLOT
###########

library(viridis)
library(ggrepel)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "diabetes.allfeatures.11.11.Rda"), envir = globalenv())
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
conditions<- read.csv(paste0(path_cond,"conditionConceptClassification.csv"))%>%
  rename(type=conceptName, group=categoryName)%>%select(-1,-4)
keep<-c("Blood disease",
        "Cardiovascular disease",
        "Congenital disease",
        "Digestive disease",
        "Endocrine or metabolic disease",
        "ENT disease",
        "Eye disease",
        "Genitourinary disease",
        "Iatrogenic condition",
        "Infection",
        "Injury and poisoning",
        "Mental disease",
        "Neoplasm",
        "Nerve disease and pain",
        #"Perinatal disease",
        #"Pregnancy or childbirth disease",
        "Respiratory disease",
        "Skin disease",
        "Soft tissue or bone disease")


merged_pretty<-table%>%
  separate(covariate_name, into=c("cohort","entire"), sep = "during", remove = T)%>%
  separate(entire, into=c("date","type"), sep = ": ", remove = T)%>%
  select(-contains("SMD"),-contains("20201008"))%>%
  select( -covariate_id)%>%
  filter(!str_detect(cohort, "drug"),str_detect(date, "day -365 through -1 days"))%>%
  filter(type%in%conditions$type)%>%
  left_join(conditions, by = "type")

merged_pretty<-merged_pretty %>% 
  mutate(group=ifelse(!is.na(group), as.character(group), "Uncategorised"))%>% 
  mutate(group=ifelse(group%in%keep, as.character(group), "Uncategorised")) %>% 
  mutate(group=factor(group, levels=c(keep, "Uncategorised")))


obese<-merged_pretty%>%select(-contains("No_T2"))%>%relocate("group", .after="type")%>%unique()
noobese<-merged_pretty%>%select(1,2,3,group,contains("No_T2"))%>%unique()
test1<-obese%>%pivot_longer(cols=5:ncol(obese), names_to=c("base", "diab", "hospit", "disease", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%distinct(across(1:9), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")

test2<-noobese%>%pivot_longer(cols=5:ncol(noobese), names_to=c("base", "no","diab", "hospit", "disease", "tipo"), names_sep="_")%>%select(-no)%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo), diab="No_T2_Diabetes")%>%distinct(across(1:9), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")

plot.data<-rbind(test1, test2)


#add smd ob vs no ob
prova<-plot.data%>%filter(diab=="Hypertension")
prova1<-plot.data%>%filter(diab=="No_T2_Diabetes")
plot.data.obVSnoob<-left_join(prova, prova1, by=c("cohort","date", "type","group","base", "disease", "hospit"))%>%
  mutate(smd=((abs(mean.x)-abs(mean.y))/(sqrt(sd.x^2+sd.y^2))), tipo_smd="diabVSnodiab")%>%
  select(-diab.x, -diab.y)

plot.data.obVSnoob<-plot.data.obVSnoob%>%filter(!group=="Not Available")  
keep<-c("Blood disease",
        "Cardiovascular disease",
        "Congenital disease",
        "Digestive disease",
        "Endocrine or metabolic disease",
        "ENT disease",
        "Eye disease",
        "Genitourinary disease",
        "Iatrogenic condition",
        "Infection",
        "Injury and poisoning",
        "Mental disease",
        "Neoplasm",
        "Nerve disease and pain",
        #"Perinatal disease",
        #"Pregnancy or childbirth disease",
        "Respiratory disease",
        "Skin disease",
        "Soft tissue or bone disease")
bases<-c("CUIMC",
         "HEALTHVERITY",
         "HIRA",
         "IQVIA-OpenClaims",
         "OptumEhr",
         "SIDIAP",
         "CU-AMC-HDC",
         "UWM-CRD",
         "VA-OMOP")


plot.data.obVSnoob<-plot.data.obVSnoob%>%filter(base%in%bases, !hospit=="ICU", disease=="covid",!group=="Uncategorised")

plot.data.obVSnoob<-plot.data.obVSnoob%>%mutate(base=recode(base, 
                                                            "SIDIAP"="SIDIAP (ES)",
                                                            "CUIMC" ="CUIMC (US)",
                                                            "IQVIA-OpenClaims"="IQVIA-OpenClaims (US)",
                                                            "OptumEhr"="OPTUM EHR (US)",
                                                            "VA-OMOP"="VA-OMOP (US)",
                                                            "HEALTHVERITY"="HEALTHVERITY (US)",
                                                            "HIRA"="HIRA (SK)",
                                                            "CU-AMC-HDC"="CU-AMC-HDC (US)",
                                                            "UWM-CRD"="UWM-CRD (US)"
                                                            
))

toplot<-plot.data.obVSnoob%>%mutate(toplot=paste0(group, type, sep="_"))%>%
  filter(mean.x>0, mean.y>0, !(base=="HIRA"&hospit=="Diagnosed"),
         !str_detect(tolower(type), "diabetes"),
         disease=="covid")

bases<-c("CUIMC (US)",
         "HEALTHVERITY (US)",
         "HIRA (SK)",
         "IQVIA-OpenClaims (US)",
         "OPTUM EHR (US)",
         "SIDIAP (ES)",
         "CU-AMC-HDC (US)",
         "UWM-CRD (US)",
         "VA-OMOP (US)")

plot<-ggplot(toplot, aes(x=toplot, y = smd, fill= group,label=type))+
  geom_point(size=3, shape=21, colour="grey30",alpha=.9)+
  facet_grid(factor(base, levels=bases)~ hospit, scales = "free", switch="y")+
  scale_size(guide = 'none')+
  geom_hline(yintercept = 0, colour = "#000000", linetype=2, size=1.05) +
  theme_bw()+
  theme(panel.spacing.x=unit(0, "lines"),
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        legend.key.height = unit(0.05, "cm"), 
        legend.position = "bottom",
        legend.title = NULL,
        legend.text=element_text(size=10),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        panel.grid = element_blank(),
        strip.text.y.left = element_text(angle = 0)
  )+
  scale_x_discrete(expand = c(0.03, 0))+
  scale_y_continuous(breaks=c(-0.3, 0, 0.3, 0.6, 0.9), lim=c(-0.40,1.1),position = "right")+
  #scale_fill_viridis(discrete = TRUE, option = "D")+
  scale_fill_manual(values = brightness(palette,0.9))+
  guides(fill=guide_legend(nrow=6))+
  xlab("Covariates")+
  ylab("SMD")+
  # geom_text_repel(data=toplot%>%filter(smd>0.5),
  #                  size=4,position ='jitter',box.padding = 0.5, 
  #                  force=20,fill="white",
  #                  alpha=0.8, ylim=c(0.3,NA))+
  ggtitle("SMDs of COVID-19 patients with Type 2 Diabetes Mellitus compared to those without it")

plot
ggsave(paste0("//epofs/apistillo/CHARYBDIS/Diabetes/", "manhattan_diabetes.png"),
       dpi=300,
       width = 10, height =9)

install.packages("shades")
library(shades)
library(scales)
a<-(hue_pal()(18))
index<-c(12,2,5,9,13,15,3,6,10,14,17,4,7,1,16,8,11,18)
palette<-a[index]
show_col(palette)

###################
## plot y tabla 30 days outcomes 
#######################
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "diabetes.allfeatures.11.11.Rda"), envir = globalenv())

## barplot
keep<-c(
  "Hospitalization episodes",
  "intensive services during hospitalization",
  "Acute myocardial infarction",
  "Cardiac arrhythmia during hospitalization",
  "Heart failure during hospitalization",
  "Stroke (ischemic or hemorrhagic) events",
  "Total cardiovascular disease events",
  "Deep vein thrombosis events" ,
  "Pulmonary embolism" ,
  "Acute kidney injury (aki) diagnosis during hospitalization" ,
  "Acute pancreatitis" ,
  "acute respiratory distress syndrome (ards) during hospitalization",
  "Gastrointestinal bleeding events" ,
  "Hepatic failure",
  "Sepsis during hospitalization",
  "death"
  
)
# merged<-table%>%filter(str_detect(covariate_name, paste0(keep,"$", collapse="|"))|
#covariate_name%in%keep)
merged_pretty<-table%>%
  separate(covariate_name, into=c("cohort","entire"), sep = "during day", remove = T)%>%
  separate(entire, into=c("date","type"), sep = ": ", remove = T)%>%
  select(-contains("SMD"),-contains("20201008"))%>%
  select( -covariate_id)%>%
  filter(!str_detect(cohort, "drug"),str_detect(date, "0 through 30 days"))%>%
  filter(!str_detect(type,"Brainstem|Fear"),
         tolower(type)%in%tolower(keep))



obese<-merged_pretty%>%select(-contains("No_T2"))
noobese<-merged_pretty%>%select(1,2,3,contains("No_T2"))
test1<-obese%>%pivot_longer(cols=4:ncol(obese), names_to=c("base", "diab", "hospit", "disease", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%distinct(across(1:7), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")

test2<-noobese%>%pivot_longer(cols=4:ncol(noobese), names_to=c("base", "no","diab", "hospit", "disease", "tipo"), names_sep="_")%>%select(-no)%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo), diab="No_T2_Diabetes")%>%distinct(across(1:7), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")

plot.data<-rbind(test1, test2)


bases<-c("CUIMC",
         "HEALTHVERITY",
         "HIRA",
         "IQVIA-OpenClaims",
         "OptumEhr",
         "SIDIAP",
         "CU-AMC-HDC",
         "UWM-CRD",
         "VA-OMOP")

to.plot<-plot.data%>%filter(base%in%bases, disease=="covid", !hospit=="ICU", mean>=0)%>%
  mutate(base=factor(base, levels=bases))%>%
  # mutate(mean=ifelse((hospit=="Diagnosed"& !type%in%c("Hospitalization episodes", "death")), NA, mean),
  mutate( mean=ifelse((hospit=="Hospitalized"& type==c("Hospitalization episodes")), NA, mean),
          mean=ifelse((hospit=="Diagnosed"& base=="HIRA"), NA, mean))%>%
  #        mean=ifelse((hospit=="Hospitalized"& !type==c("death")&base=="SIDIAP"), NA, mean))%>%
  # select(-cohort, -date, -disease)%>%
  pivot_wider(names_from=diab, values_from=mean)%>%
  mutate(difference=ifelse(`Hypertension`>No_T2_Diabetes, No_T2_Diabetes, `Hypertension`))%>%
  separate(type, into=c("type", NA), sep=" events")%>%
  separate(type, into=c("type", NA), sep=" during")%>%
  # rbind(tibble(type="Cardiovascular events", base="SIDIAP", hospit="Hospitalized", Obese=0, `Non Obese`=0, difference=0))%>%
  # rbind(tibble(type="Thromboembolic events", base="SIDIAP", hospit="Hospitalized", Obese=0, `Non Obese`=0, difference=0))%>%
  # rbind(tibble(type="Other events", base="SIDIAP", hospit="Hospitalized", Obese=0, `Non Obese`=0, difference=0))%>%
  mutate(type=factor(str_to_title(type), levels=rev(keep1)), 
         type=recode(type,"Stroke (Ischemic Or Hemorrhagic)"="Stroke",
                     "Total Cardiovascular Disease"= "Cardiovascular Disease",
                     "Acute Kidney Injury (Aki) Diagnosis"="Acute Kidney Injury",
                     "Acute Respiratory Distress Syndrome (Ards)"="ARDS",
                     "Hospitalization Episodes"="Hospitalization"))%>%
  mutate(base=recode(base, 
                     "SIDIAP"="SIDIAP (ES)",
                     "CUIMC" ="CUIMC (US)",
                     "IQVIA-OpenClaims"="IQVIA-OpenClaims\n(US)",
                     "OptumEhr"="OPTUM EHR (US)",
                     "VA-OMOP"="VA-OMOP (US)",
                     "HEALTHVERITY"="HEALTHVERITY\n(US)",
                     "HIRA"="HIRA (SK)",
                     "CU-AMC-HDC"="CU-AMC-HDC (US)",
                     "UWM-CRD"="UWM-CRD (US)"))

keep1<-str_to_title(c(
  "Hospitalization episodes",
  "intensive services",
  # "Cardiovascular events",
  "Acute myocardial infarction",
  "Cardiac arrhythmia",
  "Heart failure",
  "Stroke (ischemic or hemorrhagic)",
  "Total cardiovascular disease",
  # "Thromboembolic events",
  "Deep vein thrombosis" ,
  "Pulmonary embolism" ,
  # "Other events",
  "Acute kidney injury (AKI) diagnosis" ,
  "Acute pancreatitis" ,
  "acute respiratory distress syndrome (ARDS)",
  "Gastrointestinal bleeding" ,
  "Hepatic failure",
  "Sepsis",
  "death"
))

##ob no ob
type.levels<-(to.plot%>%filter(base=="VA-OMOP (US)", hospit=="Hospitalized")%>%arrange(`Hypertension`))$type

ggplot(na.omit(to.plot),aes(x=factor(type, levels=type.levels), y=mean))  +
  facet_grid(hospit~base,scale="free_y", space = "free_y")+
  geom_bar(aes(y=abs(`Hypertension`)*100,fill="With Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(aes(y=abs(No_T2_Diabetes)*100, fill="Without Hypertension"),alpha=.9,  stat="identity")+
  geom_bar(aes(y = abs(difference)*100,fill="Overlap") , stat = "identity")+
  theme(legend.position="bottom",
        #axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.key.height = unit(0.05, "cm"),
        legend.text=element_text(size=10),
        legend.title = element_blank(),
        text = element_text(size=14),
        strip.text.y = element_text(face = "bold"),
        strip.text.x = element_text(size=10,face="bold"),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        
        panel.border = element_rect(color = "grey", fill = NA, size = 0.5) 
  )+
  scale_y_continuous(limits = c(0,80))+
  
  xlab(NULL)+
  ylab(NULL)+
  
  #edit legends
  scale_fill_manual(breaks=c("Overlap","With Hypertension","Without Hypertension"),
                    values=c("Overlap"="grey60","With Hypertension"="#FF1E00", "Without Hypertension"="#0776A0"))+
  coord_flip()

ggsave(paste0("//epofs/apistillo/CHARYBDIS/Diabetes/","outcomes.bar.diabetes.png"),
       dpi=300,
       width = 16, height = 10)

save(to.plot, file="//epofs/apistillo/CHARYBDIS/SavedData/to.plot.diabetes.outcomes.20.11.Rda")
#add counts


## table outcomes

to.plot<-plot.data%>%filter(base%in%bases, disease=="covid", !hospit=="ICU", mean>=0)%>%
  mutate(base=factor(base, levels=bases))%>%
  # mutate(mean=ifelse((hospit=="Diagnosed"& !type%in%c("Hospitalization episodes", "death")), NA, mean),
  mutate( mean=ifelse((hospit=="Hospitalized"& type==c("Hospitalization episodes")), NA, mean),
          mean=ifelse((hospit=="Diagnosed"& base=="HIRA"), NA, mean))%>%
  left_join(to.join)%>%
  mutate(mean=(paste0(format(mean*100,nsmall=1)," (", format(mean*100-round(100*1.96*sqrt(( mean*(1-mean))/n),1),nsmall=1),
                      "-",format(mean*100+round(100*1.96*sqrt(( mean*(1-mean))/n),1),nsmall=1),")")))
%>%
  select(-n)%>%
  separate(type, into=c("type", NA), sep=" events")%>%
  separate(type, into=c("type", NA), sep=" during")%>%
  # rbind(tibble(type="Cardiovascular events", base="SIDIAP", hospit="Hospitalized", Obese=0, `Non Obese`=0, difference=0))%>%
  # rbind(tibble(type="Thromboembolic events", base="SIDIAP", hospit="Hospitalized", Obese=0, `Non Obese`=0, difference=0))%>%
  # rbind(tibble(type="Other events", base="SIDIAP", hospit="Hospitalized", Obese=0, `Non Obese`=0, difference=0))%>%
  mutate(type=factor(str_to_title(type), levels=rev(keep1)), 
         type=recode(type,"Stroke (Ischemic Or Hemorrhagic)"="Stroke",
                     "Total Cardiovascular Disease"= "Cardiovascular Disease",
                     "Acute Kidney Injury (Aki) Diagnosis"="Acute Kidney Injury",
                     "Acute Respiratory Distress Syndrome (Ards)"="ARDS",
                     "Hospitalization Episodes"="Hospitalization"))%>%
  mutate(base=recode(base, 
                     "SIDIAP"="SIDIAP (ES)",
                     "CUIMC" ="CUIMC (US)",
                     "IQVIA-OpenClaims"="IQVIA-OpenClaims\n(US)",
                     "OptumEhr"="OPTUM EHR (US)",
                     "VA-OMOP"="VA-OMOP (US)",
                     "HEALTHVERITY"="HEALTHVERITY\n(US)",
                     "HIRA"="HIRA (SK)",
                     "CU-AMC-HDC"="CU-AMC-HDC (US)",
                     "UWM-CRD"="UWM-CRD (US)"))

tabla<-rbind(
  tabla.out.diag<-to.plot%>%filter(disease=="covid",hospit=="Diagnosed")%>%
    pivot_wider(names_from=c("base","diab"),values_from="mean")%>%
    select(type, contains(bases))%>%
    mutate_at(2:ncol(tabla.out.diag), funs(str_replace_all(.,"NA", "-"))),
  
  tabla.out.hosp<-to.plot%>%filter(disease=="covid",hospit=="Hospitalized")%>%
    pivot_wider(names_from=c("base","diab"),values_from="mean")%>%
    select(type, contains(bases))%>%
    mutate_at(2:ncol(tabla.out.diag), funs(str_replace_all(.,"NA", "-")))
)
write.table(tabla,paste0("//epofs/apistillo/CHARYBDIS/Diabetes/","tabla.ICs.diabetes.csv"), sep = ";", row.names = F, dec = ",",na="")

#######
## compute confidence intervals
path_cond="//epofs/apistillo/CHARYBDIS/Conditions/"
# conditions: an excel file containing names of feature to consider
conditions<- read.csv2(paste0(path_cond,"confidence_obesity3.csv"), header = F, stringsAsFactors = FALSE)

data<-conditions[-c(1:5),]%>%mutate_at(2:length(conditions),funs(as.numeric(.)))
data<-data%>%mutate_at(2:length(data), funs(paste0(format(.,nsmall=1)," (", format(.-round(100*1.96*sqrt(( ./100*(1-./100))/.[1]),1),nsmall=1),
                                                   "-",format(.+round(100*1.96*sqrt(( ./100*(1-./100))/.[1]),1),nsmall=1),")")))%>%
  mutate_at(2:ncol(data), funs(str_replace_all(.,"NA", "-")))
write.table(data,paste0("//epofs/apistillo/CHARYBDIS/Tablas/","tabla.ICs.obesity3.second.csv"), sep = ";", row.names = F, dec = ",",na="")

#database cleaning --------
data.clean <- table %>% select(-contains("SMD"),-contains("sd"),-covariate_id) %>% 
  filter(!str_detect(covariate_name, "no prior")) 


hyper<-data.clean%>%select(-contains("No_Hypertension"))
nohyper <- data.clean%>%select(1,contains("No_Hypertension"))
test1<-hyper%>%pivot_longer(cols=2:ncol(hyper), names_to=c("base", "hyper", "hospit", "disease"), names_sep="_") 

test2<-nohyper%>%pivot_longer(cols=2:ncol(nohyper), names_to=c("base", "hyper", "no", "hospit", "disease"), names_sep="_") %>% 
  mutate(hyper="No Hypertension") %>% select(-no)


data.clean<-rbind(test1, test2) %>% filter(!is.na(value))

rm(table, test1,test2, hyper, nohyper)
