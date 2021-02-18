####### manhattan con proporciones

library(viridis)
library(ggrepel)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)

load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "cancer.allfeature.28.01.Rda"), envir = globalenv())
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
  select(-contains("SMD"))%>%
  select( -covariate_id)%>%
  filter(!str_detect(cohort, "drug"),str_detect(date, "day -365 through -1 days"))%>%
  filter(type%in%conditions$type)%>%
  left_join(conditions, by = "type")

merged_pretty<-merged_pretty %>% 
  mutate(group=ifelse(!is.na(group), as.character(group), "Uncategorised"))%>% 
  mutate(group=ifelse(group%in%keep, as.character(group), "Uncategorised")) %>% 
  mutate(group=factor(group, levels=c(keep, "Uncategorised")))


obese<-merged_pretty%>%select(-contains("No_Cancer"))%>%relocate("group", .after="type")%>%unique()
noobese<-merged_pretty%>%select(1,2,3,group,contains("No_Cancer"))%>%unique()
test1<-obese%>%pivot_longer(cols=5:ncol(obese), names_to=c("base", "obese", "hospit", "disease", "tipo"), names_sep="_")%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo))%>%distinct(across(1:9), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")

test2<-noobese%>%pivot_longer(cols=5:ncol(noobese), names_to=c("base", "no","obese", "hospit", "disease", "tipo"), names_sep="_")%>%select(-no)%>%
  mutate(tipo=ifelse(is.na(tipo), "mean", tipo), obese="No_Cancer")%>%distinct(across(1:9), .keep_all=TRUE)%>%
  pivot_wider(values_from = "value", names_from="tipo")

plot.data<-rbind(test1, test2)

bases<-c("SIDIAP","CU-AMC-HDC" , "CUIMC", "HEALTHVERITY", "IQVIA-OpenClaims", "OPTUM-EHR", "STARR-OMOP" , "VA-OMOP")
plot.data<-plot.data%>%filter(base%in%bases,
                              obese=="Cancer")%>%
  mutate(base=recode(base,"HEALTHVERITY"="HealthVerity","OPTUM-EHR"="Optum-Ehr"))

#SMD hospitalized vs diagnosed COVID-19 (los hospitalized "arriba")
#SMD hospitalzied COVID-19 vs hospitalized flue (COVID "arriba") 
bases<-c("SIDIAP","CU-AMC-HDC" , "CUIMC", "HealthVerity", "IQVIA-OpenClaims", "Optum-Ehr", "STARR-OMOP" , "VA-OMOP")



toplot<-plot.data%>%mutate(toplot=paste0(group, type, sep="_"))%>%
  filter(mean>=0, !hospit=="ICU", disease=="covid")%>%
  mutate(base=factor(base, levels=bases)) #
save(toplot, file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "dataset.manhattan.cancer_jan21.Rda"))

library(shades)
library(scales)
a<-(hue_pal()(18))
index<-c(12,2,5,9,13,15,3,6,10,14,17,4,7,1,16,8,11,18)
palette<-a[index]
show_col(palette)


count.h<-summarise(toplot%>%filter(hospit=="Hospitalized")%>%unique()%>%
                     select(3,4, base)%>%group_by(base), n())%>%
  mutate(hospit="Hospitalized")
count.d<-summarise(toplot%>%filter(hospit=="Diagnosed")%>%unique()%>%select(3,4, base)%>%group_by(base), n())%>%
  mutate(hospit="Diagnosed")
count<-rbind(count.h, count.d)%>%
  mutate(label=paste0(base, "\nN of conditions = ",`n()`))

toplot<-left_join(toplot,count, by=c("base","hospit"))
toplot.d<-toplot%>%filter(hospit=="Diagnosed")
plot1<-ggplot(toplot.d, aes(x=toplot, y = mean*100,fill= group))+
  geom_point( shape=21, size=2,colour="grey20",alpha=.9)+
  facet_grid(hospit~base, scales = "free",labeller=labeller(base=setNames(toplot.d$label,toplot.d$base)))+
  # scale_size(guide = 'none')+
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
        #strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        panel.grid = element_blank()
  )+
  
  scale_x_discrete(expand = c(0.03, 0))+
  scale_y_continuous(lim=c(0,100),expand = c(0,0.03))+#breaks=c(-0.2, 0, 0.2, 0.4, 0.6), 
  #scale_fill_viridis(discrete = TRUE, option = "D")+
  scale_fill_manual(values = brightness(palette,0.9))+
  #geom_text_repel(data=toplot%>%filter(smd>0.3),size=3,positsion ='jitter', force=3)+
  guides(fill=guide_legend(nrow=3))+
  xlab(NULL)+
  ylab("%")
plot1


toplot.h<-toplot%>%filter(hospit=="Hospitalized")
plot2<-ggplot(toplot.h, aes(x=toplot, y = mean*100,fill= group))+
  geom_point( shape=21, size=2,colour="grey20",alpha=.9)+
  facet_grid(hospit~base, scales = "free",labeller=labeller(base=setNames(toplot.h$label,toplot.h$base)))+
  # scale_size(guide = 'none')+
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
        #strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        panel.grid = element_blank()
  )+
  
  scale_x_discrete(expand = c(0.03, 0))+
  scale_y_continuous(lim=c(0,100),expand = c(0,0.03))+#breaks=c(-0.2, 0, 0.2, 0.4, 0.6), 
  #scale_fill_viridis(discrete = TRUE, option = "D")+
  scale_fill_manual(values = brightness(palette,0.9))+
  #geom_text_repel(data=toplot%>%filter(smd>0.3),size=3,positsion ='jitter', force=3)+
  guides(fill=guide_legend(nrow=3))+
  xlab("Covariates")+
  ylab("%")

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

leg<-get_legend(plot1)

library(gridExtra)

final<-grid.arrange(plot1+ theme(legend.position="none"),plot2+ theme(legend.position="none"),leg, nrow = 3,heights=c(2, 2, 0.8))

ggsave(final,file=paste0("//epofs/apistillo/CHARYBDIS/Cancer/", "manhattan_cancer.mean2.png"),
       dpi=300,
       width = 15, height = 6)