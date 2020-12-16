#packges 
library(tidyverse)
library(lubridate)
library(reshape2)
library(scales)
library(igraph)
library(ggalluvial)
library(epinemo)
library(EpiContactTrace)
library(sf)
library(readr)
library(tidyverse);library(tibble)
library(viridis); library(ggplot2)
library(lubridate);library(reshape2)
library(rgdal);library(broom)
library(readr); library(igraph)
library(reshape2);library(sf)
library(ggpubr); library(tidygraph);
library(ggraph);library(ggpubr)
library(epicontacts)
library(openxlsx);library(lubridate)
library(gt)
require(plyr)
require(dplyr)



# Erase workplace
rm(list = ls())
cat("\014")

pop<-read_csv("./data/PrestageFarms_list_2020.csv");pop


salmo<-read_csv("./data/Growout_Bootie.csv");salmo

## transform to date formats
salmo$Start_Date<-as.Date (as.character(salmo$Start_Date), format= "%m/%d/%y")
salmo$Process_Date<-as.Date (as.character(salmo$Process_Date), format= "%m/%d/%y")

## transform as.factors

salmo<-salmo%>%
  mutate(Project=`Project#`)%>%
  select(-`Project#`)


salmo$Prod_Type<-as.factor(salmo$Prod_Type)
salmo$Sex<-as.factor(salmo$Sex)
salmo$Breed<-as.factor(salmo$Breed)
salmo$Brooder<-as.factor(salmo$Brooder)
salmo$Grower<-as.factor(salmo$Grower)
salmo$Bootie_Swabs<-as.factor(salmo$Bootie_Swabs)
salmo$Group<-as.factor(salmo$Group)
salmo$Serotype<-as.factor(salmo$Serotype)
salmo$Project<-as.factor(salmo$Project)
str(salmo)

## relevel prod. type
## check the sex<>production type -- Ask 

salmo<-salmo%>%
  mutate(Prod_Type = revalue(Prod_Type,
                                c("Conv"="CONV", "Conv."="CONV")))

## relevel swabs
salmo%>%
  mutate(Bootie_Swabs = revalue(Bootie_Swabs,
                c("positive"="Positive", "Positive"="Positive")))



## relevel prod. type
salmo%>%
  mutate(Bootie_Swabs = revalue(Bootie_Swabs,
                                c("positive"="Positive", "Positive"="Positive")))

salmo$Bootie_Swabs<-ifelse(salmo$Bootie_Swabs ="NA", Negative, salmo$Bootie_Swabs)

## relevel of Serotype
salmo%>%
  mutate(Serotype = revalue(Serotype,
                                c("16:d:-"="16:d:1,2", "Positive"="Positive", "6,7:r:"="6,7:r", "1,4,5,12:i:"="1,4,5,12:i", "1,4,5,12:"="1,4,5,12", "Rough "O":r:1,5"="Rough O:r:1,5","TYPH VAR.O:5-"="TYPH VAR. O:5", "Typhimirium"="Typhimurium", "OUAKAM"="Ouakam", "Lillie"="Lille","Bertaq"="Berta")))


#16:d:-        --- 16:d:1,2
#6,7:r:         ---- 6,7:r
#1,4,5,12:i:-  ---1,4,5,12:i
#1,4,5,12:      ---1,4,5,12
#Rough "O":r:1,5    ---Rough O:r:1,5
#TYPH VAR. O:5-     -----TYPH VAR. O:5
# Typhimirium       ---Typhimurium
#OUAKAM             --Ouakam
#Lillie              -- Lille
##Bertaq            --Berta          



# descriptive stats ----
## now free description
salmo%>%
  group_by(Serotype)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  drop_na()%>%
  dplyr::select(n,Serotype)%>%
  arrange(desc(n))%>%
  print(n = 10)


salmo%>%
  group_by(Prod_Type)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  ungroup() %>%
  drop_na()%>%
  arrange(desc(n))%>% 
  ggplot(aes(fct_infreq(Prod_Type,n), n)) +
  geom_bar(stat="identity") + coord_flip()+
  scale_y_continuous(breaks = seq(0,2500,by = 10))+
  labs(x="TProd_Type", y="volume")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))

ggsave("./Fig/freq_prod_type.tiff",plot = last_plot(), dpi = 300, width = 190, height = 130, units = "mm")



