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
library(ggrepel)
### Process_Date is two weeks before that column the samples are collected
## just be aware that it will change more likely to be 4 week (02/04/2021)
### Start_date 5 weeks birds fo from brooder et o grower.



# Erase workplace
rm(list = ls())
cat("\014")

## list of farm and location
pop<-read_csv("./data/PrestageFarms_list_2020.csv");pop

## salmonela time series
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
salmo$Bootie_Swabs<-as.factor(as.character(salmo$Bootie_Swabs))
salmo$Grower<-as.factor(salmo$Grower)
salmo$Group<-as.factor(salmo$Group)
salmo$Serotype<-as.factor(salmo$Serotype)
salmo$Project<-as.factor(salmo$Project)
str(salmo)

## relabel prod. type
## check the sex<>production type -- Ask 

salmo<-salmo%>%
  mutate(Prod_Type = revalue(Prod_Type,
                                c("Conv"="CONV", "Conv."="CONV")))

## relabel swabs
salmo<-salmo%>%
  mutate(Bootie_Swabs = revalue(Bootie_Swabs,
                c("positive"="Positive", "Positve"="Positive",
                  "Positiive"="Positive")))

## if NA we make them negative
salmo$Bootie_Swabs[is.na(salmo$Bootie_Swabs)] = "Negative"

## relevel of Serotype
table(salmo$Serotype)

salmo<-salmo%>%
  mutate(Serotype = revalue(Serotype,
                                c("16:d:-"="16:d:1,2",
                                 
                                  "6,7:r:-"="6,7:r",
                                  
                                  "1,4,5,12:i:"="1,4,5,12:i",
                                  
                                  "1,4,5,12:i:-"="1,4,5,12:i",
                                  
                                  "1,4,5,12:"="1,4,5,12",
                                  
                                  "Typhimirium"="Typhimurium",
                                  
                                  "OUAKAM"="Ouakam",
                                  
                                  "Lillie"="Lille",
                                  
                                  "Bertaq"="Berta")))

table(salmo$Serotype)


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
  labs(x="Production type", y="volume")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))

ggsave("./Fig/freq_prod_type.tiff",plot = last_plot(), dpi = 300, width = 190, height = 130, units = "mm")



salmo%>%
  dplyr::count(Prod_Type,Serotype, sort = TRUE)%>%
  drop_na()%>%
  #mutate(prop=QTD_DECLARADA/sum(QTD_DECLARADA)*100)%>%
  #print(n = 30)%>%
  ggplot(aes(x = n, y =  Prod_Type, fill =  Serotype)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(x="Number of isolates", y="Production type")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))


salmo%>%
  dplyr::count(Sex,Serotype, sort = TRUE)%>%
  drop_na()%>%
  #mutate(prop=QTD_DECLARADA/sum(QTD_DECLARADA)*100)%>%
  #print(n = 30)%>%
  ggplot(aes(x = n, y =  Sex, fill =  Serotype)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(x="Number of isolates", y="Production type")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))



#ggsave("./Fig/freq_prod_type.tiff",plot = last_plot(), dpi = 300, width = 190, height = 130, units = "mm")


## join both data
head(salmo)
names(pop)

# the collumn Grower in the pop in not complete 
# please make sure all names are there for the perfect match
##DAN & ANGIE WACHTER

data<-salmo%>%
  left_join(pop, by="Grower")


### distribution 

## set the zoom limit
lon_bounds <- c(-77.56 ,-79.32)
lat_bounds <- c(-35.23,-34.44 )


## get NC shape
nc <- st_read(system.file("shape/nc.shp", package="sf"))

ggplot() +
  geom_sf(data = nc,  colour = "grey", alpha = 0.005) +
  #coord_sf(xlim = lon_bounds, ylim = lat_bounds)+
  geom_point(data = data %>% drop_na(Serotype), aes(x = LONG,
                              y = LAT))+
  ggtitle("Salmonella distribution")+
  # geom_label_repel(data = data , nudge_x = 0, nudge_y = -0,
  #                  aes(x = LONG, 
  #                      y = LAT,
  #                      label = as.factor(Serotype)))+
  theme(axis.title = element_blank())+
  facet_wrap(~Serotype, nrow=5, ncol=10)



