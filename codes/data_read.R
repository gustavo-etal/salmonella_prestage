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


# Erase workplace
rm(list = ls())
cat("\014")

pop<-read_csv("./data/PrestageFarms_list_2020.csv");pop


salmo<-read_csv("./data/Growout_Bootie.csv");salmo

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



