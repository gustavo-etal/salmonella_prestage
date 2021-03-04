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
library(ggpubr)
library(ggrepel)
library(gganimate)
library(ggnetwork)     # devtools::install_github("briatte/ggnetwork")
library(intergraph)    # ggnetwork needs this to wield igraph things
library(ggrepel)       # fancy, non-ovelapping labels
library(svgPanZoom)
library(gridExtra)
library(linkcomm);library(ggmap)
#remotes::install_github("duncantl/SVGAnnotation")# zoom, zoom
#library(SVGAnnotation) # to help svgPanZoom; it's a bioconductor package
library(DT)  
### Process_Date is two weeks before that column the samples are collected
## just be aware that it will change more likely to be 4 week (02/04/2021)
### Start_date 5 weeks birds fo from brooder et o grower.

## remove PF from FARM + Hatchery and Feed mill


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
                                c("Conv"="Conventional", "Conv."="Conventional",
                                  "CONV"="Conventional",
                                  "ABF"="Antibiotic-free")))

## relabel swabs
salmo<-salmo%>%
  mutate(Bootie_Swabs = revalue(Bootie_Swabs,
                c("positive"="Positive", "Positve"="Positive",
                  "Positiive"="Positive","negative"="Negative")))

## if NA we make them negative
salmo$Bootie_Swabs[is.na(salmo$Bootie_Swabs)] = "Negative"

## relevel of Serotype
table(salmo$Serotype)

salmo<-salmo%>%
  mutate(Serotype = revalue(Serotype,
                                c("1,4,5,12 :i:-"="1,4,5,12:i",
                                  "1,4,5,12:"="1,4,5,12",
                                  "1,4,5,12:i:-"="1,4,5,12:i" )))

table(salmo$Serotype)


salmo%>%
  filter(!Serotype=="0")%>%
  #group_by(Start_Date)%>%
  dplyr::count(Process_Date, sort = TRUE)%>%
  #mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  mutate(month = format(Process_Date, "%m"), year = format(Process_Date, "%Y"))%>%
  ggplot(aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Montly Total Salmonella",
       y = "Number of positive cases",
       x = "Month") + theme_bw(base_size = 15)+
theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
  axis.line = element_line(size=0.8, colour = "black"),
  axis.text.x=element_text(colour="black", size = 15 ),#,angle = 90, vjust = 0.5, hjust=1),
  axis.text.y=element_text(colour="black", size = 15),
  axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
  text = element_text(size = 15, face = "bold"),
  strip.background = element_rect(
    color="black",size=1.5, linetype="solid"
  )         
)

ggsave("./Fig/freq_year_month.tiff",plot = last_plot(), dpi = 300, width = 290, height = 130, units = "mm")


## by serotype
salmo%>%
  filter(!Serotype=="0")%>%
  #group_by(Start_Date)%>%
  dplyr::count(Serotype,Process_Date, sort = TRUE)%>%
  #mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  mutate(month = format(Process_Date, "%m"), year = format(Process_Date, "%Y"))%>%
  ggplot(aes(x = month, y = n, fill=Serotype)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Montly Total Salmonella",
       y = "Number of positive cases",
       x = "Month") + theme_bw(base_size = 15)+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 15 ),#,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )


ggsave("./Fig/freq_year_month_serotype.tiff",plot = last_plot(), dpi = 300, width = 390, height = 130, units = "mm")


## by farm type
atbfree<-salmo%>%
  filter(!Serotype=="0" & Prod_Type=="Antibiotic-free")%>%
  #group_by(Start_Date)%>%
  dplyr::count(Process_Date,Serotype, sort = TRUE)%>%
  #mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  mutate(month = format(Process_Date, "%m"), year = format(Process_Date, "%Y"))%>%
  ggplot(aes(x = month, y = n, fill=Serotype)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Montly Total Salmonella-Antibiotic-free",
       y = "Number of positive cases",
       x = "Month") + theme_bw(base_size = 15)+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 15 ),#,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  );atbfree



commercial<-salmo%>%
  filter(!Serotype=="0" & Prod_Type=="Conventional")%>%
  #group_by(Start_Date)%>%
  dplyr::count(Process_Date,Serotype, sort = TRUE)%>%
  #mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  mutate(month = format(Process_Date, "%m"), year = format(Process_Date, "%Y"))%>%
  ggplot(aes(x = month, y = n, fill=Serotype)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Montly Total Salmonella-commercial",
       y = "Number of positive cases",
       x = "Month") + theme_bw(base_size = 15)+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 15 ),#,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  );commercial

# group commercial with atb=free
ggarrange(atbfree , commercial, nrow=2)

## by farm type
salmo%>%
  filter(!Serotype=="0")%>%
  #group_by(Start_Date)%>%
  dplyr::count(Process_Date,Prod_Type, sort = TRUE)%>%
  #mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  mutate(month = format(Process_Date, "%m"), year = format(Process_Date, "%Y"))%>%
  ggplot(aes(x = month, y = n, fill=Prod_Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "Montly Total Salmonella",
       y = "Number of positive cases",
       x = "Month") + theme_bw(base_size = 15)+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 15 ),#,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 15),
    axis.title.x = element_text(size = 15, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 15, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 15, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )


ggsave("./Fig/freq_year_month_production_type.tiff",plot = last_plot(), dpi = 300, width = 290, height = 130, units = "mm")


# descriptive stats ----
# total population is the list of farm from 2020

## now free description Serotype ----
salmo%>%
  filter(!Serotype=="0")%>%
  group_by(Serotype,Prod_Type)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  drop_na()%>%
  dplyr::select(n,Serotype)%>%
  arrange(desc(n))%>%
  print(n = 30)


salmo%>%
  filter(!Serotype=="0")%>%
  group_by(Prod_Type)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  arrange(desc(prop))%>% 
  ggplot(aes(x=Serotype , y=prop,
             fill=Prod_Type)) +
  geom_bar(position="dodge",stat='identity')+
  
  labs(fill = "Farm type") +
  scale_y_continuous(labels = function(x) paste0(x*2, "%"))+
  ylab("Proportion of positive")+
  xlab("")+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 18,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 18),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )

ggsave("./Fig/freq_year_month.tiff",plot = last_plot(), dpi = 300, width = 290, height = 130, units = "mm")


p<-salmo%>%
  filter(!Serotype=="0"&!Prod_Type=="Conventional")%>%
  group_by(Prod_Type)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  arrange(desc(prop))%>% 
  ggplot(aes(x=Serotype , y=prop,
             fill=Prod_Type)) +
  geom_bar(position="dodge",stat='identity')+
  
  labs(fill = "Farm type") +
  scale_y_continuous(labels = function(x) paste0(x*2, "%"))+
  ylab("Proportion of positive")+
  xlab("")+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 18,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 18),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )


p1<-salmo%>%
  filter(!Serotype=="0"& Prod_Type=="Conventional")%>%
  group_by(Prod_Type)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  arrange(desc(prop))%>% 
  ggplot(aes(x=Serotype , y=prop,
             fill=Prod_Type)) +
  geom_bar(position="dodge",stat='identity')+
  
  labs(fill = "Farm type") +
  scale_y_continuous(labels = function(x) paste0(x*2, "%"))+
  ylab("Proportion of positive")+
  xlab("")+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 18,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 18),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )

ggarrange(p,p1)

ggsave("./Fig/freq_prod_type.tiff",plot = last_plot(), dpi = 300, width = 190, height = 130, units = "mm")


salmo%>%
  filter(!Serotype=="0")%>%
  group_by(Sex)%>%
  dplyr::count(Serotype, sort = TRUE)%>%
  mutate(prop = n/sum(n)*100)%>% # will add a new variable name=prop
  ungroup() %>%
  drop_na()%>%
  arrange(desc(prop))%>% 
  ggplot(aes(x=Serotype , y=prop,
             fill=Sex)) +
  geom_bar(position="dodge",stat='identity')+
  
  labs(fill = "Farm type") +
  scale_y_continuous(labels = function(x) paste0(x*2, "%"))+
  ylab("Proportion of positive")+
  xlab("")+
  theme(#axis.line.x = element_line(size = 0.4, colour = "black"),
    axis.line = element_line(size=0.8, colour = "black"),
    axis.text.x=element_text(colour="black", size = 18,angle = 90, vjust = 0.5, hjust=1),
    axis.text.y=element_text(colour="black", size = 18),
    axis.title.x = element_text(size = 22, margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 22, margin = margin(t = 0, r = 20, b = 0, l = -1)), #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 7))
    text = element_text(size = 18, face = "bold"),
    strip.background = element_rect(
      color="black",size=1.5, linetype="solid"
    )         
  )

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
  geom_point(data = data %>% drop_na(Serotype)%>% filter(!Serotype==0),
             aes(x = LONG,y = LAT))+
  ggtitle("Salmonella distribution")+
  # geom_label_repel(data = data , nudge_x = 0, nudge_y = -0,
  #                  aes(x = LONG, 
  #                      y = LAT,
  #                      label = as.factor(Serotype)))+
  theme(axis.title = element_blank())+
  facet_wrap(~Serotype )


data%>%
  mutate(month = format(Process_Date, "%m"), year = format(Process_Date, "%Y"))%>%
  filter(Serotype=="Infantis")%>%
  ggplot() +
  geom_sf(data = nc,  colour = "grey", alpha = 0.005) +
  geom_point(data = data %>% drop_na(Serotype), aes(x = LONG,y = LAT))+
  ggtitle("Salmonella distribution")+
  theme(axis.title = element_blank())+
  facet_wrap(~ as.factor(Process_Date))


## networks Brooder>>Grower

salmo<-salmo%>%
  drop_na(Brooder,Grower)
# edge list
nodes <- unique(c(salmo$Brooder,salmo$Grower))

# netwrork
g <- simplify(graph_from_data_frame
              (salmo[,c("Brooder","Grower")],
                directed=TRUE))

V(g)$size <- centralization.degree(g)$res
# add the weight to the edges
E(g)$weight <- seq_len(ecount(g))

igraph::degree(g,mode="out") 
igraph::degree(g,mode="in") 


# table by degree
datatable(arrange(data_frame(person=V(g)$name, centrality_degree=V(g)$size), desc(centrality_degree)))
#plot(g)


# netwrork
salmonet<-salmo%>%
  distinct(Brooder,Grower)

## list of farms
popnet<-pop%>%
  select(Grower,BIRDS,TYPE)


pnet<-tbl_graph(popnet,salmonet)
plot(pnet)


pnet %>% activate(edges) %>% as_tibble()


pnet_h <- pnet %>% activate(edges)
plot(pnet_h)


pnet_h %>%
  ggraph(layout = "kk") +
  geom_node_point() +
  geom_edge_link() 


pnet_h %>%
  activate(nodes) %>%
  mutate(centrality = centrality_betweenness()) %>%
  ggraph(layout = "graphopt") +
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = media), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()


pnet_h %>%
  activate(nodes) %>%
  mutate(centrality = centrality_betweenness()) 


g1 <- simplify(graph_from_data_frame
              (salmo[,c("Brooder","Grower")]))

graph <- as_tbl_graph(pnet) %>% 
  mutate(In = centrality_degree(),
         community = as.factor(group_infomap()))


## visual
ggraph(graph, layout = 'kk') +
  geom_edge_link(alpha=0.4) +
  geom_node_point(aes(size = In, colour= In ))+
  geom_node_text(aes(filter=In > 10, label = Grower), repel = TRUE)+
  scale_colour_gradient(low = "#00008B", high = "#63B8FF")+
  theme_graph()



node.level.network <- data.frame(Mean_degree= mean(degree(g, mode= "all")),
                                 Nos = vcount(g),
                                 edge =  ecount(g),
                                 diameter_i = diameter(g),
                                 #birds = sum(gtac_sample$BOVINO_TOT),
                                 GSCC = sort(clusters(g, "strong")$csize, decreasing = T)[1],
                                 GWCC = sort(clusters(g, "weak")$csize, decreasing = T)[1],
                                 Mean_betweenness = mean(betweenness(g)), 
                                 Clustering_coefficient = transitivity(g),
                                 Centralization = centr_degree(g)$centralization)

grid.table(node.level.network)


g1<-as_adjacency_matrix(g, attr="weight")
# 
edges <- graph_from_adjacency_matrix(as.matrix(g1),
                                     mode="directed", weighted=TRUE)


neta<-as_edgelist(g, names = TRUE)
net0<-asNetwork(edges)
library(GGally)
ggnet2(net0, 
       label = TRUE, 
       label.size = 3, 
       arrow.size = 3, 
       #size = "degree",
       arrow.gap = .03,
       palette = 'Set2', mode = "kamadakawai")+
  theme(legend.position = "bottom")+
  ggtitle("full netwrok")




## cluster formation-----

lc <- getLinkCommunities(neta, hcmethod = "single")
plot(lc, type = "graph", 
     layout = layout.fruchterman.reingold,
     scale.vertices = 0.2,
     nspace = 10,
     cid.cex=0.5,
     shownodesin=1)




#montly netwrok

salmo$month <- floor_date(salmo$Process_Date, "month")
time_lags <- unique(sort(salmo$month))

# funcao pra relizar mesalmente as an?lises 
rede.temporal <- c()
ienesimas <- c()

for ( j in time_lags) { 
  banco.month <- salmo[salmo$month %in% j,]
  banco.month <-  banco.month %>% drop_na(Brooder, Grower)
  #make the network
  nodes <- unique(c(banco.month$Brooder,
                    banco.month$Grower))
  
  g <- simplify(graph_from_data_frame(banco.month[,c("Brooder", "Grower")],
                                      directed=TRUE))
  
  # calculando os parametros da rede
  rede.estatica <- data.frame(Mean_degree  = mean(degree(g, mode= "all")),
                              farms = vcount(g),
                              edges_i =  ecount(g),
                              #diameter_i = diameter(g),
                              #Animais = sum(banco.month$BOVINO_TOT),
                              #GSCC = sort(clusters(g, "strong")$csize, decreasing = T)[1],
                              GWCC = sort(clusters(g, "weak")$csize, decreasing = T)[1],
                              #Mean_betweenness = mean(betweenness(g)), 
                              #Clustering_coefficient = transitivity(g),
                              Centralization = centr_degree(g)$centralization,
                              month = unique(banco.month$month) )
  rede.temporal <- as.data.frame(rbind(rede.temporal, rede.estatica)) 
  ienesimas <- c(ienesimas, j)
  # controle do for   
  x <- round((NROW(ienesimas)/NROW(time_lags)), digits = 2)
  print (paste("Estou no", x*100, "%  "))
  
  
  
}


#plot temporal network
data <- melt(rede.temporal, id = c("month"))

# O seguinte codigo adapta e cria o grafico


data %>%
  mutate(Date = as.Date(month)) %>%
  ggplot(aes(x= Date, y = value))+
  geom_line(colour = "red", size = .5)+
  geom_point(colour = "red", size  = .5)+
  facet_wrap(~variable,scales = "free" , ncol = 2)+
  theme(text = element_text(size = 15, face = "bold"))








## come back here
## edge level
E(g)$weight <- 1
g <- simplify(g, edge.attr.comb="sum")

## make the network
dat <- ggnetwork(g)


ggplot() +
  geom_edges(data=dat, 
             aes(x=x, y=y, xend=xend, yend=yend),
             color="grey50", curvature=0.1, size=0.15, alpha=1/2) +
  geom_nodes(data=dat,
             aes(x=x, y=y, xend=xend, yend=yend, size=sqrt(weight)),
             alpha=1/3) +
  #geom_label_repel(data=unique(dat[dat$size>50,c(1,2,5)]),
  #                 aes(x=x, y=y, label=vertex.names), 
   #                size=2, color="#8856a7") +
  theme_blank() +
  theme(legend.position="none") -> gg


svgPanZoom(svgPlot(show(gg), height=15, width=15), 
           width="960px",
           controlIconsEnabled=TRUE)
