## Creating maps of whitebark pine distribution in the interior west
## original code from: 
## updated and reformatted by Cecilia Martinez
## July 24 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------

library(maps)
library(ggplot2)
library(ggmap)
library(mapdata)
library(viridis)
library(dplyr)

# Load necessary data -----------------------------------------------------

map_data <- read_csv("data_processed/WBP_growth.csv")

# Making maps -------------------------------------------------------------


##Making Maps
m = map_data('state', region = c('Idaho', "Wyoming", "Montana"))

spatial_df <- map_data %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,STATECD,SPCD) %>%
  distinct() %>%
  mutate(Species = ifelse(SPCD==101,"Whitebark Pine"))
length(unique(spatial_df$CN)) #318

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(STATECD),alpha = .9))+
  ggtitle("Distribution of Remeasured Trees in Interior West")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed() 
#theme(legend.position = "bottom")


#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html


ditch_the_axes <- theme(
  #axis.text = element_blank(),
  #axis.line = element_blank(),
  #axis.ticks = element_blank(),
  #panel.border = element_blank(),
  panel.grid = element_blank(),
  #axis.title = element_blank()
)

ggplot()+
  geom_polygon(data = m, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") + 
  coord_fixed(1.3) + 
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(STATECD),alpha = .9)) +
  #scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
  #   "Engelmann spruce" = "gold1")) +
  theme_bw() +
  ditch_the_axes

#elev & climate with distribution

map_df <- map_data %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,SPCD,STATECD) %>%
  distinct() %>%
  mutate(Species = ifelse(SPCD==101,"Whitebark Pine"))
length(unique(map_df$CN)) 

map_wbp <- ggplot() + 
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey9", fill="white" )+
  geom_point(data=map_df,aes(x=LON,y=LAT,color=STATECD,alpha = .5), show.legend = F)+
  scale_colour_manual(values = c("Montana" = "purple4", "Idaho" = "gold1")) +
  ggtitle("a)")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed(1.3) +
  theme_bw() +
  ditch_the_axes + theme(legend.position = "none")

# Effect plots
plot(effect("BALIVE", gmodel.DIA.q))
plot(effect("BALIVE", gmodel.DIA_lin))
plot(effect("PREVDIA", gmodel.DIA.q))
plot(effect("PREVDIA", gmodel.DIA_lin))


# export model for coefficients and scaling information -------------------
#save(gmodel.7, gr.scaling, growSD, file = "./Code/IPM/GrRescaling.Rdata")


##Making Maps
library(maps)
m = map_data('state', region = c('Idaho', "Wyoming", "Montana"))

spatial_df <- grow_data_remeas %>%
  ungroup() %>%
  select(CN,LAT,LON,SPCD,STATECD) %>%
  distinct() %>%
  mutate(Species = ifelse(SPCD==101,"Whitebark Pine"))
length(unique(spatial_df$CN)) #318

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(STATECD),alpha = .9))+
  ggtitle("Distribution of Remeasured Trees in Interior West")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed() 
#theme(legend.position = "bottom")



ditch_the_axes <- theme(
  #axis.text = element_blank(),
  #axis.line = element_blank(),
  #axis.ticks = element_blank(),
  #panel.border = element_blank(),
  panel.grid = element_blank(),
  #axis.title = element_blank()
)

ggplot()+
  geom_polygon(data = m, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") + 
  coord_fixed(1.3) + 
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(STATECD),alpha = .9)) +
  #scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
  #   "Engelmann spruce" = "gold1")) +
  theme_bw() +
  ditch_the_axes

#elev & climate with distribution

map_df <- grow_data_remeas %>%
  ungroup() %>%
  select(CN,LAT,LON,SPCD,STATECD) %>%
  distinct() %>%
  mutate(Species = ifelse(SPCD==101,"Whitebark Pine"))
length(unique(map_df$CN)) 

map_wbp <- ggplot() + 
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="grey9", fill="white" )+
  geom_point(data=map_df,aes(x=LON,y=LAT,color=STATECD,alpha = .5), show.legend = F)+
  scale_colour_manual(values = c("Montana" = "purple4", "Idaho" = "gold1")) +
  ggtitle("a)")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed(1.3) +
  theme_bw() +
  ditch_the_axes + theme(legend.position = "none")

