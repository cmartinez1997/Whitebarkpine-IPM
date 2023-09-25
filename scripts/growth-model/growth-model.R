## Creating growth model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# Load necessary packages -------------------------------------------------
library(tidyverse)
library(lme4)
library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)
library(DHARMa)
library(performance)


# Read in data used for model ---------------------------------------------

model_growth_data <- read_csv("data_processed/WBP_growth.csv")

model_growth_data <- model_growth_data %>% dplyr::select(TRE_CN, PREV_TRE_CN, PLT_CN, PREV_PLT_CN, LAT, LON, ELEV, 
                                                  DIA, PREVDIA, DIA_DIFF, DIA_INCR, MEASYEAR, PREV_MEASYEAR, CENSUS_INTERVAL, 
                                                  BALIVE, CONDID, STATUSCD, DSTRBCD1, DSTRBCD2, DSTRBCD3, AGENTCD)

#standardize numerical covariates, center and scale 

scaled_growth_data <- model_growth_data %>% mutate_at(scale, .vars = vars(-TRE_CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                      -MEASYEAR, -PREV_MEASYEAR,
                                                                      -CENSUS_INTERVAL, -DIA_INCR, -STATUSCD))


#growth model that includes plot level random effect

#this is the linear model 
grow_model_lin <- lmer(DIA_INCR ~ PREVDIA + BALIVE + (1|PLT_CN), data = scaled_growth_data)
class(grow_model_lin) <- "lmerMOD"

# check the model residuals in DHARMA: not looking so great 
plot(simulateResiduals(grow_model_lin, integerResponse = F), quantreg = T)
check_model(grow_model_lin)
#how to graph residuals with mixed models?? - ask for help

#check the residuals using DHARMA package
plot(grow_model_lin)
#quantile deviations detected 
#model prediction - rank transformed

#quadratic effects - check preference for model by using AICc
grow_model_quad <- lmer(DIA_INCR ~ PREVDIA + BALIVE  + 
                       I(PREVDIA^2) + I(BALIVE^2) +
                       (1|PLT_CN), data = scaled_growth_data)
qqnorm(residuals(grow_model_quad))

check_model(grow_model_quad)
summary(grow_model_quad)
plot(simulateResiduals(grow_model_quad, integerResponse = F), quantreg = T)
plot(grow_model_quad)

resid_pearson <- residuals(gmodel.DIA.q,type="pearson",scaled=TRUE)
predicted <- predict(gmodel.DIA.q)
plotLowess(resid_pearson~predicted, ylab="Residuals",xlab="Predicted", main="Gaussian")


#model selection step using AIC
mod.comp0<-model.sel(gmodel.DIA_lin,gmodel.DIA.q)
#better model is the quadratic, but linear has the lower AIC

gmodel.DIA.size <- lm(DIA ~ PREVDIA  + 
                        I(PREVDIA^2) , data = scaled_grow_data)
xx=seq(0,30,by=0.5)
plot(grow_data_remeas$PREVDIA,jitter(grow_data_remeas$DIA),main='Growth/Shrinkage/Stasis')  
lines(xx,predict(gmodel.DIA.size, data.frame(PREVDIA=xx)),col='red',lwd=3)

# growSD is used for building IPM (see BuildIPM.R), need sd to build IPM
grow.SD.DIA.q <- sd(resid(gmodel.DIA.q))

#make a model to check the IPM with only size as predictor
gmodel_check <- lmer(DIA_INCR ~ PREVDIA   + 
                       I(PREVDIA^2) +
                       (1|PLT_CN), data = scaled_grow_data)
grow_SD_check <- sd(resid(gmodel_check))

# specify the predictors in the exported models
gr.predictors <- c("PREVDIA", "BALIVE") 

# deal with scaled data
get_scale = function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

gr.scaling = get_scale(scaled_grow_data, gr.predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in gr.predictors) {
  attributes(scaled_grow_data[, i]) = NULL
}

plot(effect("BALIVE", gmodel.DIA.q))
plot(effect("BALIVE", gmodel.DIA_lin))
plot(effect("PREVDIA", gmodel.DIA.q))
plot(effect("PREVDIA", gmodel.DIA_lin))


# export model for coefficients and scaling information -------------------
#save(gmodel.7, gr.scaling, growSD, file = "./Code/IPM/GrRescaling.Rdata")
save(gmodel.DIA.q, grow.SD.DIA.q,gmodel_check, grow_SD_check,
     gr.scaling, scaled_grow_data, file = "data/WBP_data/GrRescaling.Rdata")


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


#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)

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

library(maps)

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

#########################################
load("./data/formatted/data_all.Rdata")
length(unique(data_all$TRE_CN)) #531
tre_cal <- unique(cal_dia$TRE_CN)
spatial_df <- data_all %>%
  ungroup() %>%
  filter(TRE_CN %in% tre_cal,
         Year > 1960) %>%
  #filter(SPCD %in% c(93,122,202)) %>%
  mutate(ppt_an = ppt_Jan + ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul +
           ppt_Aug + ppt_Sep + ppt_Oct + ppt_Nov + ppt_Dec,
         tmax_an = (tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul +
                      tmax_Aug + tmax_Sep + tmax_Oct + tmax_Nov + tmax_Dec)/12) %>%
  dplyr::select(TRE_CN,SPCD,ELEV,ppt_an,tmax_an,RW) %>%
  group_by(TRE_CN,SPCD,ELEV) %>%
  summarise(mn_ppt = mean(ppt_an, na.rm = TRUE),
            mn_tmax = mean(tmax_an, na.rm = TRUE),
            mn_growth = mean(RW, na.rm = TRUE)) %>%
  mutate(Species = factor(ifelse(SPCD==93,"Engelmann spruce",
                                 ifelse(SPCD==122,"Ponderosa pine",
                                        "Douglas fir"))))

length(unique(spatial_df$TRE_CN)) #265

#precip
dis_prec <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_ppt,y=ELEV, color=Species, alpha = 0.5), 
             show.legend = F)+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() +   ggtitle("b)")+
  xlab("Average Annual Precipitation (mm)") + ylab("Elevation (ft)")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/dis_prec.png",
       width = 3, height = 2, units = "in")

#temp
dis_temp <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_tmax,y=ELEV,color=Species, alpha = 0.5), 
             show.legend = F)+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() + ggtitle("c)") +
  xlab("Average Monthly Max Temperature (C)") + ylab("Elevation (ft)")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/dis_temp.png",
       width = 3, height = 2, units = "in")

spat_plt <- list(map_ut,dis_prec,dis_temp)
lay = rbind(c(1,1,1,2,2,2),
            c(1,1,1,2,2,2),
            c(1,1,1,3,3,3),
            c(1,1,1,3,3,3))
spat_fig <- grid.arrange(grobs = spat_plt, layout_matrix = lay)
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/spat_fig.png", spat_fig,
       height = 8, width = 10, units = "in")

sp_leg <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_tmax,y=ELEV,color=Species, alpha = 0.5))+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() + 
  xlab("Average Monthly Max Temperature (C)") + ylab("Elevation (ft)") +
  theme(legend.position="left")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/sp_leg.png",
       height = 8, width = 10, units = "in")

#arrange into one plot

grow_data_model <- grow_data_remeas[, c("CN", "PREV_TRE_CN", "PLT_CN", "PREV_PLT_CN","LAT", "LON", "ELEV",
                                        "DIA", "PREVDIA", "DIA_DIFF", "CONDID", "PREV_CONDID", "HT", "SUBP", "INVYR", 
                                        "UNITCD", "PLOT","TREE", "DIA_INCR",
                                        "MEASYEAR", "PREV_MEASYEAR", "CENSUS_INTERVAL", "BALIVE" )]


scaled_grow_data <- grow_data_model %>% mutate_at(scale, .vars = vars(-CN, -PREV_TRE_CN, -PLT_CN, -PREV_PLT_CN, -CONDID,
                                                                      -MEASYEAR, -PREV_MEASYEAR, 
                                                                      -CENSUS_INTERVAL, -DIA_INCR))

library(lme4)
library(lmerTest)
library(MuMIn) # use MuMin to choose between models (AICc)
library(DHARMa)

#growth model 

hist(grow_data_model$DIA_INCR) #looks normally distributed - some negative values, itneresting, how to correct for this

#growth mode, and plot level random effect
gmodel.DIA_lin <- lmer(DIA_INCR ~ PREVDIA + BALIVE + (1|PLT_CN), data = scaled_grow_data)
plot(simulateResiduals(gmodel.DIA_lin, integerResponse = F), quantreg = T)

#how to graph residuals with mixed models

#check the residuals 
plot(gmodel.DIA_lin)
#quantile deviations detected 
#model prediction - rank transformed
#gamma distirbution? data is skewed

#quadratic effect
gmodel.DIA.q <- lmer(DIA_INCR ~ PREVDIA + BALIVE  + 
                       I(PREVDIA^2) + I(BALIVE^2) +
                       (1|PLT_CN), data = scaled_grow_data)
qqnorm(residuals(gmodel.DIA.q))

check_model(gmodel.DIA.q)
summary(gmodel.DIA.q)
plot(simulateResiduals(gmodel.DIA.q, integerResponse = F), quantreg = T)
plot(gmodel.DIA.q)

resid_pearson <- residuals(gmodel.DIA.q,type="pearson",scaled=TRUE)
predicted <- predict(gmodel.DIA.q)
plotLowess(resid_pearson~predicted, ylab="Residuals",xlab="Predicted", main="Gaussian")


#model selection step using AIC
mod.comp0<-model.sel(gmodel.DIA_lin,gmodel.DIA.q)
#better model is the quadratic, but linear has the lower AIC

gmodel.DIA.size <- lm(DIA ~ PREVDIA  + 
                        I(PREVDIA^2) , data = scaled_grow_data)
xx=seq(0,30,by=0.5)
plot(grow_data_remeas$PREVDIA,jitter(grow_data_remeas$DIA),main='Growth/Shrinkage/Stasis')  
lines(xx,predict(gmodel.DIA.size, data.frame(PREVDIA=xx)),col='red',lwd=3)

# growSD is used for building IPM (see BuildIPM.R), need sd to build IPM
grow.SD.DIA.q <- sd(resid(gmodel.DIA.q))

#make a model to check the IPM with only size as predictor
gmodel_check <- lmer(DIA_INCR ~ PREVDIA   + 
                       I(PREVDIA^2) +
                       (1|PLT_CN), data = scaled_grow_data)
grow_SD_check <- sd(resid(gmodel_check))

# specify the predictors in the exported models
gr.predictors <- c("PREVDIA", "BALIVE") 

# deal with scaled data
get_scale = function(data, predictors) {
  sc = list("scale" = NULL, "center"  = NULL)
  for (i in predictors) {
    sc$scale[i] = attributes(data[, i])$"scaled:scale"
    sc$center[i] = attributes(data[, i])$"scaled:center"
  }
  return(sc)
}

gr.scaling = get_scale(scaled_grow_data, gr.predictors)

# remove scaling information from the dataset so that the model doesnt expect scaled data in predict()
for (i in gr.predictors) {
  attributes(scaled_grow_data[, i]) = NULL
}

plot(effect("BALIVE", gmodel.DIA.q))
plot(effect("BALIVE", gmodel.DIA_lin))
plot(effect("PREVDIA", gmodel.DIA.q))
plot(effect("PREVDIA", gmodel.DIA_lin))


# export model for coefficients and scaling information -------------------
#save(gmodel.7, gr.scaling, growSD, file = "./Code/IPM/GrRescaling.Rdata")
save(gmodel.DIA.q, grow.SD.DIA.q,gmodel_check, grow_SD_check,
     gr.scaling, scaled_grow_data, file = "data/WBP_data/GrRescaling.Rdata")


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


#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)

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

library(maps)

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

#########################################
load("./data/formatted/data_all.Rdata")
length(unique(data_all$TRE_CN)) #531
tre_cal <- unique(cal_dia$TRE_CN)
spatial_df <- data_all %>%
  ungroup() %>%
  filter(TRE_CN %in% tre_cal,
         Year > 1960) %>%
  #filter(SPCD %in% c(93,122,202)) %>%
  mutate(ppt_an = ppt_Jan + ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul +
           ppt_Aug + ppt_Sep + ppt_Oct + ppt_Nov + ppt_Dec,
         tmax_an = (tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul +
                      tmax_Aug + tmax_Sep + tmax_Oct + tmax_Nov + tmax_Dec)/12) %>%
  dplyr::select(TRE_CN,SPCD,ELEV,ppt_an,tmax_an,RW) %>%
  group_by(TRE_CN,SPCD,ELEV) %>%
  summarise(mn_ppt = mean(ppt_an, na.rm = TRUE),
            mn_tmax = mean(tmax_an, na.rm = TRUE),
            mn_growth = mean(RW, na.rm = TRUE)) %>%
  mutate(Species = factor(ifelse(SPCD==93,"Engelmann spruce",
                                 ifelse(SPCD==122,"Ponderosa pine",
                                        "Douglas fir"))))

length(unique(spatial_df$TRE_CN)) #265

#precip
dis_prec <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_ppt,y=ELEV, color=Species, alpha = 0.5), 
             show.legend = F)+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() +   ggtitle("b)")+
  xlab("Average Annual Precipitation (mm)") + ylab("Elevation (ft)")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/dis_prec.png",
       width = 3, height = 2, units = "in")

#temp
dis_temp <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_tmax,y=ELEV,color=Species, alpha = 0.5), 
             show.legend = F)+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() + ggtitle("c)") +
  xlab("Average Monthly Max Temperature (C)") + ylab("Elevation (ft)")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/dis_temp.png",
       width = 3, height = 2, units = "in")

spat_plt <- list(map_ut,dis_prec,dis_temp)
lay = rbind(c(1,1,1,2,2,2),
            c(1,1,1,2,2,2),
            c(1,1,1,3,3,3),
            c(1,1,1,3,3,3))
spat_fig <- grid.arrange(grobs = spat_plt, layout_matrix = lay)
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/spat_fig.png", spat_fig,
       height = 8, width = 10, units = "in")

sp_leg <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_tmax,y=ELEV,color=Species, alpha = 0.5))+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() + 
  xlab("Average Monthly Max Temperature (C)") + ylab("Elevation (ft)") +
  theme(legend.position="left")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/sp_leg.png",
       height = 8, width = 10, units = "in")

#arrange into one plot
