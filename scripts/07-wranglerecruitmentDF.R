## Making WBP data frame for recruitment model
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com


# load packages -----------------------------------------------------------

library(tidyverse)
library(sp)
library(raster)
library(rgdal)
library(wesanderson)

# read in data ------------------------------------------------------------

tree_iw <- read_csv("data_processed/TREE_MT-ID-WY.csv") 
plot_iw <- read_csv("data_processed/PLOT_MT-ID-WY.csv")
cond_iw <- read_csv("data_processed/COND_MT-ID-WY.csv") 

# filter data -------------------------------------------------------------

cond_iw <- cond_iw %>% 
  filter(COND_STATUS_CD == 1) # "accessible forest land" by FIA classification, go from 98,000 to 26,905

# Create data frame that will be used for zero-inflated Poisson.   --------

# building data frame for recruitment
recruitment_dat <- data.frame(plot = as.character(plot_iw$CN),
                       lat = plot_iw$LAT, 
                       lon = plot_iw$LON, 
                       elev = plot_iw$ELEV,
                       state = plot_iw$STATECD,
                       county = plot_iw$COUNTYCD,
                       plotID = plot_iw$PLOT,
                       PAwbp = integer(length(plot_iw$CN)), # will contain P/A information
                       measyear = plot_iw$MEASYEAR, 
                       plotprev = as.character(plot_iw$PREV_PLT_CN))
recruitment_dat$plot <- as.character(recruitment_dat$plot) # strangely enough, this line is necessary for the PA loop to work
recruitment_dat$plotprev <- as.character(recruitment_dat$plotprev)

# get rid of plots that were not remeasured
recruitment_dat <- recruitment_dat %>%
  filter(!is.na(plotprev)) # goes down a lot to 16,463 observations

# look up time 1 condition ID for each plot...but this isn't informative until plots are split by CONDID
recruitment_dat$CONDID <- cond_iw$CONDID[match(recruitment_dat$plotprev, cond_iw$PLT_CN)]


# figuring out wbpPA ------------------------------------------------------

## was WBP present or absent in each FIA plot? this is where tree data comes in
# make list of those tree records that fall in (time 2) plots that are found in recruitment data
pa.trees <- tree_iw %>% 
  filter(PLT_CN %in% unique(recruitment_dat$plot))
pa.list <- split(x=pa.trees, f=pa.trees$PLT_CN) 

# Loop through trees and code WBP presence/absence at time 2
# note that this includes dead trees, not just live trees
# needs to be split by CONDID within plots, see above...not implemented
for (i in 1:nrow(recruitment_dat)) {
  plot <- recruitment_dat[i, "plot"] # use concatenation of PLT and CONDID?
  print(i / nrow(recruitment_dat) * 100)
  tmp <- pa.list[[plot]]
  if (is.null(tmp)) recruitment_dat[i, "PAwbp"] <- NA # no tree records for this plot CN
  else if (101 %in% unique(tmp$SPCD)) recruitment_dat[i, "PAwbp"] <- 1
  else recruitment_dat[i, "PAwbp"] <- 0
  print(recruitment_dat[i, "PAwbp"])
}

# Get rid of all plots with no TREE records, no CONDID
recruitment_dat <- recruitment_dat[!is.na(recruitment_dat$PAwbp),] #goes down to 5656 observations

# Add fields for previous measurement year, census interval
recruitment_dat <- recruitment_dat %>%
  mutate(PREV_MEASYEAR = plot_iw$MEASYEAR[match(plotprev, plot_iw$CN)])
recruitment_dat <- recruitment_dat %>% 
  mutate(CENSUS_INTERVAL = measyear - PREV_MEASYEAR)


# count WBP recruits ------------------------------------------------------

# Loop through plots
# more generous definition of recruits, use this for now
# Recruit codes, RECONLECD 1= ingrowth, 2= through growth
recruitment_dat$recruits12 <- 0
recruits12 <- tree_iw %>%
  filter(RECONCILECD %in% c(1, 2) & SPCD == 101)
  for (i in unique(recruits12$PLT_CN)) {
  tmp <- subset(recruits12, PLT_CN == i) 
  print(nrow(tmp))
  recruitment_dat[recruitment_dat$plot == i, "recruits12"] <- nrow(tmp)
}

# more constrained definition of recruits, only RECONCILE CD 1
# this is what I ended up using in ZIP models
recruitment_dat$recruits1 <- 0
recruits1 <- tree_iw %>%
  filter(RECONCILECD == 1 & SPCD == 101)
for (i in unique(recruits1$PLT_CN)) {
  tmp <- subset(recruits1, PLT_CN == i) 
  print(nrow(tmp))
  recruitment_dat[recruitment_dat$plot == i, "recruits1"] <- nrow(tmp)
}

# there is no difference here
recruitment_dat$diff <- recruitment_dat$recruits12-recruitment_dat$recruits1
sum(recruitment_dat$diff) # returns the value 0

# create competition covariate data ---------------------------------------

plot_conds <- cond_iw %>%
  filter(PLT_CN %in% unique(recruitment_dat$plotprev))
for (i in 1:nrow(recruitment_dat)) {
  tmpPlot <- recruitment_dat[i, "plotprev"] # time 1 plot control number, extracting the plotprev value for current row
  condsMatch <- subset(plot_conds, PLT_CN == as.character(tmpPlot)) 
  balive <- condsMatch$BALIVE
  print(i)
  props <- condsMatch$CONDPROP_UNADJ #CONDPROP_UNADJ is the unadjusted proportion of the plot that is in the condition 
  wmean <- weighted.mean(balive, props, na.rm = T) #calculating the weighted mean of BALIVE for a plot using CONDPROP_UNADJ as weights
  if (length(balive) == 0) recruitment_dat[i, "BALIVE"] <- NA #if BALIVE is empty, set it to NA 
  else recruitment_dat[i, "BALIVE"] <- wmean #otherwise set BALIVE to the calculated weighted mean of BALIVE for the plot
}

recruitment_dat[is.nan(recruitment_dat$BALIVE), "BALIVE"] <- NA
recruitment_dat <- subset(recruitment_dat, !is.na(BALIVE)) #5354 trees


##create OFFSET (SEED SOURCE) data## I need to ask margaret about this
## ...and additional competition/facilitation offset variables that might affect recruitment rate

# get rid of trees that are not in the time 1 plots in recruitment dataframe (just to make the search smaller)
plot_trees <- subset(tree_iw, PLT_CN %in% unique(recruitment_dat$plotprev))
# create a list (indexed by plot) of trees (this also increases efficiency)
trees_list <- split(x=plot_trees, f=plot_trees$PLT_CN) #6723
missing <- setdiff(x=recruitment_dat$plotprev, y=tree_iw$PLT_CN)
head(recruitment_dat[recruitment_dat$plotprev %in% missing,])

# next line gets rid of those plots that had no trees at time 1 census
recruitment_dat <- recruitment_dat[!(recruitment_dat$plotprev %in% missing),]
# but trees could have colonized these 18 plots between time 1 and time 2

for (i in 1:nrow(recruitment_dat)) {
  plot <- recruitment_dat[i, "plotprev"] 
  print(i / nrow(recruitment_dat) * 100)
  plot_trees <- trees_list[[plot]]
  tmp.WBP <- plot_trees[plot_trees$SPCD == 101 & plot_trees$STATUSCD == 1, ]
  tmp.notWBP <- plot_trees[plot_trees$SPCD != 101 & plot_trees$STATUSCD == 1, ]
  recruitment_dat$WBPadults1[i] <- nrow(tmp.WBP) # all trees >1" DBH
  recruitment_dat$otheradults1[i] <- nrow(tmp.notWBP) # all trees >1" DBH
  recruitment_dat$WBPadults4[i] <- nrow(subset(tmp.WBP, DIA > 4.0)) # number of trees >4" DBH
  recruitment_dat$otheradults4[i] <- nrow(subset(tmp.notWBP, DIA > 4.0)) 
  recruitment_dat$WBPadults8[i] <- nrow(subset(tmp.WBP, DIA > 8.0)) # number of trees >8" DBH
  recruitment_dat$otheradults8[i] <- nrow(subset(tmp.notWBP, DIA > 8.0))                               
  recruitment_dat$cumDIA.WBP[i] <- sum(tmp.WBP$DIA)
  recruitment_dat$cumDIA.others[i] <- sum(tmp.notWBP$DIA)
}

table(recruitment_dat[, c("recruits1", "PAwbp")]) # PAwbp is presence/absence at time 2, including dead trees
table(recruitment_dat[, c("WBPadults1", "recruits1")])
table(recruitment_dat[, c("WBPadults4", "recruits1")])
table(recruitment_dat[, c("WBPadults8", "recruits1")])


### last thing that needs to be done for recruitment subkernel
### get size distribution of recruits (ingrowth)
## it would be better to make sure size distribution is taken from the exact same set of trees that get put into ZIP model
## but it probably doesn't make a difference 
recruits <- subset(tree_iw, RECONCILECD %in% c(1,2) & SPCD == 101)
r_sizemean <- mean(log(recruits$DIA))
r_sizesd <- sd(log(recruits$DIA))

recruitment_dat$WBPadults148 <- rowSums(recruitment_dat[, c("WBPadults1", "WBPadults4", "WBPadults8")])

recruitment_dat1 <- recruitment_dat[complete.cases(recruitment_dat),]

# export ------------------------------------------------------------------
write.csv(recruitment_dat, "data_processed/WBP_recruit.csv")

save(r_sizemean, r_sizesd, file = "models//recrstats.Rdata")
