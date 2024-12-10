# heroldji.github.io
First Repository for Pages
## Project title
## Biogeography Final Project
---
title: "Mapping Soil Properties: SSURGO vs. FIA"
author: "Joslyn Herold"
date: "12/09/2024"
output:
  html_document:
    css: screen.css
    toc: true
    toc_depth: 2
    toc_float: false
    self_contained: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

## Introduction

Understanding soil properties spatially across the eastern US would be beneficial for assessing soil as a factor in tree species distribution. There are two possible databases that can be used to map soil properties. SSURGO contains data collected by the National Cooperative Soil Survey. The Forest inventory and Analysis database (FIA) also contains soil data from each grid where they collect tree inventory data. In order to compare these two databases, I am generating maps of soil properties from both datasets and comparing them. For the purposes of this project, I will be comparing mineral grain size data, soil organic matter, and soil pH in a 1:1 ratio with water.

## Objectives

1.  Plot SSURGO pH, % organic matter, % clay, % silt, %fine sand, and % coarse sand for the eastern 31 United States.
2.  Plot FIA pH, % organic carbon, % coarse mineral material for the eastern 31 United States.
3.  Compare maps and datasets.

## Results

### Part 1: SSURGO Data

```{r, echo=TRUE,results='hide',fig.keep='all'}
library('tidyverse')
library('devtools')
library('rnaturalearthhires')
library('rnaturalearth')
library('rnaturalearthdata')
library('dplyr')
library('ggplot2')
library('tidyr')
library('classInt')
library('sf')
library('gridExtra')
library('rFIA')

setwd("E:/biogeog")

FIADB <- readRDS("FIA_tree_master1.RDS")
soils <- na.omit(readRDS("dfm_env_soil_clean.RDS"))
usa <- ne_states(country="united states of america",returnclass = "sf")
eastusa <- usa%>%
  filter(longitude>=-95)

soils.1 <- soils%>%
  group_by(GRID_LON,GRID_LAT)%>%
  summarize(Organics=om,
            pH=ph1to1h2o,
            silt=silttotal,
            clay=claytotal,
            sandsmall=(sandvf+sandfine+sandmed),
            sandlarge=(sandco+sandvc))


p1 <- ggplot(data = eastusa) +
  theme_bw()+
  geom_sf(fill="grey90") +
  geom_point(data=soils.1, aes(x=GRID_LON, y=GRID_LAT, color=Organics),size=0.8)+
  labs(title="Soil Organic Matter in Eastern US", x="Longitude",y="Latitude")+
  scale_color_gradient(low="gold",high="#dd1c77", name= "Organic Matter %")

p2 <- ggplot(data = eastusa) +
  theme_bw()+
  geom_sf(fill="grey90") +
  geom_point(data=soils.1, aes(x=GRID_LON, y=GRID_LAT, color=pH),size=0.7)+
  labs(title="Soil pH in Eastern US", x="Longitude",y="Latitude")+
  scale_color_gradient(low="gold",high="#dd1c77", name= "pH")

grid.arrange(p1,p2, nrow=2, ncol=1,heights=c(1,1))

p3 <- ggplot(data = eastusa) +
  theme_bw()+
  geom_sf(fill="grey90") +
  geom_point(data=soils.1, aes(x=GRID_LON, y=GRID_LAT, color=silt),size=0.7)+
  labs(title="Total Silt in Eastern US", x="Longitude",y="Latitude")+
  scale_color_gradient(low="gold",high="#dd1c77", name= "Silt %")

p4 <- ggplot(data = eastusa) +
  theme_bw()+
  geom_sf(fill="grey90") +
  geom_point(data=soils.1, aes(x=GRID_LON, y=GRID_LAT, color=clay),size=0.7)+
  labs(title="Total Clay in Eastern US", x="Longitude",y="Latitude")+
  scale_color_gradient(low="gold",high="#dd1c77", name= "Clay %")

p5 <- ggplot(data = eastusa) +
  theme_bw()+
  geom_sf(fill="grey90") +
  geom_point(data=soils.1, aes(x=GRID_LON, y=GRID_LAT, color=sandsmall),size=0.7)+
  labs(title="Total Fine-Med Sand in Eastern US", x="Longitude",y="Latitude")+
  scale_color_gradient(low="gold",high="#dd1c77", name= "Sand (Fine-Med) %")

p6 <- ggplot(data = eastusa) +
  theme_bw()+
  geom_sf(fill="grey90") +
  geom_point(data=soils.1, aes(x=GRID_LON, y=GRID_LAT, color=sandlarge),size=0.7)+
  labs(title="Total Coarse Sand in Eastern US", x="Longitude",y="Latitude")+
  scale_color_gradient(low="gold",high="#dd1c77", name= "Sand (Coarse) %")

grid.arrange(p4,p3,p5,p6, nrow=2,ncol=2, widths=c(1,1),heights=c(1,1))


```

### Part 2: FIA Soil Data

```{r, echo=TRUE, results='hide'}
# Download soil data for eastern 31 states
FIASOILS <- readFIA(states= c("AL","AR","CT","DE","FL","GA", "IL","IN","IA","KY",
                             "LA","ME","MD","MA","MI","MS","MO","NH","NJ","NY",
                             "NC","OH","PA","RI","SC","TN","VT","VA","WV"),
                   tables = c("PLOTSNAP","SOILS_EROSION","SOILS_LAB",
                              "SOILS_SAMPLE_LOC" ,
                              "SOILS_VISIT"),
                   dir="D:/biogeog",
                   common=F)
# Convert database to dataframes that I can work with
fiasoilse <- as.data.frame(FIASOILS$SOILS_EROSION)
fiasoilsl <- as.data.frame(FIASOILS$SOILS_LAB)
fiasoilss <- as.data.frame(FIASOILS$SOILS_SAMPLE_LOC)
fiasoilsv <- as.data.frame(FIASOILS$SOILS_VISIT)
fiasoilsplot <- as.data.frame(FIASOILS$PLOTSNAP)

#successful joining of 2 dfs with soil properties required for comparison
soil_properties <- left_join(fiasoilsl,fiasoilss)

print(class(FIADB$PLT_CN))
print(class(soil_properties$PLT_CN))
print(head(FIADB$PLT_CN))
print(head(soil_properties$PLT_CN))

df3 <- left_join(soil_properties, FIADB)
soilmap.df <- df3%>% 
  group_by(LON,LAT)%>%
  summarise(pH=PH_H2O,
            coarse=COARSE_FRACTION_PCT,
            orgcarbon=C_ORG_PCT)

print(head(soilmap.df))

```
## Discussion

Unfortunately, I was unable to map the FIA soil properties. I am still exploring why, but PLT_CN's, which matched between the class FIA data and the FIA soil data I downloaded, lacked any LAT/LON. I tried multiple ways to get LAT/LON data, including downloading PLOTSNAP files. Due to this, I was unable to compare the FIA and SSURGO databases via mapping.

However, there are some notable differences in the data sets! The SSURGO database contains much more grain size information; where the FIA soil data lacked in this, it made up for it with data on soil texture with depth. I found the SSURGO documentation easier to follow and understand, which may be due to its relatively lower complexity when compared with the FIADB. At the moment, the SSURGO database has more soil data t explore than the FIA soil properties. When considering tree species distributions, it would likely be easier to use the FIA soil data, as locations of soil samples are directly correlated with tree observations, so I hope to successfully integrate the FIA soil data with the existing FIA data that we use for this class.

## Resources

Soil Survey Staff, Natural Resources Conservation Service, United States Department of Agriculture. Web Soil Survey. Available online at <https://websoilsurvey.nrcs.usda.gov/>. Accessed [12/09/2024].

Woodall, Christopher W.; Conkling, Barbara L.; Amacher, Michael C.; Coulston, John W.; Jovan, Sarah; Perry, Charles H.; Schulz, Beth; Smith, Gretchen C.; Will-Wolf, Susan. 2010. The Forest Inventory and Analysis Database Version 4.0: Database Description and Users Manual for Phase 3. Gen. Tech. Rep. NRS-61. Newtown Square, PA: U.S. Department of Agriculture, Forest Service, Northern Research Station. 180 p

