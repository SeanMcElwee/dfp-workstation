library(ggplot2)
library(rgdal)
library(maptools)
library(ggmap)
library(scales)

setwd("~/Desktop/Data For Progress/US Mapping/")

# load in function to load census fips data
source("load_census_fips.R")

# load census fips data
load_census_fips()

# load in the csv
polling_data <- read.csv("data/polling_data.csv")

# join it into the census basetable
polling_data <- plyr::join(census_fips, polling_data)

fixup <- function(usa,alaskaFix,hawaiiFix){
  
  alaska=usa[usa$NAME=="Alaska",]
  alaska = prelim_fix(alaska,alaskaFix)
  proj4string(alaska) <- proj4string(usa)
  
  hawaii = usa[usa$NAME=="Hawaii",]
  hawaii = prelim_fix(hawaii,hawaiiFix)
  proj4string(hawaii) <- proj4string(usa)
  
  usa = usa[! usa$NAME %in% c("Alaska","Hawaii"),]
  usa = rbind(usa,alaska,hawaii)
  
  return(usa)
  
}
prelim_fix <- function(object,params){
  r=params[1];scale=params[2];shift=params[3:4]
  object = elide(object,rotate=r)
  size = max(apply(bbox(object),1,diff))/scale
  object = elide(object,scale=size)
  object = elide(object,shift=shift)
  object
}

# tranformations 
states <- readOGR(dsn = "cb_2017_us_state_20m",layer="cb_2017_us_state_20m_2")
states <- subset(states, NAME %in% census_fips$name) 
states <- spTransform(states, CRS("+init=epsg:2163"))
#states = fixup(states,c(-35,1.8,-2800000,-2600000),c(-35,1,6800000,-1600000))
states <- fixup(states, c(-35,1.75,-3500000,-2000000),c(-35,1,4500000,-1200000))
states <- spTransform(states, CRS("+init=epsg:4326"))

id_lookup <- data.frame("id"=rownames(states@data), "STUSPS"=states@data$STUSPS)
nationwide <- unionSpatialPolygons(states, rep(1,length(states)))
states_f <- fortify(states)
states_f$abbr <- id_lookup[match(states_f$id, id_lookup$id),]$STUSPS
nationwide <- fortify(nationwide)

produce_map <- function(states_data, column, title=NULL, subtitle=NULL, font="Verdana"){
  states <- plyr::join(states_f, states_data)
  ggplot() +
    geom_polygon(data=nationwide, aes(x=long, y=lat, group=group), fill = NA, color = "black", size = 1.25) +
    geom_polygon(data=states, aes(x=long, y=lat, group=group, fill = get(column)/100), color = "white", size = 0.075) +
    scale_fill_gradientn(
      name = "", label=percent, colors=c("#ff8000","#FFFFFF","#006600"), values = c(0, 0.45, 0.55, 1),
      guide = guide_colorbar(direction = "horizontal", barheight = 1.5, barwidth = 45, guide = guide_legend()),
      limits = c(0.2,0.8), na.value = "#006600") +
    coord_map() +
    labs(title = title, subtitle = toupper(subtitle)) +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 24),
      plot.subtitle = element_text(hjust = 0.5, size = 16, face = "bold"),
      text = element_text(family = font),
      legend.text = element_text(size = 12),
      legend.position="bottom",
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
}

png(filename = "expand_rental.png", width = 5 * diff(states@bbox[,1]), height = 5 * diff(states@bbox[,2]), units = "px")
produce_map(polling_data, "expand_rental", title = "expand_rental", font = "Arial")
dev.off()

png(filename = "housing_cand.png", width = 5 * diff(states@bbox[,1]), height = 5 * diff(states@bbox[,2]), units = "px")
produce_map(polling_data, "housing_cand", title = "housing_cand", font = "Arial")
dev.off()

