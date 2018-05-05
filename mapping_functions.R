library(scales)
library(ggplot2)
library(fiftystater)

# set working directory
setwd("~/Desktop/Data For Progress/US Mapping/")

# load in function to load census fips data
source("load_census_fips.R")

# load census fips data
load_census_fips()

# load in the csv
expand_rental <- read.csv("data/expand_rental.csv")

# join it into the census basetable
expand_rental <- plyr::join(census_fips, expand_rental)

produce_map <- function(data, title=NULL, subtitle=NULL, font="Verdana"){
  ggplot(data, aes(map_id = name_lower)) + 
    geom_map(aes(fill = Mean.Success/100), map = fifty_states, color = "white", size = 0.05) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    scale_fill_gradientn(name = "", label=percent, colors=c("#ff8000","#FFFFFF","#006600"), guide = guide_colorbar(direction = "horizontal", barheight = 0.75, barwidth = 30),  limits = c(0.2,0.8), na.value = "#006600") +
    scale_x_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "", title = title, subtitle = subtitle) +
    theme(
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14),
      text = element_text(family = font),
      legend.position="bottom"
    )  
}

pdf(filename = "rental.png", units = "px")
produce_map(data = expand_rental, title = "Support For Expanded Rental Programs", subtitle = "SUBTITLE TEXT HERE", font = "Georgia")
dev.off()

getwd()
