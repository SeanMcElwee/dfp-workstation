library(scales)

setwd("~/Desktop/Data For Progress/Mapping Functions/")

census_fips <- function(){
  census_fips <- xml2::read_html("https://www.census.gov/geo/reference/ansi_statetables.html") %>% 
    rvest::html_nodes(xpath = '//*[@id="middle-column"]/div/div[1]/div[2]/table') %>%
    rvest::html_table(header = TRUE)
  census_fips <- census_fips[[1]]
  names(census_fips) <- c("name", "fips", "abbr")
  census_fips$name_lower <- tolower(census_fips$name)
  assign("census_fips", census_fips, envir = .GlobalEnv)
}
census_fips()

expand_rental <- read.csv("data/expand_rental.csv")

expand_rental <- plyr::join(census_fips, expand_rental)

library(ggplot2)
library(fiftystater)
library(tigris)

??tigris
install.packages("sf")

library(tigris)
states
data("fifty_states") # this line is optional due to lazy data loading

range(expand_rental$Mean.Success)

# map_id creates the aesthetic mapping to the state name column in your data
ggplot(expand_rental, aes(map_id = name_lower)) + 
  # map points to the fifty_states shape data
#  geom_map(fill = NA, map = fifty_states, color = "black") + 
  geom_map(aes(fill = Mean.Success/100), map = fifty_states, color = NA, size = 0) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_fill_gradientn(name = "", label=percent, colors=c("#ff8000","#FFFFFF","#006600"), guide = "colourbar",  limits = c(0.2,0.8), na.value = "#006600") +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(panel.background = element_blank())

name = "Support For Expanded Rental Programs", 

??percent
