library(dplyr)

load_census_fips <- function(){
  census_fips <- xml2::read_html("https://www.census.gov/geo/reference/ansi_statetables.html") %>% 
    rvest::html_nodes(xpath = '//*[@id="middle-column"]/div/div[1]/div[2]/table') %>%
    rvest::html_table(header = TRUE)
  census_fips <- census_fips[[1]]
  names(census_fips) <- c("name", "fips", "abbr")
  census_fips$name_lower <- tolower(census_fips$name)
  census_fips$conus <- ifelse(census_fips$abbr %in% c("HI","AK"), FALSE, TRUE)
  assign("census_fips", census_fips, envir = .GlobalEnv)
}
