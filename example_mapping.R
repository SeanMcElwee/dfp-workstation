
# set the working directory to the branched repository
setwd()

# load the 
source("usa_dfa_map_functions.R")

# this is the function 
usa_dfp_map(
  states_data = , # data frame that contains a column entitled "abbr"
  color_value = , # within data frame, column name of value you want to colors for
  alpha_value = , # within data frame, column name of value you want to alpha transparencies for; default is NULL
  title = , # title name; default is NULL
  subtitle = , # subtitle name; default is NULL
  font = , # font of choice
  color_scheme = , # choose the color scheme for the map default is "orange_to_green" also supports "brown_to_purple"
  limits = , # the range of the color scale; e.g. c(-1, 1); default is c(0.2, 0.8)
  save_plot = , # do you want to save the plot (TRUE) or do you want to just display the plot (FALSE)?
  output_folder = # output folder destination; leave blank for same folder
)