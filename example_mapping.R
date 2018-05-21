
# set the working directory to the branched repository
setwd()

# load the 
source("usa_dfa_map_functions.R")

# this is the function 
usa_dfp_map(
  states_data = read.csv("sample_data.csv"), # data frame that contains a column entitled "abbr"
  color_value = "trump_approval_april", # within data frame, column name of value you want to colors for as character
  alpha_value = NULL, # within data frame, column name of value you want to alpha transparencies for as character; default is NULL
  title = "Trump Approval (Morning Consult, April 2018)", # title name; default is NULL
  subtitle = NULL, # subtitle name; default is NULL
  font = "Arial", # font of choice; limited selection at the moment
  color_scheme = "orange_to_green", # choose the color scheme for the map default is "orange_to_green" also supports "brown_to_purple"
  limits = c(0.2, 0.8), # the range of the color scale; e.g. c(-1, 1); default is c(0.2, 0.8)
  save_plot = FALSE, # do you want to save the plot (TRUE) or do you want to just display the plot (FALSE)?
  output_folder = NULL # output folder destination; leave blank for same folder
)
