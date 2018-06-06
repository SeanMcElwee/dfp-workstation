
# set the working directory to the branched repository
setwd("~/Desktop/Data For Progress/US Mapping/")

# load the functions
source("usa_dfp_map_functions.R")

# load the mapping function
usa_dfp_map(
  states_data = read.csv("sample_data.csv"), # data frame that contains a column entitled "abbr"
  color_value = "trump_approval_april", # within data frame, column name of value you want to colors for as character
  alpha_value = NULL, # within data frame, column name of value you want to alpha transparencies for as character; default is NULL
  title = "Trump Approval Rating", # title name; default is NULL
  subtitle = NULL, # subtitle name; default is NULL
  font = "Montserrat", # font of choice; limited selection at the moment
  color_scheme = "orange_to_green", # choose the color scheme for the map default is "orange_to_green" also supports "brown_to_purple"
  limits = c(0.2, 0.8), # the range of the color scale; e.g. c(-1, 1); default is c(0.2, 0.8)
  save_plot = TRUE, # do you want to save the plot (TRUE) or do you want to just display the plot (FALSE)?
  output_folder = NULL  # output folder destination; leave NULL for same folder
)

# appends the branding to the bottom of the produced map
append_dfp_branding(
  plot_location = "trump_approval_april.png", # this is the full path to the plot produced in the first step
  annotation_text = "data retrieved from\nmorning consult 2018" # this is the annotation to be attached; usually data source
)
