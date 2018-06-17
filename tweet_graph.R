# ===============================================================================================
# Load in Libraries & Set Working Directory
# ===============================================================================================

# load in libraries
library(dfp)
library(rvest)
library(reshape2)
library(stringr)

# set working directory
setwd("~/Desktop/Tech Resources/GitHub Projects/dfp-workstation/senator_tweets/")

# ===============================================================================================
# Load in Datasets
# ===============================================================================================

# load in congress metadata (lots of information here)
congress_metadata <- read.csv("https://theunitedstates.io/congress-legislators/legislators-current.csv", stringsAsFactors = FALSE)

# load in database of all senator tweets and do minor cleaning
senator_tweets <- read.csv("data/sen.tweets.full.2.csv", stringsAsFactors = FALSE)
senator_tweets$X. <- NULL
senator_tweets$ratio <- NULL
senator_tweets$date <- as.Date(senator_tweets$date, "%m/%d/%Y")

# load in senator metadata 
senator_metadata <- read_html("https://www.socialseer.com/resources/us-senator-twitter-accounts/") %>% 
  html_nodes(xpath = '//*[@id="post-109"]/div/table[1]') %>%
  html_table(header = TRUE)
senator_metadata <- senator_metadata[[1]]

# ===============================================================================================
# Clean Senator Metadata Table
# ===============================================================================================

# restructure the table
names(senator_metadata) <- c("state", "senator", "official", "office", "campaign")
split_names <- str_split(senator_metadata$senator, ", ")
senator_metadata$first_name <- sapply(split_names, `[[`, 2)
senator_metadata$last_name <- sapply(split_names, `[[`, 1)
senator_metadata$senator <- NULL

# melt the table and remove empty handles
senator_metadata <- senator_metadata %>%
  melt(
    id.vars = c("state", "first_name", "last_name"),
    measure.vars = c("official", "office", "campaign"), 
    variable.name = "account_type", value.name = "handle"
  )
senator_metadata <- data.frame(lapply(senator_metadata, as.character), stringsAsFactors=FALSE)
senator_metadata <- senator_metadata[senator_metadata$handle != "",]

# correct twitter handles
senator_metadata$last_name <- str_replace(senator_metadata$last_name, "Jomes", "Jones")
senator_metadata$handle <- senator_metadata$handle %>%
  str_replace("SenatorSanders", "SenSanders") %>%
  str_replace("jiminhofe", "JimInhofe") %>%
  str_replace("SenDavidPerdue", "sendavidperdue") %>%
  str_replace("senmarkey", "SenMarkey") %>%
  str_replace("SenOrrinHatch", "senorrinhatch") %>%
  str_replace("SenRobPortman", "senrobportman")

# make manual corrections to replace nicknames with formal names
senator_metadata[senator_metadata$last_name %in% c("Risch","Inhofe"),]$first_name <- "James"
senator_metadata[senator_metadata$last_name %in% c("Schumer", "Grassley"),]$first_name <- "Charles"
senator_metadata[senator_metadata$last_name %in% c("Cardin","Sasse"),]$first_name <- "Benjamin"
senator_metadata[senator_metadata$last_name %in% c("Portman", "Casey"),]$first_name <- "Robert"
senator_metadata[senator_metadata$last_name == "Carper",]$first_name <- "Thomas"
senator_metadata[senator_metadata$last_name == "Crapo",]$first_name <- "Michael"
senator_metadata[senator_metadata$last_name == "Isakson",]$first_name <- "John"
senator_metadata[senator_metadata$last_name == "Durbin",]$first_name <- "Richard"
senator_metadata[senator_metadata$last_name == "Markey",]$first_name <- "Edward"
senator_metadata[senator_metadata$last_name == "Hassan",]$first_name <- "Margaret"
senator_metadata[senator_metadata$last_name == "Kaine",]$first_name <- "Timothy"
senator_metadata[senator_metadata$last_name == "Enzi",]$first_name <- "Michael"
senator_metadata[senator_metadata$last_name == "Toomey",]$first_name <- "Patrick"
senator_metadata[senator_metadata$last_name == "Sanders",]$first_name <- "Bernard"
congress_metadata[congress_metadata$first_name == "Shelley",]$first_name <- "Shelley Moore"
congress_metadata[congress_metadata$last_name == "Menéndez",]$last_name <- "Menendez"
senator_metadata[senator_metadata$last_name == "Menendez",]$first_name <- "Robert"
senator_metadata[senator_metadata$last_name == "Reed",]$first_name <- "John"

# remove and replace Cochran
senator_metadata <- senator_metadata[senator_metadata$last_name != "Cochran",]

# add Hyde-Smith
senator_metadata <- senator_metadata %>%
  rbind(c("Mississippi","Cindy","Hyde-Smith","official","senhydesmith")) %>%
  rbind(c("Mississippi","Cindy","Hyde-Smith","personal","cindyhydesmith"))
names(senator_metadata)[names(senator_metadata) == "state"] <- "state_long"

# create a full name column
senator_metadata$full_name <- paste(senator_metadata$first_name, senator_metadata$last_name)

# ===============================================================================================
# Bundle the Datasets Together
# ===============================================================================================

# merge the metadata tables
merged_metadata <- plyr::join(senator_metadata, congress_metadata, by = c("first_name", "last_name"))

# add farmstate and medicrate-for-all support flags
merged_metadata$farmstate <- ifelse(
  merged_metadata$state_long %in% c("Wisconsin","Tennessee","Arkansas","Nebraska","South Dakota","Kansas","North Dakota","Iowa","Texas","Illinois","Oklahoma","Missouri","Indiana","Montana","New Mexico","Ohio","Wyoming","Minnesota","Kentucky","Colorado"),
  "Farm","Not Farm"
)
merged_metadata$m4a_eval <- 
  ifelse(merged_metadata$last_name %in% c("Barrasso", "Cornyn", "Sasse", "Rubio", "Roberts", "Johnson"), "Against", 
  ifelse(merged_metadata$last_name %in% c("Harris", "Booker", "Heinrich", "Hirono", "Leahy", "Udall", "Blumenthal", "Gillibrand", "Merkley", "Markey", "Sanders", "Warren", "Whitehouse", "Baldwin", "Shaheen"), "Support", 
  ifelse(merged_metadata$last_name %in% c("Cardin", "Kaine", "Paul", "Lee", "Graham","Daines", "Bennet"), "Neutral", NA)))

# bundle the data together
senator_tweets_final <- merge(merged_metadata, senator_tweets, by.x = "handle", by.y = "username")

# make final correction to allow the data to be processed
senator_tweets_final$tweet <- str_replace(senator_tweets_final$tweet, "©","")

# plot the bargraph
issue_bargraph(c("gaza"), senator_tweets_final, scale_by_total = TRUE, save_local = FALSE)
