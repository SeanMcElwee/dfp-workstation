library(rvest)
library(reshape2)
library(stringr)

setwd("~/Desktop/Data For Progress/U.S. Senator Tweets/")

congress_metadata <- read.csv("https://theunitedstates.io/congress-legislators/legislators-current.csv", stringsAsFactors = FALSE)
senator_tweets <- read.csv("sen.tweets.full.csv", stringsAsFactors = FALSE)
senator_tweets$X.1 <- NULL
senator_tweets$X <- NULL
senator_tweets$ratio <- NULL

senator_tweets$date <- as.Date(senator_tweets$date, "%m/%d/%Y")

senator_metadata_2 <- read_html("https://www.socialseer.com/resources/us-senator-twitter-accounts/") %>% 
  html_nodes(xpath = '//*[@id="post-109"]/div/table[1]') %>%
  html_table(header = TRUE)
senator_metadata_2 <- senator_metadata_2[[1]]

# restructure the table
names(senator_metadata_2) <- c("state", "senator", "official", "office", "campaign")
split_names <- str_split(senator_metadata_2$senator, ", ")
senator_metadata_2$first_name <- sapply(split_names, `[[`, 2)
senator_metadata_2$last_name <- sapply(split_names, `[[`, 1)
senator_metadata_2$senator <- NULL

# melt the table and remove empty handles
senator_metadata_2 <- senator_metadata_2 %>%
  melt(
    id.vars = c("state", "first_name", "last_name"),
    measure.vars = c("official", "office", "campaign"), 
    variable.name = "account_type", value.name = "handle"
  )
senator_metadata_2 <- data.frame(lapply(senator_metadata_2, as.character), stringsAsFactors=FALSE)
senator_metadata_2 <- senator_metadata_2[senator_metadata_2$handle != "",]

# correct twitter handles
senator_metadata_2$last_name <- str_replace(senator_metadata_2$last_name, "Jomes", "Jones")
senator_metadata_2$handle <- senator_metadata_2$handle %>%
  str_replace("SenatorSanders", "SenSanders") %>%
  str_replace("jiminhofe", "JimInhofe") %>%
  str_replace("SenDavidPerdue", "sendavidperdue") %>%
  str_replace("senmarkey", "SenMarkey") %>%
  str_replace("SenOrrinHatch", "senorrinhatch") %>%
  str_replace("SenRobPortman", "senrobportman")

paste0(senator_metadata_2$first_name, senator_metadata_2$last_name)[
  !(paste0(senator_metadata_2$first_name, senator_metadata_2$last_name) %in% paste0(congress_metadata$first_name, congress_metadata$last_name))]

senator_metadata_2[senator_metadata_2$last_name %in% c("Risch","Inhofe"),]$first_name <- "James"
senator_metadata_2[senator_metadata_2$last_name %in% c("Schumer", "Grassley"),]$first_name <- "Charles"
senator_metadata_2[senator_metadata_2$last_name %in% c("Cardin","Sasse"),]$first_name <- "Benjamin"
senator_metadata_2[senator_metadata_2$last_name %in% c("Portman", "Casey"),]$first_name <- "Robert"
senator_metadata_2[senator_metadata_2$last_name == "Carper",]$first_name <- "Thomas"
senator_metadata_2[senator_metadata_2$last_name == "Crapo",]$first_name <- "Michael"
senator_metadata_2[senator_metadata_2$last_name == "Isakson",]$first_name <- "John"
senator_metadata_2[senator_metadata_2$last_name == "Durbin",]$first_name <- "Richard"
senator_metadata_2[senator_metadata_2$last_name == "Markey",]$first_name <- "Edward"
senator_metadata_2[senator_metadata_2$last_name == "Hassan",]$first_name <- "Margaret"
senator_metadata_2[senator_metadata_2$last_name == "Kaine",]$first_name <- "Timothy"
senator_metadata_2[senator_metadata_2$last_name == "Enzi",]$first_name <- "Michael"
senator_metadata_2[senator_metadata_2$last_name == "Toomey",]$first_name <- "Patrick"
senator_metadata_2[senator_metadata_2$last_name == "Sanders",]$first_name <- "Bernard"
congress_metadata[congress_metadata$first_name == "Shelley",]$first_name <- "Shelley Moore"
congress_metadata[congress_metadata$last_name == "Menéndez",]$last_name <- "Menendez"
senator_metadata_2[senator_metadata_2$last_name == "Menendez",]$first_name <- "Robert"
senator_metadata_2[senator_metadata_2$last_name == "Reed",]$first_name <- "John"

# remove and replace Cochran with Hyde-Smith
senator_metadata_2 <- senator_metadata_2[senator_metadata_2$last_name != "Cochran",]

senator_metadata_2 <- senator_metadata_2 %>%
  rbind(c("Mississippi","Cindy","Hyde-Smith","official","senhydesmith")) %>%
  rbind(c("Mississippi","Cindy","Hyde-Smith","personal","cindyhydesmith"))
names(senator_metadata_2)[names(senator_metadata_2) == "state"] <- "state_long"

senator_metadata_2$full_name <- paste(senator_metadata_2$first_name, senator_metadata_2$last_name)

merged <- plyr::join(senator_metadata_2, congress_metadata, by = c("first_name", "last_name"))

merged$farmstate <- ifelse(
  merged$state_long %in% c("Wisconsin","Tennessee","Arkansas","Nebraska","South Dakota","Kansas","North Dakota","Iowa","Texas","Illinois","Oklahoma","Missouri","Indiana","Montana","New Mexico","Ohio","Wyoming","Minnesota","Kentucky","Colorado"),
  "Farm","Not Farm"
)

senator_tweets_final <- merge(merged, senator_tweets, by.x = "handle", by.y = "username")

# SOME DUPLICATION, TAKE NOTE!!!

senator_tweets_final$tweet <- str_replace(senator_tweets_final$tweet, "©","")

senator_tweets_final$m4a_eval <- 
  ifelse(senator_tweets_final$last_name %in% c("Barrasso", "Cornyn", "Sasse", "Rubio", "Roberts", "Johnson"), "Against", 
  ifelse(senator_tweets_final$last_name %in% c("Harris", "Booker", "Heinrich", "Hirono", "Leahy", "Udall", "Blumenthal", "Gillibrand", "Merkley", "Markey", "Sanders", "Warren", "Whitehouse", "Baldwin", "Shaheen"), "Support", 
  ifelse(senator_tweets_final$last_name %in% c("Cardin", "Kaine", "Paul", "Lee", "Graham","Daines", "Bennet"), "Neutral", NA)))
  

library(dplyr)
library(ggplot2)
library(extrafont)
library(scales)

issue_terms = c("medicare-for-all", "medicareforall", "medicare for all", "single-payer")
issue_terms = c("medicare","medicaid","healthcare","health care")
issue_terms = c("putin","russia")
scale_by_total = TRUE

issue_barplot <- function(issue_terms, scale_by_total=FALSE, save_local=FALSE) { 
  
  senator_tweets_final$tweet_exists <- TRUE
  
  senator_tweets_final$issue_flag <- senator_tweets_final$tweet %>%
    tolower %>%
    str_detect(paste0(issue_terms, collapse = "|"))
  
  issue_data <- senator_tweets_final %>%
    dcast(last_name+party+state+m4a_eval~"total_mentions", value.var = "issue_flag", fun.aggregate = sum)
  
  issue_data$party <- ifelse(issue_data$party == "Democrat", "D", 
  ifelse(issue_data$party == "Republican", "R",
  ifelse(issue_data$party == "Independent", "I", "")))
  
  if(scale_by_total) {
    tweet_counts <- senator_tweets_final %>%
      dcast(last_name+party~"total_tweets", value.var = "tweet_exists", fun.aggregate = sum)
    
    issue_data$total_mentions <- issue_data$total_mentions/tweet_counts$total_tweets
  }
  
  issue_data$last_name <- issue_data$last_name %>%
    toupper 
  
  issue_data$last_name <- paste0(issue_data$last_name, " (", issue_data$party, "-", issue_data$state, ")")
  
  issue_data$last_name <- issue_data$last_name %>%
    factor(levels = issue_data$last_name[order(issue_data$party, issue_data$total_mentions)])

  #issue_data$total_mentions <- ifelse(issue_data$m4a_eval == "Against", issue_data$total_mentions * -1, issue_data$total_mentions)
  
  #dcast(issue_data, party ~ "Total Mentions", value.var = "total_mentions", fun.aggregate = sum)
  #dcast(tweet_counts, party ~ "Total Tweets", value.var = "total_tweets", fun.aggregate = sum)
  
  # issue_data <- issue_data %>% subset(total_mentions != 0)
  
  barplot <- ggplot(data = issue_data, aes(x=last_name, y=total_mentions, fill=party, alpha = factor(m4a_eval))) +
    geom_bar(stat="identity", width = 1)+ 
    coord_flip() +
    labs(
      title = toupper(paste0(if(scale_by_total){"Scaled "},"Senator Mentions of Terms")), 
      subtitle = paste0("'",paste0(issue_terms, collapse = "', '"), "'"), 
      x = "Senator", 
      y = if(scale_by_total){"Share of Term Mentions To All Tweets"}else{"Count of Term Mentions"}
    ) +
    # scale_alpha_discrete(name = "Farm State", range = c(1, 0.2)) +
    scale_alpha_discrete(name = "M4A Support", range = c (0.05, 1)) +
    # facet_grid(. ~ m4a_eval) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0),labels=if(scale_by_total){percent}) +
    scale_fill_manual(
      name = "Party",
      values = c("#0000cc", "#595959", "#cc0000"),
      breaks = c("Democrat", "Independent", "Republican"),
      labels = c("Democrat", "Independent", "Republican"),
      guide=FALSE
    ) + 
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 13),
      panel.background = element_blank(),
      text = element_text(family = "Montserrat"),
      axis.text.x = element_text(size = 9, face = "bold"),
      axis.text.y = element_text(size = 7),
      axis.ticks.length=unit(1, "mm"),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.line.x = element_line(color="black", size = 0.5)
    )
  
  if(save_local){
    ggsave(
      filename = paste0(if(scale_by_total){"scaled_"},str_replace_all(paste0(issue_terms, collapse = ""), " ", "_"), ".png"),
      width = 160, height = 280, units = "mm",
      scale = 1, dpi = 320
    )
  } else {
    print(barplot)
  }
}

?scale_alpha_discrete

issue_barplot(c("medicare","medicaid","healthcare","health care"), scale_by_total = TRUE, save_local = TRUE)
issue_barplot(c("medicare-for-all", "medicareforall", "medicare for all", "single-payer"), scale_by_total = TRUE, save_local = TRUE)
issue_barplot(c("netneutrality", "net neutrality"), scale_by_total = TRUE, save_local = TRUE)

subset(senator_tweets_final, full_name = "Van Hollen", select = "tweet")
