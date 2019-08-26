library(rtweet)
library(tidyverse)
library(glue)

handles_orig <- read.csv("./data/candidate_twitter_handles.csv",
                         na.strings = "",
                         colClasses = "character", 
                         encoding = "UTF-8")

handles_only <- handles_orig %>% 
  filter(!is.na(twitter_handle) & str_length(twitter_handle) <= 15) %>% 
  mutate(twitter_handle = tolower(twitter_handle))

handles_only_char_list <- as.character(handles_only$twitter_handle[1:length(handles_only$twitter_handle)])

#function from https://stackoverflow.com/questions/42025979/avoid-rate-limit-with-rtweet-get-timeline
get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}

#collect 100 past tweets from full list of candidates
candidate_timelines <- get_timeline_unlimited(handles_only_char_list, 100)

#filter tweets only past June 1st
candidate_timelines_short <-
  candidate_timelines %>% 
  select(user_id:text, favourites_count, retweet_count, is_retweet, is_quote, source, hashtags, media_url, media_type) %>%
  filter(created_at > lubridate::ymd("2019-06-01")) %>% 
  mutate(screen_name = tolower(screen_name)) 


#join to get the political party associated to account
candidate_tweets <- 
candidate_timelines_short %>% 
  left_join(handles_only, by = c("screen_name" = "twitter_handle"))


#saveRDS(candidate_tweets, file = "./data/tweets.rds")


tweets <- readRDS("./data/tweets.rds")


glimpse(tweets)


# to stream tweets until the end of the election:----
## set stream time

## search terms
users <- handles_only_char_list

## stream the tweets and write to "data/stream.json"
stream_tweets(
  q = users,
  timeout = twoweeks,
  file_name = json_file,
  parse = FALSE,
)

stream_tweets(q = handles_only_char_list[1], timeout = Inf, parse = FALSE )

?stream_tweets

## parse json data and convert to tibble
rt <- parse_stream("tweets_stream.json")




