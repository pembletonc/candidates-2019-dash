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
    tweets <- get_timeline(users,
                           n,
                           check = FALSE) 
    #note: set since_id later to whatever is the most recent tweet in the RDS
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue("Getting data for {length(users_first)} users"))
      
      tweets_first <- get_timeline(users_first,
                                   n,
                                   check = FALSE,
                                   parse = FALSE)
      
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

#collect up to 200 past tweets from full list of candidates from aug 1st
candidate_timelines <- get_timeline_unlimited(handles_only_char_list, 200)



#filter tweets only past June 1st
candidate_timelines_short <-
  candidate_timelines %>%
  select(user_id:text, favourites_count, retweet_count, is_retweet, is_quote, 
         source, hashtags, media_url, media_type) %>%
  filter(created_at > "2019-08-01") %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  arrange(desc(created_at))
  
  
#join to get the political party associated to account
candidate_tweets <- 
  candidate_timelines_short %>% 
  left_join(handles_only, by = c("screen_name" = "twitter_handle"))


#saveRDS(candidate_tweets, "./data/tweets.rds")


get_user_tweets <- function(n){
  
  tweets_saved <- readRDS("./data/tweets.rds")
  #since_id() function not working so doing this to get latest tweet saved
  since_id <- as.character(tweets_saved %>% top_n(status_id, n = 1) %>% select(status_id))
  
  #thanks - from https://stackoverflow.com/questions/42025979/avoid-rate-limit-with-rtweet-get-timeline
  #function to get timelines for each user set at n amount
  get_timeline_unlimited <- function(users, n){
    

    if (length(users) ==0){
      return(NULL)
    }
    
    rl <- rate_limit(query = "get_timeline")
    
    if (length(users) <= rl$remaining){
      print(glue("Getting data for {length(users)} users"))
      tweets <- get_timeline(users,
                             n,
                             check = FALSE,
                             since_id = since_id)  
    }else{
      
      if (rl$remaining > 0){
        users_first <- users[1:rl$remaining]
        users_rest <- users[-(1:rl$remaining)]
        print(glue("Getting data for {length(users_first)} users"))
        
        tweets_first <- get_timeline(users_first,
                                     n,
                                     check = FALSE,
                                     parse = FALSE, 
                                     since_id = since_id
                                     )
        #max_id set for August 1st
        
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
  
  #load in the handles of the candidates 
  handles_orig <- read.csv("./data/candidate_twitter_handles.csv",
                           na.strings = "",
                           colClasses = "character", 
                           encoding = "UTF-8")
  
  #make sure that the handles are the right char length (some links in the mix)
  handles_only <- handles_orig %>% 
    filter(!is.na(twitter_handle) & str_length(twitter_handle) <= 15) %>% 
    mutate(twitter_handle = tolower(twitter_handle))
  
  #convert to char only
  handles <- as.character(handles_only$twitter_handle[1:length(handles_only$twitter_handle)])
  #import new tweets using function
  
  tweets <- get_timeline_unlimited(handles, n)
  
  #clean imported tweets
  tweets <- tweets %>% 
  select(user_id:text, favourites_count, retweet_count, is_retweet, is_quote, source, hashtags, media_url, media_type) %>%
    filter(created_at > lubridate::ymd("2019-08-01")) %>% #min date Aug 1st
    mutate(screen_name = tolower(screen_name)) 
  
  #ensure newly imported tweets don't include doubles
  new_tweets_only <- anti_join(tweets, tweets_saved,  by = "status_id")
  
  print(paste("There are", nrow(new_tweets_only), "new tweets to add"))
  
  #join updated tweets to party and riding
  new_tweets_only <- new_tweets_only %>% 
    left_join(handles_only, by = c("screen_name" = "twitter_handle"))
  
  
  updated_tweets <- bind_rows(tweets_saved, new_tweets_only)
  
  saveRDS(updated_tweets, "./data/tweets1.rds")

  
  return(updated_tweets)
   
}

#e.g. running this soon after has 54 new tweets, later has 800 etc.
test <- get_user_tweets(100)




