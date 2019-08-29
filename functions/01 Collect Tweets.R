
get_user_tweets <- function(n){
  
  library(rtweet)
  library(tidyverse)
  library(glue)
  
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

tweets_in_last <- function(tweets, d = 0, h = 0, m = 15, s = 0){
  tweets %>% 
    filter(created_at >= lubridate::now() - lubridate::hours(h+d * 24) - 
             lubridate::minutes(m) - lubridate::seconds(s))
}

get_tweet_blockquote <- function(screen_name, status_id, ..., null_on_error = TRUE, theme = "light") {
  oembed <- list(...)$oembed
  if (!is.null(oembed) && !is.na(oembed)) return(unlist(oembed))
  oembed_url <- glue::glue("https://publish.twitter.com/oembed?url=https://twitter.com/{screen_name}/status/{status_id}&omit_script=1&dnt=1&theme={theme}")
  bq <- possibly(httr::GET, list(status_code = 999))(URLencode(oembed_url))
  if (bq$status_code >= 400) {
    if (null_on_error) return(NULL)
    '<blockquote style="font-size: 90%">Sorry, unable to get tweet ¯\\_(ツ)_/¯</blockquote>'
  } else {
    httr::content(bq, "parsed")$html
  }
}


twemoji <- function(runes, width = "20px") {
  runes <- tolower(runes)
  runes <- gsub(" ", "-", runes)
  runes <- sub("-fe0f$", "", runes) # seems to cause problems with twemoji :shrug:
  emojis <- glue::glue("https://cdnjs.cloudflare.com/ajax/libs/twemoji/11.2.0/2/svg/{runes}.svg")
  emojis <- glue::glue('<img src="{emojis}" width = "{width}">')
  paste(emojis)
}

tz_global <-  function(tz = NULL) {
  if (!is.null(tz)) return(tz)
  tz  <- Sys.getenv("TZ")
  if(tz == "") "UTC" else tz
}