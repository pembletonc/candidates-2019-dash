import_tweets <- function(
  start_date = NULL,
  blocklist = NULL, 
  tz_global = tz_global()
) {
  
  s3BucketName <- "candidate-tweets-2019"
  object <- get_bucket(s3BucketName,
                       "AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
                       "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY"))[[1]][[1]]
  
  tweets <- 
    s3readRDS(object = object, bucket = s3BucketName,
              "AWS_ACCESS_KEY_ID" = Sys.getenv("AWS_ACCESS_KEY_ID"),
              "AWS_SECRET_ACCESS_KEY" = Sys.getenv("AWS_SECRET_ACCESS_KEY")) %>% 
    mutate(created_at = lubridate::with_tz(created_at, tz_global())) %>% 
    tweets_since(TWEETS_START_DATE) %>%
    tweets_not_hashdump() %>% 
    arrange(desc(created_at))
    
  if(!"is_quote" %in% names(tweets)) tweets$is_quote <- FALSE
  if(!"is_retweet" %in% names(tweets)) tweets$is_retweet <- FALSE
  
  tweets
  
}


tweets_since <- function(tweets, since = "2019-08-01", tz = NULL) {
  if (is.null(since)) return(tweets)
  if (is.character(since)) since <- lubridate::ymd_hms(since, truncated = 3, tz = tz_global(tz))
  tweets %>%
    filter(created_at >= since)
}

tweets_in_last <- function(tweets, d = 0, h = 0, m = 15, s = 0){
  tweets %>% 
    filter(created_at >= lubridate::now() - lubridate::hours(h+d * 24) - 
             lubridate::minutes(m) - lubridate::seconds(s))
}


tweets_not_hashdump <- function(tweets) {
  tweets %>%
    mutate(n_hash = map_int(hashtags, length)) %>%
    filter(n_hash <= 7) %>%
    select(-n_hash)
}


tweets_just <- function(tweets, ...) {
  vars <- rlang::enquos(...)
  if (!length(vars)) {
    tweets %>%
      select(user_id, status_id, created_at, screen_name, text, party, riding)
  } else {
    tweets %>% select(!!!vars)
  }
}


tz_global <- function(tz = NULL) {
  if (!is.null(tz)) return(tz)
  tz <- Sys.getenv("TZ")
  if (tz == "") "UTC" else tz
}



#tweets_saved <- readRDS("data/tweets.rds")
#tweets_saved <- readRDS(TWEETS_FILE)

get_user_tweets <- function(n){
  
  library(rtweet)
  library(tidyverse)
  library(glue)
  library(aws.s3)
  
  Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJI423HJJKAEQREWQ",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "ca-central-1")
  
  s3BucketName <- "candidate-tweets-2019"
  name <- get_bucket(s3BucketName,
                     "AWS_ACCESS_KEY_ID" = "AKIAJI423HJJKAEQREWQ",
                     "AWS_SECRET_ACCESS_KEY" = "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",
                     "AWS_DEFAULT_REGION" = "ca-central-1")[[1]][[1]]
  
  tweets_saved <- s3readRDS(object = name, bucket = s3BucketName,
                            "AWS_ACCESS_KEY_ID" = "AKIAJI423HJJKAEQREWQ",
                            "AWS_SECRET_ACCESS_KEY" = "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",
                            "AWS_DEFAULT_REGION" = "ca-central-1")
  
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
  handles <- handles_orig %>% 
    filter(!is.na(twitter_handle) & str_length(twitter_handle) <= 15) %>% 
    mutate(twitter_handle = tolower(twitter_handle)) %>% 
    pull(twitter_handle)
  
  #convert to char only
  #handles <- as.character(handles_only$twitter_handle[1:length(handles_only$twitter_handle)])
  #import new tweets using function
  
  tweets <- get_timeline_unlimited(handles, 100)
  
  #clean imported tweets
  tweets <- tweets %>% 
    select(user_id:text, favorite_count, retweet_count, is_retweet, is_quote,
           source, hashtags, media_url, media_type, profile_url, profile_image_url) %>%
    filter(created_at > lubridate::ymd("2019-08-01")) %>% #min date Aug 1st
    mutate(screen_name = tolower(screen_name)) 
  
  #ensure newly imported tweets don't include doubles
  new_tweets_only <- anti_join(tweets, tweets_saved,  by = "status_id")
  
  print(paste("There are", nrow(new_tweets_only), "new tweets to add"))
  
  #join updated tweets to party and riding
  new_tweets_only <- new_tweets_only %>% 
    left_join(handles, by = c("screen_name" = "twitter_handle"))
  
  tweets <- bind_rows(tweets_saved, new_tweets_only)
  
  #save to local
  saveRDS(tweets, file = TWEETS_FILE)
  
  #also save to s3
  s3saveRDS(bucket = s3BucketName, 
            object = paste0(as.character(substitute(tweets)), ".rds"))
  
  return(tweets)
  
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

cache_profile_image <- function(profile_image_url, location = "www", default = "twitter-default-profile.jpg") {
  file_serve <- str_replace(profile_image_url, ".+/profile", "profile")
  file_local <- fs::path(location, file_serve)
  if (fs::file_exists(file_local)) {
    x <- list(result = file_serve)
  } else {
    fs::dir_create(fs::path_dir(file_local))
    x <- safely(download.file)(profile_image_url, file_local)
    # On fist download, the image won't be ready for the UI, so show default
    if (is.null(x$error)) x$result <- default
  }
  if (is.null(x$error)) x$result else default
}


tz_global <-  function(tz = NULL) {
  if (!is.null(tz)) return(tz)
  tz  <- Sys.getenv("TZ")
  if(tz == "") "UTC" else tz
}


# Progress Bar

#' Progress Bar
#'
#' From: https://github.com/rstudio/shinydashboard/issues/119
progressBar <- function(
  value = 0,
  label = FALSE,
  color = "aqua",
  size = NULL,
  striped = FALSE,
  active = FALSE,
  vertical = FALSE
) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  # if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
  #   stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  if (vertical)
    style <- htmltools::css(height = text_value, `min-height` = "2em")
  else
    style <- htmltools::css(width = text_value, `min-width` = "2em")
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    class = if (vertical) "vertical",
    class = if (active) "active",
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      class = if (striped) "progress-bar-striped",
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

#' Progress Group
progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}

progressBar_v <- function(x, colors) {
  if (length(colors) > length(x)) {
    colors <- rep(colors, ceiling(length(x)/length(colors)))
  }
  x <- purrr::map2(x, colors[seq_along(x)], ~ progressBar(.x, color = .y))
  map_chr(x, paste)
}

masonify_tweets <- function(tweets, id = NULL, class = NULL) {
  stopifnot("status_id" %in% names(tweets))
  
  t_embed <-
    tweets %>%
    pmap(get_tweet_blockquote) %>%
    map(HTML) %>%
    map(tags$div, class = "tweet-item")
  
  tagList(
    tags$div(id = id,
             class = paste("masonry text-left", class),
             t_embed
    )
  )
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



tweet_cache_oembed <- function(tweets, cache = "data/tweets_oembed.rds") {
  if (fs::file_exists(cache)) {
    oembed <- readRDS(cache)
    tweets <- left_join(tweets, oembed, by = "status_id")
  } else {
    oembed <- tibble(status_id = character(), oembed = character())
  }
  
  if (!"oembed" %in% names(tweets)) tweets$oembed <- map(seq_len(nrow(tweets)), ~ NULL)
  
  is_needed <- map_lgl(tweets$oembed, is.null)
  if (any(is_needed)) {
    tweets$oembed[is_needed] <- if (requireNamespace("furrr", quietly = TRUE)) {
      furrr::future_pmap(tweets[is_needed, ], get_tweet_blockquote, null_on_error = FALSE, .progress = TRUE)
    } else {
      pmap(tweets[is_needed, ], get_tweet_blockquote, null_on_error = FALSE)
    }
    oembed_new <- tweets %>% select(status_id, oembed)
    # oembed may be a superset of the oembed we got from these tweets
    # so the following keeps the old oembed not contained in oembed_new
    oembed <- bind_rows(
      semi_join(oembed_new, oembed, by = "status_id"),
      anti_join(oembed_new, oembed, by = "status_id")
    )
    saveRDS(oembed, cache)
  }
  return(tweets)
}

