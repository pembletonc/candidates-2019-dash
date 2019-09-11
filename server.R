
function(session, input, output){ 
  

  #Global Reactives -----------------------------------------

  tweets_all <-  reactivePoll(1 * 60 * 1000, session, checkFunc = function() {
    x <- import_tweets(
      tz_global = tz_global(),
      start_date = TWEETS_START_DATE
    )
    
    return(x)
  },
  valueFunc = function(){
    x <- import_tweets(
      file, 
      tz_global = tz_global(),
      start_date = TWEETS_START_DATE
    )
    
    return(x)
  }
 )
  

  tweets <- reactive({
    req(tweets_all())
    tweets_all() %>%
      tweet_cache_oembed()
  })
  

  #tweets_oembed <- tweet_cache_oembed(tweets = tweets)
  
  #tweets <- get_user_tweets(100)
  #tweets <- readRDS("./data/tweets.rds")
  #Values Boxes Front Page  ---------------------------------------
  
  #value box for # of tweets on day it is viewed
  observe({
    
    tweets_in_last <- tweets() %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today())
    
    daily_count <- tweets_in_last %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0)
    
    updateBoxValue(session, "daily_count", daily_count) 
    
  })
  
  observe({
    
    #Total number of tweets since August 1st not including RT
    total_count_f <- tweets() %>%
      filter(is_retweet == FALSE)
   
     total_count <- 
      total_count_f %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "total_count", total_count) 
    
  })
  
  #value box for # of people tweeting
  observe({
    # Count of all candidates
    
    total_tweeps <- tweets() %>% 
      group_by(user_id) %>%
      summarise() %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "total_tweeps", total_tweeps) 
    
  })
  observe({
    
    lib_users_today <- tweets() %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today())
    
    lib_users <- 
      lib_users_today %>% 
      group_by(party, screen_name) %>% 
      filter(party == "Liberal") %>% 
      summarise() %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "lib_users", lib_users) 
    
  })
  
  observe({
    
    con_users <- tweets() %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "Conservative") %>% 
      summarise() %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "con_users", con_users) 
    
  })
  
  observe({
    
    ndp_users <- tweets() %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "NDP") %>% 
      summarise() %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "ndp_users", ndp_users) 
    
  })
  
  observe({
    
    green_users <- tweets() %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "Green") %>% 
      summarise() %>% 
      count()%>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "green_users", green_users) 
    
  })
  
  observe({
    
    ppc_users <- tweets() %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "PPC") %>% 
      summarise() %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0) 
    
    updateBoxValue(session, "ppc_users", ppc_users) 
  })
  
  
  # Dashboard plots------------------------------------------------------------
  output$plotly_party_tweet_volume <- renderPlotly({
    
    tweets() %>%
      tweets_just(created_at, party) %>% 
      mutate(created_at = lubridate::ymd(as_date(created_at))) %>% 
      group_by(party, created_at) %>% 
      count() %>%
      spread(party, n, fill = 0) %>% 
      plot_ly(x = ~ created_at, 
              mode = "marker", type = "scatter",
              y = ~Liberal,
              name = 'Liberal', 
              mode = "marker",
              color = I("red"),
              alpha = 0.65) %>% 
      add_trace(y  = ~Conservative, 
                name = "Conservative", 
                mode = "marker",
                color = I("blue")) %>% 
      add_trace(y = ~NDP,
                name = 'NDP', 
                mode = "marker",
                color = I("orange")) %>% 
      add_trace(y = ~Green,
                name = 'Green', 
                mode = "marker",
                color = I("green")) %>%
      add_trace(y = ~PPC,
                name = "PPC",
                mode = "marker",
                color = I("purple")) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(
        hovermode = "compare",
        yaxis = list(title = "Tweets"),
        legend = list(orientation = "v", 
                      x = 0.1, y = 0.9, bgcolor = NULL)
      ) %>% 
      layout(
        xaxis = list(
          title = "Date",
          range = c(lubridate::now(tz_global()) - days(7), now(tz_global())),
          rangeselector = list(
            buttons = list(
              list(
                count = 1, 
                label = "Today",
                step = "day", 
                stepmode = "todate"),
              list(
                count = 1,
                label = "Yesterday",
                step = "day",
                stepmode = "backward"),
              list(
                count = 7,
                label = "week",
                step = "day",
                stepmode = "backward"),
              list(step = "all", label  = "All"))),
          rangeslider = list(type = "date")
        
      ))
  
  
  
  
  })


output$plotly_tweets_by_day <- renderPlotly({

  tweets() %>%
    tweets_just(created_at, party) %>% 
    mutate(created_at = lubridate::ymd(as_date(created_at))) %>%
    group_by(party, created_at) %>% 
    count() %>%
    spread(party, n, fill = 0) %>% 
    plot_ly(x = ~created_at, type = "bar", 
              y = ~Liberal,       name = "Liberal", color = I("red")) %>% 
    add_trace(y = ~Conservative, name = "Conservative", color = I("blue")) %>% 
    add_trace(y = ~Green,        name = "Green", color = I("green")) %>% 
    add_trace(y = ~NDP,          name = "NDP", color = I("orange")) %>% 
    layout(yaxis = list(title = "Tweets"),
           hovermode = "compare",
           xaxis = list(title = "Date")
    ) %>% 
    config(displayModeBar = FALSE)
  
})

#Dashboard tweet leaders--------------
tweets_most <- reactive({
  
  tweets() %>% 
   tweets_in_last(d = TWEET_MOST$days,
                  h = TWEET_MOST$hours,
                  m = TWEET_MOST$minutes)
    
})

output$dash_most_liked <- renderUI({
  validate(
    need(nrow(tweets_most()) > 0,
         paste("No tweets in", TWEET_MOST$text)
         ))
  
  tweets_most() %>% 
    arrange(desc(favorite_count)) %>% 
    slice(1) %>% 
    pmap(get_tweet_blockquote) %>% 
    .[[1]] %>% 
    HTML()
  
})

output$dash_most_rt<- renderUI({
  validate(
    need(nrow(tweets_most()) > 0,
         paste("No tweets in", TWEET_MOST$text)
    ))
  
  tweets_most() %>% 
    filter(is_retweet == FALSE) %>% #removes the retweets 
    arrange(desc(retweet_count)) %>% 
    slice(1) %>% 
    pmap(get_tweet_blockquote) %>% 
    .[[1]] %>% 
    HTML()
  
})

output$dash_most_recent <- renderUI({
  
  tweets_most() %>% 
    arrange(desc(created_at)) %>% 
    slice(1) %>% 
    pmap(get_tweet_blockquote) %>% 
    .[[1]] %>% 
    HTML()
})

# Front page top columns end---------------------------------------------------

output$top_tweeters <- renderUI({
  
  tweets() %>%
    filter(is_retweet == FALSE, created_at > c(lubridate::now(tz_global()) - days(7))) %>% 
    group_by(screen_name, profile_url,profile_image_url) %>% 
    summarise(engagement = (sum(retweet_count) * 2 + sum(favorite_count))/ n()) %>% 
    arrange(desc(engagement)) %>% 
    ungroup() %>% 
    slice(1:10) %>% 
    mutate(
      engagement        = scale(engagement, center = FALSE),
      engagement        = engagement / max(engagement) * 100,
      profile_image     = map_chr(profile_image_url, cache_profile_image),
      profile_image_url = glue::glue('<div class="center-block"><img class="img-responsive img-circle" src="{profile_image}" alt={screen_name} style="max-height: 25px; min-width: 20px;"></div>'),
      profile_url       = if_else(is.na(profile_url), glue::glue("https://twitter.com/{screen_name}"), profile_url),
      screen_name       = glue::glue('<a href="{profile_url}" target="_blank">@{screen_name}</a>'),
      engagement        = progressBar_v(engagement, rep(BASIC_COLORS[1:5], 2))
    ) %>% 
    select(profile_image_url, screen_name, engagement) %>% 
    knitr::kable(
      format = "html",
      escape = FALSE,
      align = "cll",
      col.names = c("", "Screen Name", "Engagement / Tweet "),
      table.attr = 'class = "table"'
    ) %>% 
    HTML()
})

output$top_hashtags <- renderUI({
  
  twh <- 
    tweets() %>%
    filter(created_at > c(lubridate::now(tz_global()) - days(7))) %>% 
    select(hashtags) %>% 
    unnest(cols = c(hashtags)) %>% 
    count(hashtags, sort = TRUE) %>% 
    filter(!is.na(hashtags)) %>% 
    #filter(!str_detect(tolower(hashtags), TOPIC$hashtag_exclude)) %>% #hashtags to remove later if needed
    mutate(hashtags = paste0("#", hashtags))
  
  colors <- rep(BASIC_COLORS[1:5], 2) 
  
  tags$div(
    map(seq_len(min(10, nrow(twh))), ~ {
      progressGroup(twh$hashtags[[.]], twh$n[[.]], max = max(twh$n), color = colors[.])
    })
  )
  
})

output$top_words <- renderUI({
  tw <- tweets() %>% 
    filter(created_at > c(lubridate::now(tz_global()) - days(7))) %>% 
    select(text) %>% 
    mutate(
      text = str_remove_all(text, "@[[:alnum:]_]+\\b"),
      text = str_remove_all(text, "&\\w+;")
    ) %>% 
    tidytext::unnest_tokens(word, text) %>% 
    filter(
      !word %in% c("http", "https", "t.co"),
      #!str_detect(word, TOPIC$wordlist_exlude) if added later
      nchar(word) >= 3
    ) %>% 
    anti_join(tidytext::stop_words, by = "word") %>%
    anti_join(proustr::proust_stopwords(), by = "word") %>% 
    count(word, sort = TRUE) %>% 
    slice(1:10)
  
  colors <- rep(BASIC_COLORS[1:5], 2)
  
  tags$div(
    map(seq_len(min(10, nrow(tw))), ~ {
      progressGroup(tw$word[[.]], tw$n[[.]], max = max(tw$n), color = colors[.])
    })
  )
  
})

#picture tweet wall---------
pic_tweets_page_break <- 20

tweets_pictures <- reactive({
  tweets() %>%
    filter(created_at > c(lubridate::now(tz_global()) - days(7))) %>% 
    filter(is_retweet == FALSE) %>%
    select(created_at, status_id, screen_name, media_url) %>% 
    filter(!map_lgl(media_url, ~length(.) > 1 || is.na(.)))
})

pic_tweets_n_items <- reactive({ nrow(tweets_pictures()) })
pic_tweets_page <- shinyThings::pager("pic_tweets", pic_tweets_n_items, pic_tweets_page_break)

output$pic_tweets_wall <- renderUI({
  s_page_items <- pic_tweets_page() %||% 1L
  
  validate(need(
    nrow(tweets_pictures()) > 0,
    "No media yet, check back soon."
  ))
  
  tweets_pictures() %>% 
    slice(s_page_items) %>% 
    masonify_tweets()
  
})

#Tweet explorer

callModule(tweetExplorer, "tweet_table", reactive({ tweets() }), tzone = tz_global())

}

