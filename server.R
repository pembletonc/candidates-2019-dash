

function(session, input, output){ 
  
  #Global Reactives -----------------------------------------
  
  #can adjust n here if slowing things down
  
  #tweets <- get_user_tweets(200)
  
  temp <- readRDS("./data/tweets1.rds")
  
  
  #Values Boxes Front Page  ---------------------------------------
  
  #value box for # of tweets on day it is viewed
  observe({
    
    daily_count <- temp %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      count() %>% 
      pull(n) %>% 
      format(big.mark = ",", digits = 0)
    
    updateBoxValue(session, "daily_count", daily_count) 
    
  })
  
  observe({
    
    #Total number of tweets since August 1st not including RT
    total_count <- temp %>%
      filter(is_retweet == FALSE) %>% 
      nrow() %>% 
      format(big.mark = ",", digits = 0)
    
    updateBoxValue(session, "total_count", total_count) 
    
  })
  
  #value box for # of people tweeting
  observe({
    # Count of all candidates
    
    total_tweeps <- temp %>% 
      group_by(user_id) %>%
      summarise() %>% 
      count() 
    
    updateBoxValue(session, "total_tweeps", total_tweeps$n) 
    
  })
  observe({
    
    lib_users <- temp %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "Liberal") %>% 
      summarise() %>% 
      count() 
    
    updateBoxValue(session, "lib_users", lib_users$n) 
    
  })
  
  observe({
    
    con_users <- temp %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "Conservative") %>% 
      summarise() %>% 
      count() 
    
    updateBoxValue(session, "con_users", con_users$n) 
    
  })
  
  observe({
    
    ndp_users <- temp %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "NDP") %>% 
      summarise() %>% 
      count() 
    
    updateBoxValue(session, "ndp_users", ndp_users$n) 
    
  })
  
  observe({
    
    green_users <- temp %>%
      mutate(created_at = lubridate::ymd(lubridate::as_date(created_at))) %>% 
      filter(created_at == lubridate::today()) %>% 
      group_by(party, screen_name) %>% 
      filter(party == "Green") %>% 
      summarise() %>% 
      count() 
    
    updateBoxValue(session, "green_users", green_users$n) 
    
  })
  
  # Dashboard plots------------------------------------------------------------
  output$plotly_party_tweet_volume <- renderPlotly({
    
    temp %>% 
      mutate(created_at = lubridate::ymd(as_date(created_at))) %>% 
      group_by(party, created_at) %>% 
      count() %>%
      spread(party, n, fill = 0) %>% 
      plot_ly(x = ~ created_at, 
              mode = "marker", type = "scatter",
              y = ~Liberal,
              name = 'Liberal', 
              mode = "marker",
              color = I("red")) %>% 
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
      config(displayModeBar = FALSE) %>% 
      layout(
        hovermode = "compare",
        yaxis = list(title = "Tweets"),
        legend = list(orientation = "h", 
                      x = 0.05, y = 0.9)
      ) %>% 
      layout(
        xaxis = list(
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
                stepmode = "todate"),
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

    temp %>% 
    mutate(created_at = lubridate::ymd(as_date(created_at))) %>%
    filter(created_at > "2019-07-31") %>% 
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
  
  temp %>% 
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
    arrange(desc(favourites_count)) %>% 
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
    arrange(desc(retweet_count)) %>% 
    slice(1) %>% 
    pmap(get_tweet_blockquote) %>% 
    .[[1]] %>% 
    HTML()
  
})

output$dash_most_recent <- renderUI({
  
  tweets %>% 
    arrange(desc(created_at)) %>% 
    slice(1) %>% 
    pmap(get_tweet_blockquote) %>% 
    .[[1]] %>% 
    HTML()
})


}

