

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
      count()
    
    updateBoxValue(session, "daily_count", daily_count) 

  })

    observe({
    
      #Total number of tweets since August 1st not including RT
    total_count <- temp %>%
      filter(is_retweet == FALSE) %>% 
      nrow()

    updateBoxValue(session, "total_count", total_count) 
    
  })

  #value box for # of people tweeting
  observe({
    # Count of all candidates
    
        total_tweeps <- temp %>% 
          group_by(user_id) %>% 
          count()
          
    updateBoxValue(session, "total_tweeps", total_tweeps) 
    
  })
  observe({
    
    lib_users <- 100
    
    updateBoxValue(session, "lib_users", lib_users) 
    
  })
  
  observe({
    
    con_users <- 66
    
    updateBoxValue(session, "con_users", con_users) 
    
  })
  
  observe({
    
    ndp_users <- 150
    
    updateBoxValue(session, "ndp_users", ndp_users) 
    
  })
  
  observe({
    
    green_users <- 66
    
    updateBoxValue(session, "green_users", green_users) 
    
  })
  
  # Dashboard plots------------------------------------------------------------
  output$plotly_party_tweet_volume <- renderPlotly({
    economics %>%
      plot_ly(x = ~date, color = I("black")) %>% 
      add_lines(y = ~uempmed) %>% 
      add_lines(y = ~psavert, color = I("Red"))
      
  })
  
  
  output$plotly_tweets_by_day <- renderPlotly({
    economics %>%
      plot_ly(x = ~date, color = I("black")) %>% 
      add_lines(y = ~uempmed) %>% 
      add_lines(y = ~psavert, color = I("Red"))
    
  })
  
  
  }

