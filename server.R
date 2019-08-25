

function(session, input, output){ 
  
  #Values Boxes Front Page  ---------------------------------------
  #value box for # of tweets per day
  observe({
   
    daily_count <- 15
    
    updateBoxValue(session, "daily_count", daily_count) 

  })
  #value box for # of people tweeting
  observe({
    
    daily_users <- 100
    
    updateBoxValue(session, "daily_users", daily_users) 
    
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

