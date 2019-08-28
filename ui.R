library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(lubridate)
#note using the forked version of the app from @grrrck ref = @stream-box

dashboardPage(
  # Dashboard Page Setup ----------------------------------------------------
  title = META$name,
  skin  = "blue",
  #theme = c(META$theme_css, "custom.css"),
  dashboardHeader(
    titleWidth = 400,
    title = HTML(glue::glue(
      '<span class="logo-mini">{META$logo_mini}</span>
      <span class="logo-lg">{META$logo_lg}</span>'
    ))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "tab_dashboard", icon = icon("dashboard")),
      menuItem("High Score", tabName = "tab_high_score", icon = icon("trophy")),
      menuItem("Tweet Wall", tabName = "tab_tweet_wall", icon = icon("stream")),
      menuItem("Media Tweets", tabName = "tab_pic_tweets", icon = icon("images")),
      menuItem("Explore", tabName = "tab_explore", icon = icon("compass")),
      menuItem("About", tabName = "tab_about", icon = icon("info"))
    )
  ),
  dashboardBody(
    tabItems(
      #Front Page - tab_dashboard
      tabItem(
        "tab_dashboard",
        tags$head(
          HTML(glue::glue(
            '<meta property="og:title" content="{META$name}">
            <meta property="og:description" content="{META$description}">
            <meta property="og:url" content="{META$app_url}">
            <meta property="og:image" content="{META$app_icon}">
            <meta name="twitter:card" content="summary">
            <meta name="twitter:creator" content="@coreypembleton">
            <meta name="twitter:site" content="https://data-break.netlify.com">
            '
          ))
          # Metadata <head> end-----
        ),
        fluidRow(
          # Frontpage - boxes - start-----
          valueBox(
            inputId = "daily_count",
            "—", "Tweets Today",
            color = "purple",
            icon = icon("comment-dots"),
            width = 3),
          valueBox(
            inputId = "total_count",
            "—", " Total Tweets Since August 1st, 2019",
            color = "fuchsia"  ,  
            icon = icon("comment-dots"),
            width = 3),
          valueBox(
            inputId = "total_tweeps",
            "—", "# of candidates included",
            color = "fuchsia"  ,  
            icon = icon("user-circle"),
            width = 3),
          valueBox(
            inputId = "lib_users",
            "—", "Liberal Candidates Tweeted Today",
            color = "red"  ,  
            icon = icon("cannabis"),
            width = 3),
          valueBox(
            inputId = "con_users",
            "—", "Conservative Candidates Tweeted Today",
            color = "blue"  ,  
            icon = icon("cut"),
            width = 3),
          valueBox(
            inputId = "ndp_users",
            "—", "NDP Candidates Tweeted Today",
            color = "orange"  ,  
            icon = icon("hands-helping"),
            width = 3),
          valueBox(
            inputId = "green_users",
            "—", "Green Candidates Tweeted Today",
            color = "green"  ,  
            icon = icon("pagelines"),
            width = 3)
          # Frontpage Boxes - End---------------------------------------------
        ),
        fluidRow(
          # Frontpage tweet volume plots---------------------------------------------
          
          tabBox(
            width = 12,
            tabPanel(
              status = "primary", 
              title = "Tweet Volume by Party", 
              withSpinner(plotlyOutput("plotly_party_tweet_volume", height = "250px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Day",
              withSpinner(plotlyOutput("plotly_tweets_by_day", height = "250px"))
            )
          )
        )
      ),
      tabItem(tabName = "tab_high_score",
              fluidRow(
                box(
                 title = "placeholder box", 
                 status = "info",
                 width = "6 col-lg-4"
                )
              )),
      tabItem(tabName = "tab_tweet_wall",
              fluidRow(
                box(
                  title = "placeholder box", 
                  status = "warning",
                  width = "6 col-lg-4"
                )
              )),
      tabItem(tabName = "tab_pic_tweets",
              class = "text-center",
              fluidRow(
                box(
                  title = "placeholder box", 
                  status = "success",
                  width = "6 col-lg-4"
                )
              )),
      tabItem(tabName = "tab_explore",
              class = "text-center",
              fluidRow(
                box(
                  title = "placeholder box", 
                  status = "primary",
                  width = "6 col-lg-4"
                )
              )),
      tabItem(tabName = "tab_about",
              fluidRow(
                box(title = "About Me",
                    status = "danger",
                    width = "6 col-lg-4",
                    tags$p(
                      class = "text-center",
                      tags$strong("Thanks for checking out my app!")
                      ),
                    tags$p(
                      class = "center",
                      "I'm a program officer by day in Montreal helping cities",
                      "in their smart city programming, and by night I'm",
                      "learning to program in R, with this shiny app being my first!"
                    ),
                    tags$p(
                      class = "center",
                      ""
                    ),
                    tags$p(
                      icon("twitter"),
                      "Drop my a line on twitter and let me know what you think!",
                      HTML(paste0("(", tags$a(href = "https://twitter.com/coreypembleton", 
                                              "@coreypembleton", target = "_blank"), ")")),
                      icon("twitter")
                    )
                    ),
                box(title = "Credit and Thanks",
                    status = "success",
                    width = "6 col-lg-4",
                    tags$p(
                      "This app simply would not exist if not for",
                      HTML(paste0(tags$a(href = "apps.garrickadenbuie.com/rstudioconf-2019/", "this amazing template"))),
                      "developed in the spirit of open source coding by",
                      HTML(paste0( tags$a(href ="https://garrickadenbuie.com", "Garrick Aden-Buie"))),
                      "and the remarkable work of countless engineers tirelessly in the R community",
                      "developing the packages used to make it possible such as shiny, shinydashboard",
                      "rtweet, plotly, and pretty much the whole of the tidyverse."
                    )
                    ),
                box(title = "About this app",
                    status = "danger", 
                    width = "6 col-lg-5",
                    tags$p(
                      "I constructed this app by being kindly provided a list",
                      "of the twitter handles of all (known) candidates running",
                      "in the 2019 Canadian Federal election by",
                      HTML(paste0(tags$a(href = "twitter.com/robroc", "Robert Rocha,"))),
                      "a data journalist at CBC. Tweets are updated every 5 minutes,",
                      "and all source code for the tweets and the apps development",
                      "can be found",
                      HTML(paste0(tags$a(href = "github.com/pembletonc", "on my github page.")))
                    ))
              )
      )
      
    )

  )

)