library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
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
            "—", " Tweets Today",
            color = "purple",
            icon = icon("comment-dots"),
            width = 3),
          valueBox(
            inputId = "daily_users",
            "—", " Candidates Tweeted Today",
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
                box(title = "About me",
                    status = "danger",
                    width = "6 col-lg-4",
                    tags$p(
                      "Get in touch with me on Twitter at",
                      HTML(paste0("(", tags$a(href = "https://twitter.com/coreypembleton", 
                                              "@coreypembleton", target = "_blank"), ","))
                    ))
              )
      )
      
    )

  )

)