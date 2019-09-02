
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
      menuItem("Bot Effects", tabName = "tab_bot", icon = icon("robot")),
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
            width = 3),
          valueBox(
            inputId = "ppc_users",
            "—", "PPC Candidates Tweeted Today",
            color = "purple"  ,  
            icon = icon("thumbs-down"),
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
              withSpinner(plotlyOutput("plotly_party_tweet_volume", height = "450px"))
            ),
            tabPanel(
              status = "success",
              title = "Tweets by Day",
              withSpinner(plotlyOutput("plotly_tweets_by_day", height = "450px"))
            )
          )
          # Front Page tweet plots - end ----------------------------------------
          
        ),
        fluidRow(
          #Frontpage most tweets columns start------------------------------------
          column(
            width = 8,
            offset = 2,
            class =  "col-md-6 col-md-offset-0 col-lg-4",
            class = "text-center",
            tags$h4(HTML(twemoji("2764"), "Most Liked in", TWEET_MOST$text)),
            withSpinner(uiOutput("dash_most_liked"), proxy.height = "200px")
          ),
          column(
            width = 8,
            offset = 2,
            class =  "col-md-6 col-md-offset-0 col-lg-4",
            class = "text-center",
            tags$h4(HTML(twemoji("1F31F"), "Most RT in", TWEET_MOST$text)),
            withSpinner(uiOutput("dash_most_rt"), proxy.height = "200px")
          ),
          column(
            width = 8,
            offset = 2,
            class =  "col-md-6 col-md-offset-0 col-lg-4",
            class = "text-center",
            tags$h4(HTML(twemoji("1F389"), "Most recent")),
            withSpinner(uiOutput("dash_most_recent"), proxy.height = "200px")
          )
          #Frontpage most tweets columns end--------------------------------------
        )
        
      ),
      tabItem(tabName = "tab_high_score",
              fluidRow(
                box(
                 title = "Top Tweeters", 
                 status = "info",
                 width = "6 col-lg-4",
                 tags$div(
                   class = "scroll-overflow-x",
                   withSpinner(uiOutput("top_tweeters"))
                 ),
                 helpText("Weighted average of RT (2x) and favourites (1x) per tweet")
              ),
              box(
                width = "6 col-lg-4",
                status = "danger",
                title  = "Top Hashtags",
                withSpinner(uiOutput("top_hashtags")),
                helpText("Times hashtag was used relative to most popular hastag")
              ),
              box(
                width = "6 col-lg-4",
                status = "success",
                title  = "Top Words", 
                withSpinner(uiOutput("top_words")),
                helpText("Times word was used relative to top word")
              )
              )),
      tabItem(tabName = "tab_bot",
              fluidRow(
                box(
                  title = "Coming Soon - see who's tweets are most impacted by bots!", 
                  status = "warning",
                  width = "6 col-lg-4"
                )
              )),
      
      #tab_pic_tweets---------------------------------------------------------
      tabItem(tabName = "tab_pic_tweets",
              class = "text-center",
              tags$h1(HTML("Tweets with", twemoji("1F5BC", width = "42px"))),
              shinyThings::paginationUI("pic_tweets", width = 12, offset = 0),
              withSpinner(uiOutput("pic_tweets_wall"), type = 3, color.background = "#F5F5F5"),
              shinyThings::pagerUI("pic_tweets", centered = TRUE)
              ),
      
      tabItem(tabName = "tab_explore",
              fluidRow(
                tweetExplorerUI("tweet_table", collapsed = TRUE, status = "success")
                )
              ),
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
                      "Hi! Thanks for checking out my app.",
                      "Being able to see what different politicians are saying",
                      "on twitter is becoming increasingly important, so I",
                      "made this app for all people on twitter or not to be able",
                      "to explore their tweets, and check out some small analytics",
                      "around them."
                    ),
                    tags$p(
                      class = "center",
                      "While I'm partisan, and carry certain political values,",
                      "I have tried to make this app as neutral as possible.",
                      "I have some concerns over hate speech being propogated by",
                      "certain political parties and individuals, and I may choose",
                      "to filter these individuals and parties out of the analysis",
                      "at a later date."
                    ),
                    tags$p(
                      class = "center",
                      "Building this app, or rather following a very detailed",
                      "template provided by a professional engineer",
                      "has been largely a learning process for myself, and I'm",
                      "still going to work to try to speed up the app, improve",
                      "the style and functionality, and give others an opportunity",
                      "to collaborate together to make this as useful as possible!"
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
                    width = "6 col-lg-4",
                    tags$p(
                      "I constructed this app by being kindly provided a list",
                      "of the twitter handles of all (known) candidates running",
                      "in the 2019 Canadian Federal election by",
                      HTML(paste0(tags$a(href = "twitter.com/robroc", "Robert Rocha,"))),
                      "a data journalist at CBC. He nor his organization have professional or personal",
                      "responsibility towards this app, he only shared the table.",
                      "Tweets are updated every 5 minutes,",
                      "and all source code for the tweets and the apps development",
                      "can be found",
                      HTML(paste0(tags$a(href = "github.com/pembletonc", "on my github page.")))
                    ))
              )
      )
      
    )

  )

)