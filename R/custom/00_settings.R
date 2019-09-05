# ---- Metadata ----
META <- list(
  # Name of the app, used in the browser/tab title
  name        = "Canada Votes 2019",
  # A description of the app, used in social media cards
  description = "A Shiny Dashboard Built to Explore Candidates Tweeting Habits in the 2019 Federal Election",
  # Link to the app, used in social media cards
  app_url     = "https://data-break.shinyapps.io/Canada_2019_Election_Tweets/",
  # Link to app icon image, used in social media cards
  app_icon    = "https://www.elections.ca/templates/images/logo.jpg",
  # The name of the conference or organization
  # App title, long, shown when sidebar is open, HTML is valid
  logo_lg     = "<em>2019 Canadian Federal Election Candidate Tweets</em>",
  # App title, short, shown when sidebar is collapsed, HTML is valid
  logo_mini   = "<em>Vote</em><strong>2019</strong>",
  # Icon for box with count of conference-related tweets
  
  skin_color  = "blue-light",
  
  theme_css   = c("ocean-next/AdminLTE.css", "ocean-next/_all-skins.css")
  
)


TWEET_MOST <- list(
  hours = 12,
  days = 0,
  minutes = 0,
  text = "12 hours"
)


TWEETS_START_DATE <- "2019-08-01"  # Don't show tweets before this date
TZ_GLOBAL <- "America/Toronto"     # Time zone where conference is taking place
Sys.setenv(TZ = TZ_GLOBAL)

today_tz <- function() today(tz_global())


ADMINLTE_COLORS <- list(
  "light-blue" = "#6699CC",
  "green"      = "#99C794",
  "red"        = "#EC5f67",
  "purple"     = "#C594C5",
  "aqua"       = "#a3c1e0",
  "yellow"     = "#FAC863",
  "navy"       = "#343D46",
  "olive"      = "#588b8b",
  "blue"       = "#4080bf",
  "orange"     = "#F99157",
  "teal"       = "#5FB3B3",
  "fuchsia"    = "#aa62aa",
  "lime"       = "#b0d4b0",
  "maroon"     = "#AB7967",
  "black"      = "#1B2B34",
  "gray-lte"   = "#D8DEE9",
  "primary"    = "#6699CC",
  "success"    = "#99C794",
  "danger"     = "#EC5f67",
  "info"       = "#a3c1e0",
  "warning"    = "#FAC863"
)

BASIC_COLORS <- c("primary", "info", "success", "danger", "warning")




# ---- Tweet Storage Location ----
# The tweets should be stored in data/tweets.rds or data/tweets_simplified.rds.
# The latter is preferred and should contain only the columns needed for the
# app, which reduced start-up loading time. You do not need to make any changes
# to this section if the app is managing the tweet gathering, but I'm "exposing"
# the global argument just in case you have a very particular setup. Note that
# the app will look for the following files in the order they appear.
TWEETS_FILE <- file.path("data", paste0("tweets", ".rds"))

# Should the app manage tweet updating? If FALSE, assumes that tweets are
# updated by an external process
TWEETS_MANAGE_UPDATES <- TRUE
# Number of seconds betewen tweet updating intervals. 5-10 minutes is generally
# fine. Updates are done incrementally (new tweets only) unless it has been more
# than 2 hours since last update or within a window around the start of the hour.
TWEETS_MANAGE_UPDATE_INTERVAL <- 5 * 60

# Location of your Twitter PAT from {rtweet}. See {rtweet} for help
# authenicating your app with Twitter. Then view your users .Renviron (you can
# use `usethis::edit_r_environ()`) to locate the RDS containing your PAT.
# If you're deploying on shinyapps.io or elsewhere you should copy this RDS file
# into your project (usually it's `~/.rtweet_token.rds`) and set the variable
# below to the correct file name. DON'T COMMIT THIS FILE TO VERSION CONTROL!!

TWEETS_MANAGE_TWITTER_PAT_RDS <- "rtweet_token.rds"

if (TWEETS_MANAGE_UPDATES) {
  # Check that TWITTER_PAT is correctly set
  if (Sys.getenv("TWITTER_PAT") == "") {
    if (file.exists(TWEETS_MANAGE_TWITTER_PAT_RDS)) {
      Sys.setenv(TWITTER_PAT = TWEETS_MANAGE_TWITTER_PAT_RDS)
    } else {
      warning(
        "I can't find the file containing your Twitter PAT, so live ",
        "updating with {rtweet} won't be possible. See {rtweet} for help ",
        "authenticating and set the config TWEETS_MANAGE_TWITTER_PAT_RDS in ",
        "'R/custom/00_settings.R'."
      )
      TWEETS_MANAGE_UPDATES <- FALSE
    }
  }
}

