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
