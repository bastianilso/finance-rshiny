
# Define server logic required to draw a histogram
data_server <- function(input, output, session) {
  ns <- session$ns

  toReturn <- reactiveValues(
    df = data_load(),
    trigger = 1
  )
    
  return(toReturn)
}

data_save <- function(df) {
  save(df, file = 'data.rda', compress=FALSE)
}

data_load <- function() {
  load('data.rda')
  return(df)
}


# df <- tibble(
#    timestamp = as.POSIXct("2022-01-01 13:30:15.1234", format = "%Y-%m-%d %H:%M:%OS"),
#    amount = 0.00,
#    submitted = as.POSIXct(now(tzone = ""), format = "%Y-%m-%d %H:%M:%OS"),
#    label = "Starting Point",
#    category = "Test",
#    icon = "heart",
#    user = "Test",
#    event = "new_category",
#    eventid = 0
#  )
 