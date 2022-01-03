
# Define UI for application that draws a histogram
value_aggregator_ui <- function(id) {
  ns = NS(id)
  list(
    fluidRow(
    uiOutput(class = "d-inline-block separated", ns("daily")),
    uiOutput(class = "d-inline-block separated", ns("weekly")),
    uiOutput(class = "d-inline-block separated", ns("monthly")),
    uiOutput(class = "d-inline-block separated", ns("yearly"))
    )
  )
}

# Define server logic required to draw a histogram
value_aggregator_server <- function(input, output, session, cat, df) {
  ns <- session$ns

  
  output$yearly<- renderUI({
    cost = c_spancost(df() %>% filter(category == cat), span = "year")

    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      sprintf("%.2f", cost),
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Annual Cost",
      "</span>",
      "</p>"))
    return(ui)
  })
  
  output$monthly <- renderUI({
    cost = c_spancost(df() %>% filter(category == cat), span = "month")

    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      sprintf("%.2f", cost),
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Monthly Cost",
      "</span>",
      "</p>"))
    return(ui)
  })
  
  output$weekly<- renderUI({
    cost = c_spancost(df() %>% filter(category == cat), span = "week")
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      sprintf("%.2f", cost),
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Weekly Cost",
      "</span>",
      "</p>"))
    return(ui)
  })
  
  output$daily<- renderUI({
    cost = c_spancost(df() %>% filter(category == cat), span = "day")
    
    ui <- HTML(paste(
      "<p style='text-align:left;'>",
      "<span style='font-size: 220%;'>",
      sprintf("%.2f", cost),
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      "Daily Cost",
      "</span>",
      "</p>"))
    return(ui)
  })
  
}