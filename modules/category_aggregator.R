
# Define UI for application that draws a histogram
category_aggregator_ui <- function(id) {
  ns = NS(id)
  list(
    fluidRow(class = "d-inline-block separated", style="border: 1px solid lightgrey; padding:25px 150px 10px 35px; margin: 10px;",
    uiOutput(ns("icon")),
    uiOutput(ns("category"))
    )
  )
}

# Define server logic required to draw a histogram
category_aggregator_server <- function(input, output, session, cat, df) {
  ns <- session$ns
  
  output$icon<- renderUI({
    iconname <- df() %>% filter(category == cat) %>% slice(1) %>% select(icon)
    ui <- icon(iconname, "fa-2x")
    return(ui)
  })
  
  output$category <- renderUI({
    cost = c_spancost(df() %>% filter(category == cat), span = "month")

    ui <- HTML(paste(
      "<p style='text-align:left;margin-top:20px;'>",
      "<span style='font-size: 160%'>",
      paste(sprintf("%.2f", cost), "kr"),
      "</span>",
      "<br>",
      "<span style='font-size:75%; text-transform: uppercase;'>",
      cat,
      "</span>",
      "</p>"))
    return(ui)
  })
  
  ## TODO: Return() and showTab()
  
}