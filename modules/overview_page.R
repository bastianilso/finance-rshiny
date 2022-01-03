
# Define UI for application that draws a histogram
overview_page_ui <- function(id) {
  ns = NS(id)
  list(
    fluidRow(HTML('<h3>Overview</h3>')),
    uiOutput(ns("categories"))
  )
}

# Define server logic required to draw a histogram
overview_page_server <- function(input, output, session, df) {
  ns <- session$ns
  
  observeEvent(df(), {
    req(!is.null(df()))
    cats = unique(df()$category)
    
    lapply(1:length(cats), function(i) {
      callModule(category_aggregator_server,i,cats[i],df)
    })
    
    output$categories <- renderUI({
      lapply(1:length(cats), function(i) {
        category_aggregator_ui(ns(i))
      })
    })
    
  })

}