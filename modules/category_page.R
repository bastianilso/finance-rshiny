
# Define UI for application that draws a histogram
category_page_ui <- function(id) {
  ns = NS(id)
  list(
    uiOutput(ns("title")),
    value_aggregator_ui(ns("aggregate")),
    value_plot_ui(ns("plot")),
    actionButton(ns("actionAdd"), "Add", class="btn-primary"),
    actionButton(ns("actionRemove"), "Remove", class="btn-danger"),
    value_table_ui(ns("values"))
  )
}

# Define server logic required to draw a histogram
category_page_server <- function(input, output, session, cat, df) {
  ns <- session$ns
  callModule(value_table_server, "values", cat, df)
  callModule(value_aggregator_server, "aggregate", cat, df)
  callModule(value_plot_server, "plot", cat, df)
  
  toReturn <- reactiveValues(
    newval = NULL,
    trigger = 0
  )

  output$title <- renderUI({
    return(HTML("<h3>", cat, "</h3>"))
  })
  
  observeEvent(input$actionAdd, {
    insertUI(selector = "#actionAdd", where = "afterEnd",
                      ui = showModal(modalDialog(insert_value_ui("insertValue"), easyClose = TRUE)))
  })
  
  observeEvent(input$actionRemove, {
    insertUI(selector = "#actionRemove", where = "afterEnd",
             ui = showModal(modalDialog(remove_value_ui("removeValue"), easyClose = TRUE)))
  })
  
  return(toReturn)
}