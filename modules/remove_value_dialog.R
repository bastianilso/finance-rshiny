
# Define UI for application that draws a histogram
remove_value_ui <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>Remove Value</h3>",
         "<p>Warning! This is not possible to undo.</p>"),
    HTML("ID to remove:"), numericInput(ns("removeID"), label = "removeID", value = " "),
    actionButton(ns("actionRemove"), "Remove", class="btn-primary")
  )
}

# Define server logic required to draw a histogram
remove_value_server <- function(input, output, session) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    val = -1,
    trigger = 0
  )
  
  observeEvent(input$actionRemove, {
    toReturn$val <- input$removeID
    toReturn$trigger <- toReturn$trigger + 1
    removeModal()
  })
  
  return(toReturn)
}