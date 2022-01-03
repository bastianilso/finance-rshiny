
# Define UI for application that draws a histogram
insert_value_ui <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>Insert Value</h3>"),
    HTML("Date:"), dateInput(ns("timestamp"), label = "Date", format = "dd-mm-yy"),
    HTML("Label:"), textInput(ns("label"), label = "Name", value = " ", placeholder = "Label"),
    HTML("Amount:"), numericInput(ns("amount"), label = "Amount", min=0, value =0.00),
    actionButton(ns("actionInsert"), "Insert", class="btn-primary")
  )
}

# Define server logic required to draw a histogram
insert_value_server <- function(input, output, session, category) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    date = NULL,
    name = NULL,
    amount = NULL,
    type = NULL,
    category = NULL,
    trigger = 0
  )
  
  observeEvent(input$actionInsert, {
    toReturn$timestamp <- input$timestamp
    toReturn$label <- input$label
    toReturn$amount <- input$amount
    toReturn$trigger <- toReturn$trigger + 1
    toReturn$category <- category()
    removeModal()
  })
  
  return(toReturn)
}