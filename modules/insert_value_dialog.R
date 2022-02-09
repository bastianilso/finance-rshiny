
# Define UI for application that draws a histogram
insert_value_ui <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>Insert Payment</h3>"),
    radioButtons(ns("dateType"), label = " ", choices = c("Single", "Range"),selected = c("Single"), inline = TRUE),
    HTML("Date:"), uiOutput(ns("dateChooser")), 
    uiOutput(ns("datewarning")),
    HTML("Label:"), textInput(ns("label"), label = "Name", value = " ", placeholder = "Label"),
    HTML("Amount:"), numericInput(ns("amount"), label = "Amount", min=0, value =0.00),
    actionButton(ns("actionInsert"), "Insert", class="btn-primary")
  )
}

# Define server logic required to draw a histogram
insert_value_server <- function(input, output, session, category) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    timestampTo = NULL,
    timestampFrom = NULL,
    date = NULL,
    name = NULL,
    amount = NULL,
    type = NULL,
    category = NULL,
    trigger = 0
  )
  observeEvent(input$timestamp, {
    warning = NA
    warntext = ''
    
    dateFrom = ymd(input$timestamp[1])
    if (is.na(dateFrom)) { warning = "Invalid Date (use dd-mm-yyyy)."
    }
    
    else if (ymd(input$timestamp[1]) < ymd(20000101)) { warning = "Date below year 2000 (on purpose?)." }
    if (input$dateType == "Range") {
      dateTo = ymd(input$timestamp[1])
      if (is.na(dateTo)) { warning = "Invalid To-Date (use dd-mm-yyyy)." 
      } else if (dateFrom > dateTo) { warning = "To-Date is small er than From-Date." }
    }
    
    if (is.na(warning)) {
      warntext = '' 
    } else { 
      warntext= paste(icon('warning'), "<small>", warning, "</small>") 
    }
    
    output$datewarning<- renderUI({
      HTML(warntext)
    })
  })
  
  observeEvent(input$dateType, {
    output$dateChooser<- renderUI({
      ui <- dateInput(ns("timestamp"), label = "Date", format = "dd-mm-yyyy")
      if (input$dateType == "Range") {
        ui <- dateRangeInput(ns("timestamp"), label = "Date", format = "dd-mm-yyyy")
      }
      return(ui)
    })
  })
  
  observeEvent(input$actionInsert, {
    if (input$dateType == "Range") { 
      toReturn$timestampFrom = input$timestamp[1]
      toReturn$timestampTo = input$timestamp[2]
    } else {
      toReturn$timestampTo = input$timestamp
        toReturn$timestampFrom = input$timestamp
    }
    
    toReturn$label <- input$label
    toReturn$amount <- input$amount
    toReturn$trigger <- toReturn$trigger + 1
    toReturn$category <- category()
    removeModal()
  })
  
  return(toReturn)
}