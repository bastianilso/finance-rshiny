
# Define UI for application that draws a histogram
category_removal_ui <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>Remove Category</h3>"),
    selectizeInput(ns("categoryid"), label = "Name", choices = NULL, selected=NULL),
    HTML("<p>Warning: This cannot be undone. Removes all categories with this name.</p>"),
    actionButton(ns("actionRemove"), "Remove", class="btn-danger")
  )
}

# Define server logic required to draw a histogram
category_removal_server <- function(input, output, session, df) {
  ns <- session$ns
  
  toReturn <- reactiveValues(
    category = NULL,
    trigger = 0
  )
  
  observeEvent(df(), {
    req(!is.null(df()))
    dfc <- df() %>% distinct(category, .keep_all = T)
    updateSelectizeInput(inputId = "categoryid",
                         choices = dfc$category,
                         selected= NULL)
  })
  
  observeEvent(input$categoryid, {
    req(!is.null(df()))
    req(input$categoryid == "")
    dfc <- df() %>% distinct(category, .keep_all = T)
    updateSelectizeInput(inputId = "categoryid",
                         choices = dfc$category,
                         selected= NULL)
  })
  
  observeEvent(input$actionRemove, {
    toReturn$category <- input$categoryid
    toReturn$trigger <- toReturn$trigger + 1
    removeModal()
  })
  
  return(toReturn)
}