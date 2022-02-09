
# Define UI for application that draws a histogram
category_creator_ui <- function(id) {
  ns = NS(id)
  list(
    HTML("<h3>New Category</h3>"),
    HTML("Category Name:"), textInput(ns("categoryName"), label = "Name", value = " ", placeholder = "Name"),
    HTML("Begin Date:"), dateInput(ns("timestampTo"), label = "Date", format = "dd-mm-yy"),
    div(style="max-width:300px;", 
      checkboxGroupInput(ns("iconChooser"), label = "Icon",
                         choiceNames = list(icon("heart"), icon("stethoscope"),
                                     icon("plane"),
                                     icon("subway"), icon("car"), icon("hotel"),
                                     icon("industry"), icon("life-ring"), icon("music"),
                                     icon("tachometer-alt"), icon("thermometer"),
                                     icon("tv"), icon("tree"),
                                     icon("shopping-bag"), icon("shopping-cart"), 
                                     icon("gamepad"), icon("child"), icon("bicycle"),
                                     icon("balance-scale"), icon("paw"), icon("bolt"),
                                     icon("bath"), icon("chart-line"), icon("suitcase"),
                                     icon("user"), icon("tint"), icon("paint-brush"),
                                     icon("utensils"), icon("envelope-o"), icon("cloud"),
                                     icon("wrench"), icon("university"),icon("futbol-o"),
                                     icon("music"), icon("scissors"), icon("asterisk"),
                                     icon("cube")),
                         choiceValues = list("heart","stethoscope","plane","subway",
                                             "car","hotel","industry","life-ring",
                                             "music","tachometer-alt",
                                             "thermometer","tv","tree",
                                             "shopping-bag","shopping-cart",
                                             "gamepad","child","bicycle","balance-scale",
                                             "paw", "bolt", "bath", "chart-line", "suitcase",
                                             "user", "tint", "paint-brush", "utensils",
                                             "envelope-o", "cloud","wrench","university",
                                             "futbol-o","music", "scissors","asterisk",
                                             "cube"),
                         inline = TRUE),
    ),
    actionButton(ns("actionCreate"), "Create", class="btn-primary"),
    actionButton(ns("actionSettings"), icon('cog'), class="btn-link")
  )
}

# Define server logic required to draw a histogram
category_creator_server <- function(input, output, session) {
  ns <- session$ns

  toReturn <- reactiveValues(
    icon = NULL,
    name = NULL,
    timestampTo = NULL,
    trigger = 0
  )
  
  observeEvent(input$actionCreate, {
    toReturn$icon <- input$iconChooser
    toReturn$name <- input$categoryName
    toReturn$timestampTo <- input$timestampTo
    toReturn$trigger <- toReturn$trigger + 1
  })
  
  observeEvent(input$actionSettings, {
    insertUI(selector = "#actionSettings", where = "afterEnd",
             ui = showModal(modalDialog(category_removal_ui("removeCategory"))))
  })
  
  return(toReturn)
}