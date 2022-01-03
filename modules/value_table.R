
# Define UI for application that draws a histogram
value_table_ui <- function(id) {
  ns = NS(id)
  list(
    dataTableOutput(ns("valueTable"))
  )
}

# Define server logic required to draw a histogram
value_table_server <- function(input, output, session, cat, df) {
  ns <- session$ns

  output$valueTable <- renderDataTable({
   cat_df <- df() %>% filter(category == cat, event == "new_value")
   cat_df <- cat_df %>% select(eventid, timestamp,label, amount) %>%
     arrange(desc(timestamp)) %>%
     mutate(timestamp = format(date(as.character(timestamp)), "%d %b %Y"),
            amount = sprintf("%.2f", amount)) %>%
     rename(`Date` = timestamp, `Amount` = amount, `Name` = label, ` ` = eventid) 

   dtable = datatable(cat_df, options = list(pageLength = 50, dom='frtp'), rownames= FALSE)
   return(dtable)
  })

  
}