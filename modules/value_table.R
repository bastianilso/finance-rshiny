
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
   cat_df <- cat_df %>% select(eventid, timestampTo,label, amount) %>%
     arrange(desc(timestampTo)) %>%
     mutate(timestampTo = format(date(as.character(timestampTo)), "%d %b %Y"),
            amount = sprintf("%.2f", amount)) %>%
     rename(`Date` = timestampTo, `Amount` = amount, `Name` = label, ` ` = eventid) 

   dtable = datatable(cat_df, options = list(pageLength = 50, dom='frtp'), rownames= FALSE)
   return(dtable)
  })

  
}