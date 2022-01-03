library(shiny)
library(shinyjs)
library(plotly)
library(tidyverse)
library(lubridate)
library(DT)

# Always get full stack trace
options(shiny.fullstacktrace=TRUE)
# Show milliseconds on timestamps
options("digits.secs"=6)

source("utils/utils.R", local = T)
source("utils/calc_utils.R", local = T)

source("modules/data_module.R", local = T)
source("modules/category_creator_dialog.R", local = T)
source("modules/category_removal_dialog.R", local = T)
source("modules/category_page.R", local = T)
source("modules/category_aggregator.R", local = T)
source("modules/overview_page.R", local = T)
source("modules/insert_value_dialog.R", local = T)
source("modules/remove_value_dialog.R", local = T)
source("modules/value_plot.R", local = T)
source("modules/value_table.R", local = T)
source("modules/value_aggregator.R", local = T)


# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("www/custom.css"),
  useShinyjs(),
  tabsetPanel(type="pills", id = "categorynav",
    tabPanel(title = "All",
             div(class="main-content",  overview_page_ui("overviewPage"))
    ),
    tabPanel('addTab', title = div(class="text-center", icon('plus')),
             div(class="main-content", category_creator_ui("createCategory"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    r <- reactiveValues(df = NULL, cat = NULL)
    data <- callModule(data_server,"data_import")
    newcat <- callModule(category_creator_server,"createCategory")
    newval <- callModule(insert_value_server,"insertValue", reactive(r$cat))
    remove <- callModule(remove_value_server,"removeValue")
    removecat <- callModule(category_removal_server,"removeCategory", reactive(r$df))
    callModule(overview_page_server,"overviewPage", reactive(r$df))

    observeEvent(data$trigger, {
      req(data$trigger > 0)
      r$df <- data$df
      
      cats <- data$df %>% filter(event == "new_category") %>% distinct(category, .keep_all=T)
      lapply(1:length(cats$category), function(i) {
        insertTab("categorynav",
                   select = FALSE,
                   target = "All",
                   tab = tabPanel(title = div(class="text-center", icon(cats[i,][['icon']])),
                                  id = cats[i,][['category']],
                                  value = cats[i,][['category']],
                                  div(class="main-content",
                                      category_page_ui(u_esc(cats[i,][['category']]))
                                  )
                   )
        )
        callModule(category_page_server, u_esc(cats[i,][['category']]), cats[i,][['category']], reactive(r$df))
      })
    })
    
    observeEvent(input$categorynav, {
      r$cat <- input$categorynav
    })
    
    observeEvent(newcat$trigger, {
      req(newcat$trigger > 0)
      new_id <- max(r$df$eventid, na.rm=T) + 1
      r$df <- r$df %>% add_row(category = newcat$name,
                               icon= newcat$icon,
                               event= "new_category",
                               eventid = new_id,
                               timestamp = newcat$timestamp,
                               submitted = as.POSIXct(now(tzone = ""), format = "%Y-%m-%d %H:%M:%OS"))
      insertTab("categorynav",
                select = TRUE,
                target = "All",
                tab = tabPanel(title = div(class="text-center", icon(newcat$icon)),
                             id = newcat$name,
                             value = newcat$name,
                             div(class="main-content",
                                 category_page_ui(u_esc(newcat$name))
                             )
                    )
                )
      callModule(category_page_server, u_esc(newcat$name), newcat$name, reactive(r$df))
      data_save(r$df)
    })
    
    observeEvent(newval$trigger, {
      req(newval$trigger > 0)
      new_id <- max(r$df$eventid, na.rm=T) + 1
      r$df <- r$df %>% add_row(timestamp = newval$timestamp,
                                label= newval$label,
                                amount= newval$amount,
                                submitted = as.POSIXct(now(tzone = ""), format = "%Y-%m-%d %H:%M:%OS"),
                                category = newval$category,
                                event= "new_value",
                                eventid = new_id)
      data_save(r$df)
    })
    
    observeEvent(remove$trigger, {
      req(remove$trigger > 0)
      r$df <- r$df %>% filter(!eventid == remove$val)
      
      data_save(r$df)
    })
    
    observeEvent(removecat$trigger, {
      req(removecat$trigger > 0)
      r$df <- r$df %>% filter(!category == removecat$category)
      data_save(r$df)
      removeTab("categorynav", removecat$category)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
