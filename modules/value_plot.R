
# Define UI for application that draws a histogram
value_plot_ui <- function(id) {
  ns = NS(id)
  list(
    
    fluidRow(class="vis-plot",
    plotlyOutput(ns("valuePlot"), width = "100%")
    )
  )
}

# Define server logic required to draw a histogram
value_plot_server <- function(input, output, session, cat, df) {
  ns <- session$ns

  output$valuePlot <- renderPlotly({
    cat_df <- df() %>% filter(category == cat, event == "new_value")
    monthly_cost = c_spancost(df() %>% filter(category == cat), span = "month")

    vistemplate <- plot_ly(width="1000") %>%
      config(scrollZoom = FALSE, displaylogo = FALSE, modeBarButtonsToRemove = c("select2d","hoverCompareCartesian", "toggleSpikelines","toImage", "sendDataToCloud", "editInChartStudio", "lasso2d", "drawclosedpath", "drawopenpath", "drawline", "drawcircle", "eraseshape", "autoScale2d", "hoverClosestCartesian","toggleHover", "")) %>%
      layout(dragmode = "pan", showlegend = FALSE)
    
    ## Different options:
    ## Expenses visualized: simple bar chart
    ## Annual Aggregate: (x axis = year, each year has different value)
    ## Quarterly Aggregate: (x axis = quarter, each quarter has different value)
    ## Monthly Aggregate: (x axis = month, each month has different value)
    
    # extract year, extract month
    # add missing values, e.g. missing months. (basically perform range, from oldest month to newest month)
    # distribute the expense across the months which are not represented.
    # group_by(year, month)
    # summarize(cost = sum(amount))
    
    cat_df <- df() %>% filter(category == cat, event == "new_value")
    today = as.POSIXct(now(tzone = ""), format = "%Y-%m-%d %H:%M:%OS")
    month_cap = 12
    date_limit = today - months(month_cap)
    cat_df = cat_df %>% mutate(within_range = ifelse(timestampTo > date_limit, month_cap,NA))
    
    max_expense = cat_df %>% filter(!is.na(within_range)) %>%
      summarize(amount = max(amount, na.rm=T))
    yaxis_upper = as.integer(max_expense + 250)
    max_date = max(cat_df$timestampTo, na.rm=T)
    
    yaxis_tick = 100
    yaxis_tick = ifelse(max_expense > 1000, 250, yaxis_tick)
    yaxis_tick = ifelse(max_expense > 2000, 500, yaxis_tick)
    yaxis_tick = ifelse(max_expense > 5000, 1000, yaxis_tick)
    yaxis_tick = ifelse(max_expense > 10000, 2500, yaxis_tick)
    
    mcosts <- c_monthcosts(df() %>% filter(category == cat))
    vistemplate %>%
      add_trace(name="Expenses", data=cat_df, x=~floor_date(timestampTo,"month"), y=~amount,
                type="bar", color=I('white'), marker=list(line=list(width=1.5, color='rgb(51,122,183)')),
                text=~paste(str_trunc(label,10),'<br>',sprintf("%.0f", amount),"kr."), textfont=list(size=16, color='rgb(51,122,183)'), textposition="outside") %>%
      add_trace(name="Monthly Average", x=c(date_limit,today+months(1)), y=c(monthly_cost$amount,monthly_cost$amount),
                type="scatter",mode="lines", color=I('rgba(186,213,250,1)'),
                line=list(width=2)) %>%
      add_trace(name="Monthly Average", x=today+months(1), y=monthly_cost$amount, textfont=list(size=16),
                type="scatter",mode="text", color=I('rgba(141,186,242,1)'),
                text=paste("Average:",sprintf("%.0f", monthly_cost),"kr."), textposition="top left") %>%
      add_trace(name="Monthly averages", data=mcosts, x=~month, y=~cost,
                text=~paste(sprintf("%.0f", cost),"kr."),textposition="top center", textfont=list(size=16),
                type="scatter", mode="lines+markers+text", color=I('rgba(51,122,183,1)'),
                line=list(width=2.5), marker=list(size=8)) %>%
      layout(xaxis=list(range=c(date_limit,today+months(1)), tickfont=list(size=14),
                        type = 'date', dtick = 'M1', tickformat = "%b<br>%Y", title=" "),
             yaxis=list(fixedrange=T, range=c(0, yaxis_upper), title=' ', ticksuffix=' kr.', tickmode = 'linear', dtick=yaxis_tick))
      
    
    # oldest_date = min(cat_df$timestamp)
    # interval = interval(oldest_date, today)
    # months_span = interval %/% months(1)
    # period_cost = cat_df %>% filter(timestamp >= oldest_date) %>%
    #   select(amount) %>% summarize(amount = sum(amount))
    # cost = period_cost / months_span
    
    ## aggregate the data to a month each for the given period
    ## some months will be high, some months will be low
    ## 
    ## scatter plot, one point per month. (month = x-axis, price = y-axis)
    
    
  })
    
  # output$valuePlot <- renderPlotly({
  #   cat_df <- df() %>% filter(category == cat, event == "new_value")
  #   cat_df <- cat_df %>% select(eventid, timestamp,label, amount) %>%
  #     mutate(timestamp = format(date(as.character(timestamp)), "%d %b %Y"),
  #            amount = sprintf("%.2f", amount)) %>%
  #     rename(`Date` = timestamp, `Amount` = amount, `Name` = label, ` ` = eventid) 
  #   
  #   dtable = datatable(cat_df, options = list(pageLength = 50, dom='frtp'), rownames= FALSE)
  #   return(dtable)
  # })
  
  
}