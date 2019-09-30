#install.packages("RMySQL")
library(markdown)
library(shiny)
library(shinyWidgets)
library(RMySQL)

pkg = c('DT', 'quadprog', 'plotly', 'quantmod', 'PerformanceAnalytics')
new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
  install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)


# con<-dbConnect(MySQL(),user="GTAA",password="gtaa",dbname="gtaa",host="dudrb1418.codns.com")
# #sql <- paste("select cd '코드명' from gtaa_ticker column1 limit 100")
# sql <- paste("select cd '코드명' from gtaa_ticker where cd not like '0%' and cd not like '1%' and cd not like '2%' and cd not like '3%' and cd not like '4%' and cd not like '5%' and cd not like '6%' and cd not like '7%' and cd not like '8%' and cd not like '9%'")
# rows <- dbGetQuery(con, sql)
# mychoices <- c(rows)
mychoices <- c("SPY","IEV","EWJ","EEM","TLT","IEF","IYR","RWX","GLD","DBC")


ui = navbarPage(
  "Global Dynamic Asset Allocation",
  theme = shinythemes::shinytheme("cosmo"),
  
  # Main Page: Portfolio Return
  tabPanel("Portfolio",
           tabsetPanel(
             type = "tabs",
             tabPanel("Raw Data",
                      br(),
                      plotlyOutput("plot1"),
                      br())
           )
  )
)



server = function(input, output) {
  
  # Download Price Data
  symbols = c("SPY","IEV","EWJ","EEM","TLT","IEF","IYR","RWX","GLD","DBC")
  cnt = length(symbols)
  
  withProgress(message = 'Download Data', value = 0, { #진행상황을 알리는 것 
    for (i in 1:cnt) {
      getSymbols(symbols[i], src='yahoo')
      incProgress(1/cnt, paste0(symbols[i]))
    }
  })
  
  
  # Bind Price Data
  prices = do.call(merge, lapply(symbols, function(x) Ad(get(x))))
  rets = na.omit(Return.calculate(prices))
  names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
  
  # Raw Return Chart
  output$plot1 <- renderPlotly({
    
    plot_ly(data = data.frame(cumprod(1 + rets[paste0(input$`range3`[1],"::",input$`range3`[2])])-1) %>%
              cbind('Date' = rownames(.), .),
            x = ~Date, y = ~SPY, name = 'SPY', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~IEV, name = 'IEV', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~EWJ, name = 'EWJ', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~EEM, name = 'EEM', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~TLT, name = 'TLT', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~IEF, name = 'IEF', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~IYR, name = 'IYR', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~RWX, name = 'RWX', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~GLD, name = 'GLD', type = 'scatter', mode = 'line') %>%
      add_trace(y = ~DBC, name = 'DBC', type = 'scatter', mode = 'line') %>%
      layout(title = 'ETF Raw Return',
             xaxis = list(title = "",
                          type = 'date',
                          tickformat = '%y-%m'),
             yaxis = list(title = "",
                          tickformat = '%'))
    
  })
  
  
}

shinyApp(ui = ui, server = server)

