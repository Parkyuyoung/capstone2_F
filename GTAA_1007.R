
library(shiny)
library(d3heatmap)
library(rsconnect)
library(markdown)

rsconnect::setAccountInfo(name='assetallocationforalpha',
                          token='ABE5B7419566B81A9FF2ED280307D999',
                          secret='UdFG5ls7tSuOCkU83ZDMCBO6A5HjLWiRJtPDBJiN')

# if(!require("devtools"))
#     install.packages("devtools")
# devtools::install_github("rstudio/rsconnect")

pkg = c('DT', 'quadprog', 'plotly', 'quantmod', 'PerformanceAnalytics')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)



ui = navbarPage(
    "Global Tactical Asset Allocation",
    theme = shinythemes::shinytheme("united"),
    
    # Main Page: Portfolio Return
    tabPanel("Portfolio",
             tabsetPanel(
                 type = "tabs",
                 tabPanel("Cumulative Return",
                          br(),
                          dateRangeInput('range', 'Date Range',
                                         start = '2008-01-01',
                                         end = Sys.Date(),
                                         min = '2008-01-01',
                                         max = Sys.Date(),
                                         format = "yyyy-mm-dd",
                                         separator = " - "),
                          
                          br(),
                          plotlyOutput("port_ret"),
                          fluidRow(
                              column(12, tableOutput( "Performance_analysis"))
                              ),
                          br()#,
                          # plotlyOutput("port_ret_yr"),
                          # br(),
                          # br(),
                          # fluidRow(
                          #     
                          #     column(6, DT::dataTableOutput("port_table")),
                          #     column(6, DT::dataTableOutput("port_table_year"))
                          # )
                          
                 ),
                 tabPanel("Raw Data",
                          br(),
                          dateRangeInput('range4', 'Date Range',
                                         start = '2008-01-01',
                                         end = Sys.Date(),
                                         min = '2008-01-01',
                                         max = Sys.Date(),
                                         format = "yyyy-mm-dd",
                                         separator = " - "),
                          plotlyOutput("raw_ret_chart"),
                          br(),
                          br(),
                          br(),
                          DT::dataTableOutput("raw_data"),
                          br(),
                          fluidRow(
                              column(1, offset = 10,
                                     downloadButton("downloadData", "Download Data")
                              )),
                          br())
                 
             )
    ),
    
    # Description for strategy
    tabPanel("Description",
             tabsetPanel(
                 type = "tabs",
                 
                 tabPanel("Strategy",
                          br(),
                          strong("Global Tactical Asset Allocation"),
                          br(),
                          tags$ul(
                              tags$li("Strategy to perform asset allocation using momentum"),
                              tags$li("Invested in top 5 assets, which were among the top 10 global assets"),
                              tags$li("Calculate momentum indices using returns from 3 months to 12 months")
                          ),
                          br(),
                          strong("Weight of each asset"),
                          withMathJax(),
                          br(),
                          tags$ul(
                              tags$li("Variance of the portfolio is minimized"),
                              tags$li("The sum of the total weights is 1"),
                              tags$li("At least 10% and maximum 30% for each category to prevent corner solution")
                          ),
                          uiOutput('ex1'),
                          br(),
                          strong("ETC"),
                          br(),
                          tags$ul(
                              tags$li("Use a adjusted stock price that includes dividends"),
                              tags$li("Buy / sell commission 30bp"),
                              tags$li("Rebalancing by the end of the month")
                          )
                 ),
                 
                 tabPanel("Universe",
                          br(),
                          tableOutput("univ")
                 )
             )),
    
    # Author: Henry
    tabPanel("About developer",
             strong("홍성주"),
             tags$ul(
                 tags$li("Phone nubmer : 010-8857-6301"),
                 tags$li("E-mail : tjdwn0817@naver.com"),
                 tags$li("github : season0304"),
                 tags$li("major : Economics and Finance "),
                 br()
             ),
             div(),
             strong("박유영"),
             tags$ul(
                 tags$li("Phone nubmer : 010-9616-4766"),
                 tags$li("E-mail : yuyoung@naver.com"),
                 tags$li("github : parkyuyoung"),
                 tags$li("major : Data Analysis "),
                 br()
             ),
             div(),
             strong("김민찬"),
             tags$ul(
                 tags$li("Phone nubmer : 010-2864-3564"),
                 tags$li("E-mail : yuyoung@naver.com"),
                 tags$li("github : parkyuyoung"),
                 tags$li("major : Computer Science "),
                 br()
                 
             ),
             div(),
             strong("최영규"),
             tags$ul(
                 tags$li("Phone nubmer : 010-2019-0700"),
                 tags$li("E-mail : yuyoung@naver.com"),
                 tags$li("github : parkyuyoung"),
                 tags$li("major : Computer Science  "),
                 br()
                 
             )
    )
    
)


server = function(input, output) {
    
    # Download Price Data
    symbols = c("SPY","TLT")#c("SPY","IEV","EWJ","EEM","TLT","IEF","IYR","RWX","GLD","DBC")#c("SPY","TLT")
    
    # 몇퍼센트 다운됬는지 진행상황을 알리는 함수 
    withProgress(message = 'Download Data', value = 0, { 
        n = length(symbols)
        for (i in 1:n) {
            getSymbols(symbols[i], src='yahoo', from = "1900-01-01")
            #incProgress(1/n, detail = paste0(symbols[i]) )
        }
    })
    
    # Bind Price Data
    prices = do.call(merge, lapply(symbols, function(x) Ad(get(x))))
    # make a daily return 
    rets = na.omit(Return.calculate(prices))
    #  use Adjust 
    names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
    
    
    
    
    
    ############################################ Portfolio selection ############################################################# 
    
    
    
    
    ####################################  Stragic Algoritms################################################################## 
    
    
    # Get Weight
    
    fee = 0.00
    A = 0.7
    B = 0.3
    C = paste0(A,":",B)
    port_gross = Return.portfolio(R = rets,
                                  weights = c(A,B),
                                  rebalance_on = 'months',
                                  verbose = TRUE)
    
    port_turnover = xts(rowSums(abs(port_gross$BOP.Weight - lag(port_gross$EOP.Weight)), na.rm = TRUE),
                        order.by = index(port_gross$BOP.Weight))
    port_net = port_gross$returns - (port_turnover * fee)
    names(port_net) = 'Returns'
    port_net_yr = round(apply.yearly(port_net, Return.cumulative), 5)
    port_net_yr = data.frame(port_net_yr) %>%
        cbind('Year' = rownames(.), .)
    port_net_yr$Year = substring(port_net_yr$Year, 1, 4)
    rownames(port_net_yr) = NULL
    wts = port_gross$BOP.Weight
    
    #  Performance analysis
    
    i= 2  # <---------------------------------
    
    if(i ==1 ){
    A = port_net
    }else if(i == 2 ){
        A =  cbind(port_net,rets)
    }
    Performance_analysis = table.AnnualizedReturns(A) # 연평균,표준편차,샤프비율
    PA = rbind(Performance_analysis, maxDrawdown(A)) # mdd
    PA= rbind(PA,Return.cumulative(A)) # 누적 수익률 
    PA =rbind(PA,UpsideFrequency(A, MAR = 0)) # 승률
    row.names(PA) = c('Annualized Return','Annualized Std Dev','Annualized Sharpe (Rf=0%)','max Drawdown','Cumulative Return','UpsideFrequency') #c('연평균수익률','연평균위험','연평균샤프비율','최대낙폭','총수익률','승률') 
    colnames(PA) = c(C,"SPY","TLT")
    Performance_analysis = t(PA)
    
    
    
    
    
    
    #########################################   WEIGHT   ################################################   
    
    # Present Weight
    wts_now = t(coredata(round(last(wts)[, last(wts) != 0], 4)))
    wts_now = data.frame(ticker = rownames(wts_now), wts = wts_now)
    
    output$wts_now = renderPlotly({
        plot_ly(x = ~wts_now$wts,
                y = ~reorder(wts_now$ticker, wts_now$wts),
                type = 'bar', orientation = 'h') %>%
            layout(title = 'Current Portfolio Ratio Composition',
                   xaxis = list(title = "",
                                tickformat = "%"),
                   yaxis = list(title = "")) %>%
            add_annotations(xref = 'x1', yref = 'y',
                            x = wts_now$wts + 0.01, y = wts_now$ticker,
                            text = paste(round(wts_now$wts * 100, 2), '%'),
                            font = list(family = 'Arial', size = 12),
                            showarrow = FALSE)
    })
    
    # Historical Weight
    output$wts_hist = renderPlotly({
        plot_ly(data = data.frame(wts[paste0(input$`range2`[1],"::",input$`range2`[2])]) %>%
                    cbind('Date' = rownames(.), .),
                x = ~Date, y = ~SPY, name = 'SPY', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
            
            add_trace(y = ~TLT, name = 'TLT', type = 'scatter', mode = 'none', stackgroup = 'one') %>%
            
            layout(title = 'Historical Portfolio Weight',
                   xaxis = list(title = "",
                                type = 'date',
                                tickformat = '%y-%m'),
                   yaxis = list(title = "",
                                tickformat = '%'))
    })
    
    # Weight Table
    output$wts_table = DT::renderDataTable({
        data.frame(round(wts[paste0(input$`range2`[1],"::",input$`range2`[2])], 4)) %>%
            cbind('Date' = rownames(.), .) %>%
            `rownames<-` (NULL)
        
    })
    
    
    #########################################  totall ETF return   ################################################   
    
    
    
    # Raw Return Chart
    output$raw_ret_chart = renderPlotly({
        plot_ly(data = data.frame(cumprod(1 + rets[paste0(input$`range4`[1],"::",input$`range4`[2])])-1) %>%
                    cbind('Date' = rownames(.), .),
                x = ~Date, y = ~SPY, name = 'SPY', type = 'scatter', mode = 'line') %>%
            add_trace(y = ~TLT, name = 'TLT', type = 'scatter', mode = 'line') %>%
            layout(title = 'ETF Raw Return',
                   xaxis = list(title = "",
                                type = 'date',
                                tickformat = '%y-%m'),
                   yaxis = list(title = "",
                                tickformat = '%'))
    })

    # Raw Return Data Table
    output$raw_data = DT::renderDataTable({
        data.frame(round(rets,4)) %>%
            cbind('Date' = rownames(.), .) %>%
            `rownames<-` (NULL)
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("price_data", ".csv", sep="")
        },
        content = function(file) {
            write.csv(data.frame(round(rets,4)), file)
        }
    )
######################################### Portfolio Return ###########################################
    # Portfolio Return Graph
    port_net = A
    cnt = length(symbols)
    output$port_ret = renderPlotly({
        df <- data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1)
        p <- plot_ly(data =
                         data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1) %>%
                         cbind('Date' = rownames(.), .), type = 'scatter', mode = 'lines'
        ) %>%
            
            layout(title = 'Portfolio Cumulative Return',
                   xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
                   yaxis = list(title = "")) 
        
        cnt2 = cnt+1
        for (i in 1:cnt2) {
            if (i == 1)
                p <- add_lines(p, x=~Date, y=df[,i], name=C)
            else 
                p <- add_lines(p, x=~Date, y=df[,i], name=symbols[i-1])
        }
        p
    })
    
    # Performance analysis
    output$Performance_analysis = renderTable({round(Performance_analysis,3) %>%
            #cbind('Date' = rownames(.), .) %>%
            cbind(' ' = rownames(.), .) %>%
            `rownames<-` (NULL)})        # 소수 문제 !!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    
    # Portfolio Yearly Graph
    output$port_ret_yr = renderPlotly({
        plot_ly(data = port_net_yr,
                x = ~ Year,
                y = ~ Returns,
                type = "bar") %>%
            layout(title = 'Yearly Return',
                   xaxis = list(title = ""),
                   yaxis = list(title = "",
                                tickformat = '%')) 
    })
    
    # Portfolio Sub period
    output$port_table = DT::renderDataTable({
        data.frame(round(port_net[paste0(input$`range`[1],"::",input$`range`[2])], 5)) %>%
            cbind('Datee' = rownames(.), .) %>%
            `rownames<-` (NULL)
    })
    
    # Portfolio Yearly Return
    output$port_table_year = DT::renderDataTable({
        port_net_yr
    })
    
    
    # Min vol syntex
    output$ex1 <- renderUI({
        withMathJax(helpText('$$min\\ \\sigma_p $$'),
                    helpText(('$$s.t. \\sum w_i = 1, 0.1 ≤ w_i ≤ 0.3$$'))
        )
    })
    
    # Universe Data Frame
    output$univ <- renderTable({
        data.frame(
            'Asset' = c('Stock', 'Stock', 'Stock', 'Stock', 'Bond', 'Bond',
                        'Alternative', 'Alternative', 'Alternative', 'Alternative'),
            'Specific' = c('US Stock', 'Europe Stock', 'Japan Stock', 'Emerging Stock',
                           'US Longterm Bond', 'US Int Bond', 'US REITs', 'Global REITs',
                           'Gold', 'Commodities'),
            'ETF' = c('SPY', 'IEV', 'EWJ', 'EEM', 'TLT', 'IEF', 'IYR', 'RWX', 'GLD', 'DBC'),
            stringsAsFactors = FALSE
        )
    })
}

shinyApp(ui = ui, server = server)

 