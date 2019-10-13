

### server

shinyServer(function(input,output,session){
  
  # 
  hideTab(inputId = "inTabset", target = "tab2")
  hideTab(inputId = "inTabset", target = "tab3")
  hideTab(inputId = "inTabset", target = "tab4")
  
  
  
  # 자산(EWF) 선택
  observeEvent(input$pickerInput1,{
    symbols = c(input$pickerInput1)
    EWF_CNT = length(symbols)
    
    
    # 전략 선택
    observeEvent(input$inRadioButtons1,{
      
      if (EWF_CNT >= 2) {
        # 동일비 비율조절 컨트롤 갯수
        inputs <- lapply(1:EWF_CNT, function(i){
          numericInput(inputId = paste0("numeric", i), 
                       label = paste0("numeric ", i), 
                       min = 0.1, max = 0.9, step = 0.1, value = 0.1, width='70px')
        })
        inputs2 <- pickerInput(inputId = "pickerInput2", label='', c(symbols), multiple = TRUE)
        
        str <- input$inRadioButtons1
        if (str == "동일비중") {
          output$out <- renderUI({
            inputs
          })
          output$out2 <- renderUI({
            hidden(inputs2)
          })
        }
        
        withProgress(message = 'Download Data', value = 0, { #진행상황을 알리는 것 
          for (i in 1:EWF_CNT) {
            getSymbols(symbols[i], src='yahoo')
            incProgress(1/EWF_CNT, paste0(symbols[i]))
          }
        })
        
        # Bind Price Data
        prices = do.call(merge, lapply(symbols, function(x) Ad(get(x))))
        rets = na.omit(Return.calculate(prices))
        names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
        
      }
    })
    
  })
  
  
  
  
  
  
  
  observeEvent(input$goButton,{
    
    # 
    showTab(inputId = "inTabset", target = "tab2")
    showTab(inputId = "inTabset", target = "tab3")
    showTab(inputId = "inTabset", target = "tab4")
    updateTabsetPanel(session, "inTabset", selected = "tab2")
    
    
    # Download Price Data
    symbols = c(input$pickerInput1)
    EWF_CNT = length(symbols)
    
    withProgress(message = 'Download Data', value = 0, { #진행상황을 알리는 것 
      for (i in 1:EWF_CNT) {
        getSymbols(symbols[i], src='yahoo', from = '1900-01-01')
        incProgress(1/EWF_CNT, paste0(symbols[i]))
      }
    })
    
    # Bind Price Data
    prices = do.call(merge, lapply(symbols, function(x) Ad(get(x))))
    rets = na.omit(Return.calculate(prices))
    names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
    
    # 공통 변수
    WTS_COM <- NULL
    
    st <- input$inRadioButtons1
    if (st == "동일비중") {
      
      # Get Weight
      fee = 0.00
      rateA = 0.3
      rateB = 0.7
      #rateC = 0.3
      #rateTot = paste0(rateA,":",rateB,":",rateC)
      rateTot = paste0(rateA,":",rateB)
      
      # Portfolio Return
      port_gross = Return.portfolio(R = rets,
                                    weights = c(rateA,rateB),
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
      WTS_COM = port_gross$BOP.Weight
      
      
      #  Performance analysis
      i= 1  # <---------------------------------
      
      if(i ==1 ){
        rateA = port_net
      }
      else if(i == 2 ){
        rateA =  cbind(port_net,rets)
      }
      Performance_analysis = table.AnnualizedReturns(rateA) # 연평균,표준편차,샤프비율
      PA = rbind(Performance_analysis, maxDrawdown(rateA)) # mdd
      PA= rbind(PA,Return.cumulative(rateA)) # 누적 수익률 
      PA =rbind(PA,UpsideFrequency(rateA, MAR = 0)) # 승률
      row.names(PA) = c('Annualized Return','Annualized Std Dev','Annualized Sharpe (Rf=0%)','max Drawdown','Cumulative Return','UpsideFrequency') #c('연평균수익률','연평균위험','연평균샤프비율','최대낙폭','총수익률','승률') 
      #colnames(PA) = c(rateTot, "SPY","TLT")
      Performance_analysis = t(PA)
      
      
      # Portfolio Return Graph
      port_net = rateA
      output$port_ret = renderPlotly({
          df <- data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1)
          p <- plot_ly(data =
                         data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1) %>%
                         cbind('Date' = rownames(.), .), type = 'scatter', mode = 'lines'
          ) %>%
          
          layout(title = 'Portfolio Cumulative Return',
                 xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
                 yaxis = list(title = "")) 
        
          cnt2 = EWF_CNT
          for (i in 1:cnt2) {
            if (i == 1)
              p <- add_lines(p, x=~Date, y=df[,i], name=rateTot)
          }
          p
      })
      
      # Performance analysis
      output$Performance_analysis = renderTable({round(Performance_analysis,4) %>%
          cbind(' ' = rownames(.), .) %>%
          `rownames<-` (NULL)})        # 소수 문제 !!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      
    }
    
    
    
    
    
    #Portfolio selection
    ##################################################################################################################
    # Portfolio Yearly Graph
    output$port_ret_yr = renderPlotly({
      plot_ly(data = port_net_yr, x = ~ Year, y = ~ Returns, type = "bar") %>%
        layout(title = 'Yearly Return', 
               xaxis = list(title = ""), 
               yaxis = list(title = "", tickformat = '%'))
    })
    
    # Portfolio Sub period
    output$port_table = DT::renderDataTable({
      data.frame(round(port_net[paste0(input$`range`[1],"::",input$`rang`[2])], 5)) %>%
        cbind('Date' = rownames(.), .) %>%
        `rownames<-` (NULL)
    })
    
    # Portfolio Yearly Return
    output$port_table_year = DT::renderDataTable({
      port_net_yr
    })
    
    
    
    
    
    #Weight selection
    ##################################################################################################################
    # Present Weight
    wts_now = t(coredata(round(last(WTS_COM)[, last(WTS_COM) != 0], 4)))
    wts_now = data.frame(ticker = rownames(wts_now), WTS_COM = wts_now)
    
    output$wts_now = renderPlotly({
      plot_ly(x = ~wts_now$WTS_COM,
              y = ~reorder(wts_now$ticker, wts_now$WTS_COM),
              type = 'bar', orientation = 'h') %>%
        layout(title = 'Current Portfolio Ratio Composition',
               xaxis = list(title = "", tickformat = "%"),
               yaxis = list(title = "")) %>%
        add_annotations(xref = 'x1', yref = 'y',
                        x = wts_now$WTS_COM + 0.01, y = wts_now$ticker,
                        text = paste(round(wts_now$WTS_COM * 100, 2), '%'),
                        font = list(family = 'Arial', size = 12),
                        showarrow = FALSE)
    })
    
    
    # Historical Weight
    output$wts_hist = renderPlotly({
      df <- data.frame(WTS_COM[paste0(input$`range`[1],"::",input$`range`[2])])
      p <- plot_ly(data = 
                     data.frame(WTS_COM[paste0(input$`range`[1],"::",input$`range`[2])]) %>%
                     cbind('Date' = rownames(.), .), mode = 'none', type = 'scatter', stackgroup = 'one'
      ) %>% 
        
        layout(title = 'Historical Portfolio Weight',
               xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
               yaxis = list(title = "", tickformat = '%'))
      
      for (i in 1:EWF_CNT) {
        p <- add_trace(p, x=~Date, y = df[,i], name = symbols[i])
      }
      p
    })
    
    # Weight Table
    output$wts_table = DT::renderDataTable({
      data.frame(round(WTS_COM[paste0(input$`range`[1],"::",input$`range`[2])], 4)) %>%
        cbind('Date' = rownames(.), .) %>%
        `rownames<-` (NULL)
      
    })
    
    
    
    
    
    #Raw Data selection
    ##################################################################################################################
    # Raw Return Chart
    output$plot_etf_raw <- renderPlotly({
      
      df <- data.frame(cumprod(1 + rets[paste0(input$`range`[1], "::", input$`range`[2])])-1)
      p <-  plot_ly(data = 
                      data.frame(cumprod(1 + rets[paste0(input$`range`[1], "::", input$`range`[2])])-1) %>%
                      cbind('Date' = rownames(.), .), mode = 'line', type = 'scatter'
      ) %>%
        
        layout(title = 'ETF Raw Return',
               xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
               yaxis = list(title = "", tickformat = '%'))
      
      for (i in 1:EWF_CNT) {
        p <- add_trace(p, x=~Date, y = df[,i], name = symbols[i])
      }
      p
    })
    
    # Raw Return Data Table
    output$dataTable_etf_raw = DT::renderDataTable({
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
    ##################################################################################################################
    
  })
  
  
  # Min vol syntex(Strategy)
  output$ex1 <- renderUI({
    withMathJax(helpText('$$min\\ \\sigma_p $$'),
                helpText(('$$s.t. \\sum w_i = 1, 0.1 ≤ w_i ≤ 0.3$$'))
    )
  })
  
  # Universe Data Frame(Universe)
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
  
  
  
})