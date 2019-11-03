

### server

shinyServer(function(input,output,session) {
  
  # (Cumulative Return, Weight, Raw Data) Tab 탭 숨김
  hideTab(inputId = "inTabset", target = "tab2")
  hideTab(inputId = "inTabset", target = "tab3")
  hideTab(inputId = "inTabset", target = "tab4")
  
  # numeric 숨김
  for (i in 1:10) {
    hide(paste0("numeric_rate_",i))
  }
  hide("numeric_Momentum")
  
  # 공통변수
  EWF <- reactiveValues(lst = list(), len = 0)
  RETS <- reactiveValues(lst = list())
  EQUAL_WEIGHT_RATE <- reactiveValues(lst = c(), lbl = "")#동일비중 포트폴리오 비율
  MOMENTUM_RATE <- reactiveValues(value = 0, lbl = "")#모멘텀 포트폴리오 비율
  
  # 자산선택 이벤트
  observeEvent(input$pickerInput1,{
    
    EWF$lst = c(input$pickerInput1)
    EWF$len <- length(EWF$lst)
    
    if (EWF$len >= 2) {
      
      observeEvent(input$radioBtn1,{
        # numeric 숨김
        for (i in 1:10) {
          hide(paste0("numeric_rate_",i))
        }
        hide("numeric_Momentum")
        
        if (input$radioBtn1 == "radioBtn1_sel1") {#동일비중 포트폴리오
          for (i in 1:EWF$len) {
            toggle(paste0("numeric_rate_",i))
          }
        }
        else if (input$radioBtn1 == "radioBtn1_sel5") {#모멘텀 포트폴리오
          toggle("numeric_Momentum")
        }
      })
      
      #
      getSymbols(input$pickerInput1, src='yahoo', from = '1900-01-01')
      # Bind Price Data
      prices = do.call(merge, lapply(EWF$lst, function(x) Ad(get(x))))
      rets = na.omit(Return.calculate(prices))
      names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
      RETS$lst = rets
      # correlation
      output$heatmap <- renderD3heatmap({d3heatmap(cor(RETS$lst))})
    }
    
  })
  
  # 조회버튼 이벤트
  observeEvent(input$goButton,{
    
    if (is.null(input$pickerInput1) == TRUE || is.null(input$radioBtn1) == TRUE) {
      sendSweetAlert(
        session = session,
        title = "",
        text = "자산과 전략을 선택해주세요.",
        type = "error"
      )
    }
    else {
      
      # (Cumulative Return, Weight, Raw Data Tab) 탭 보여짐
      showTab(inputId = "inTabset", target = "tab2")
      showTab(inputId = "inTabset", target = "tab3")
      showTab(inputId = "inTabset", target = "tab4")
      updateTabsetPanel(session, "inTabset", selected = "tab2")
      
      
      # Get weight
      n = EWF$len
      min = 0.02
      max = 0.5
      
      # Min Variance Portfolio Function (최소 분산 포트폴리오  )
      wt_MVP = function(covmat) {
        lb = rep(min, n)  # num[1:5] 0.1 0.1 0.1 0.1 0.1 (MIN)
        ub = rep(max, n)  # num[1:5] 0.3 0.3 0.3 0.3 0.3 (MAX)
        dvec = c(rep(0,n))
        Amat_mv = cbind(rep(1, n), diag(n), -diag(n))
        bvec_mv = c(1, lb, -ub)
        meq = 1
        w_mv = solve.QP(covmat,dvec,Amat_mv,bvec_mv,meq)$solution
        
        return(w_mv)
      }
      # Most Diversified Portfolio Fuction (최대 분산효과 포트폴리오)
      wt_MDP = function(covmat) {
        Alb = -rep(min, n) %*% matrix(1, 1, n) + diag(n) # Min
        Aub = rep(max, n) %*% matrix(1, 1, n) - diag(n)  # Max
        
        dvec = c(rep(0,n))
        Amat_mv = cbind(sqrt(diag(covmat)), Alb, Aub)
        bvec_mv = c(1, rep(0, n), rep(0, n))
        meq =1
        w_mv = solve.QP(covmat,dvec,Amat_mv,bvec_mv,meq)$solution
        w_mv = (w_mv / sum(w_mv))
        
        return(w_mv)
      }
      # Risk Parity Portfolio Function (위험 균형 포트폴리오)
      wt_RPP = function(covmat) {
        library(cccp)
        opt = rp(x0 = rep((1/n), n), # 동일비중
                 P = covmat,
                 mrc = rep((1/n), n)) #동일 위험 비중
        w = getx(opt) %>% drop()
        w_mv = (w / sum(w))
        
        return(w_mv)
      }
      
      
      # Get Weight
      lookback = 12 # Momentum Period - Former 12 Months
      fee = 0.003
      WTS_COM = list()
      ep = endpoints(RETS$lst, on = "months") # Rebalancing Frequency
      wts = list()
      wt_zero = rep(0, ncol(RETS$lst)) %>% setNames(colnames(RETS$lst))
      
      for(i in (lookback+1) : length(ep)) {
        
        sub_ret = RETS$lst[ep[i-lookback] : ep[i] , ] # 과거  몇개월의 수익률 데이터 
        wt = wt_zero
        covs = cov(sub_ret) 
        
        # Portfoloio selection
        K = TRUE
        temp <- NULL
        
        if (input$radioBtn1 == "radioBtn1_sel1") {#동일비중 포트폴리오
          # 동일비중에서 선택된 numeric 값
          temp  = c(1/EWF$len)
        }
        else if (input$radioBtn1 == "radioBtn1_sel2") {#최소분산 포트폴리오
          temp = wt_MVP (covs) 
        }
        else if (input$radioBtn1 == "radioBtn1_sel3") {#최대분산효과 포트폴리오
          temp = wt_RPP (covs) 
        }
        else if (input$radioBtn1 == "radioBtn1_sel4") {#위험균형 포트폴리오
          temp = wt_MDP (covs)
        }
        else if (input$radioBtn1 == "radioBtn1_sel5") {#모멘텀 포트폴리오
          # 모멘텀에서 선택된 numeric 값
          # MOMENTUM_RATE$value = input[[paste0('numeric_Momentum')]]
          # print(MOMENTUM_RATE$value)
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= n  #  모집단중 가장 높은 집단 x 개를 추출 ( 추출 = TRUE , 제외 = FALSSE)
          temp = c(1/n)
        }
        
        wt[K] =  temp   
        names(wt) = colnames(RETS$lst)
        wts[[i]] = xts(t(wt), order.by = index(RETS$lst[ep[i]]))
      }
      WTS_COM = do.call(rbind, wts)
      
      
      #Portfolio selection
      ######################################################################################################
      # Portfolio Return
      port_gross <- NULL
      if (input$radioBtn1 == "radioBtn1_sel1") {#동일비중 포트폴리오
        for (i in 1:EWF$len) {
          EQUAL_WEIGHT_RATE$lst <- append(input[[paste0('numeric_rate_', i)]], EQUAL_WEIGHT_RATE$lst)
          EQUAL_WEIGHT_RATE$lbl <- paste0(EQUAL_WEIGHT_RATE$lbl, input[[paste0('numeric_rate_', i)]])
          if (i != EWF$len)
            EQUAL_WEIGHT_RATE$lbl <- paste0(EQUAL_WEIGHT_RATE$lbl, ":")
        }
        port_gross = Return.portfolio(RETS$lst, EQUAL_WEIGHT_RATE$lst, verbose = TRUE)
        WTS_COM = port_gross$BOP.Weight
      }
      else {
        port_gross = Return.portfolio(RETS$lst, WTS_COM, verbose = TRUE)
      }
      
      port_turnover = xts(rowSums(abs(port_gross$BOP.Weight - lag(port_gross$EOP.Weight)), na.rm = TRUE), order.by = index(port_gross$BOP.Weight))
      port_net = port_gross$returns - (port_turnover * fee)
      names(port_net) = 'Returns'
      port_net_yr = round(apply.yearly(port_net, Return.cumulative), 5)
      port_net_yr = data.frame(port_net_yr) %>% cbind('Year' = rownames(.), .)
      port_net_yr$Year = substring(port_net_yr$Year, 1, 4)
      rownames(port_net_yr) = NULL
      
      
      # Portfolio2 Return(Month)
      port_net_yr2 = round(apply.monthly(port_net, Return.cumulative), 5)
      port_net_yr2 = data.frame(port_net_yr2) %>% cbind('month' = rownames(.), .)
      port_net_yr2$month = substring(port_net_yr2$month, 1, 30)
      rownames(port_net_yr2) = NULL
      
      # Portfolio Return Graph
      output$port_ret = renderPlotly({
        plot_ly(data = data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1) %>%
                  cbind('Date' = rownames(.), .), x = ~ Date, y = ~ Returns, type = 'scatter', mode = 'lines') %>%
          layout(title = 'Portfolio Cumulative Return',
                 xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
                 yaxis = list(title = ""))
      })
      # output$port_ret = renderPlotly({
      #   df <- data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1)
      #   p<- plot_ly(data = 
      #                 data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1) %>%
      #                 cbind('Date' = rownames(.), .), type = 'scatter', mode = 'lines'
      #               ) %>%
      # 
      #     layout(title = 'Portfolio Cumulative Return',
      #            xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
      #            yaxis = list(title = ""))
      #   
      #   p <- add_lines(p, x=~Date, y=df[,1], name=EQUAL_WEIGHT_RATE$lbl)
      #   p
      # })
      
      #  Performance analysis
      Performance_analysis = table.AnnualizedReturns(port_net)# 연평균,표준편차,샤프비율
      PA = rbind(Performance_analysis, maxDrawdown(port_net))# mdd
      PA= rbind(PA,Return.cumulative(port_net))# 누적 수익률
      PA =rbind(PA,UpsideFrequency(port_net, MAR = 0))# 승률
      row.names(PA) = c('Annualized Return','Annualized Std Dev','Annualized Sharpe (Rf=0%)','max Drawdown','Cumulative Return','UpsideFrequency')#c('연평균수익률','연평균위험','연평균샤프비율','최대낙폭','총수익률','승률')
      Performance_analysis = t(PA)
      
      # Performance analysis
      output$Performance_analysis = renderTable({round(Performance_analysis,4) %>% 
          cbind(' ' = rownames(.), .) %>% 
          `rownames<-` (NULL)})# 소수 문제 !!!!!!!!!!!!!!!!
      
      # Portfolio Yearly Graph
      output$port_ret_yr = renderPlotly({
        plot_ly(data = port_net_yr, x = ~ Year, y = ~ Returns, type = "bar") %>%
          layout(title = 'Yearly Return', 
                 xaxis = list(title = ""), 
                 yaxis = list(title = "", tickformat = '%'))
      })
      
      # Portfolio monthly Graph
      output$port_ret_yr2 = renderPlotly({
        plot_ly(data = port_net_yr2,
                x = ~ month, y = ~ Returns, type = "bar") %>%
          layout(title = 'monthly Return', 
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
      
      #export excel(month)
      output$download_monthly <- downloadHandler(
        filename = function() {
          paste("Monthly Return", ".csv", sep="")
        },
        content = function(file) {
          write.csv(data.frame(round(port_net[paste0(input$`range`[1],"::",input$`rang`[2])], 5)) %>%
                      cbind('Date' = rownames(.), .), file)
        }
      )
      #export excel(year)
      output$download_yearly <- downloadHandler(
        filename = function() {
          paste("Yearly Return", ".csv", sep="")
        },
        content = function(file) {
          write.csv(port_net_yr, file)
        }
      )
      
      
      #Weight selection
      ######################################################################################################
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
        
        for (i in 1:EWF$len) {
          p <- add_trace(p, x=~Date, y = df[,i], name = EWF$lst[i])
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
      # Raw Return Chart
      ######################################################################################################
      output$plot_etf_raw <- renderPlotly({
        
        df <- data.frame(cumprod(1 + RETS$lst[paste0(input$`range`[1], "::", input$`range`[2])])-1)
        p <-  plot_ly(data = 
                        data.frame(cumprod(1 + RETS$lst[paste0(input$`range`[1], "::", input$`range`[2])])-1) %>%
                        cbind('Date' = rownames(.), .), mode = 'line', type = 'scatter'
        ) %>%
          
          layout(title = 'ETF Raw Return',
                 xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
                 yaxis = list(title = "", tickformat = '%'))
        
        for (i in 1:EWF$len) {
          p <- add_trace(p, x=~Date, y = df[,i], name = EWF$lst[i])
        }
        p
      })
      
      # Raw Return Data Table
      output$dataTable_etf_raw = DT::renderDataTable({
        data.frame(round(RETS$lst,4)) %>%
          cbind('Date' = rownames(.), .) %>%
          `rownames<-` (NULL)
      })
      
      
    }
    
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
  
  #deployApp()
})

