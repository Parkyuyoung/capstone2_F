source("common.R")


### server
shinyServer(function(input, output, session) {
  
  # (Cumulative Return, Weight, Raw Data) Tab 탭 숨김
  func_tabSwitch(session, "hide")
  # 모든 컨트롤 "비활성"상태로 변경
  func_controlSwitch("disable")
  
  
  # 자산선택 이벤트(테스트)
  observeEvent(input$test_btn_ewf,{
    # 테스트용 코드
    #EWF$lst <- c('AAPL', 'BABA', 'MSFT', 'AMZN', 'TLT')
    EWF$lst <- c('SPY', 'IEV', 'EWJ')
    #EWF$lst <- c('SPY', 'IEV', 'EWJ', 'EEM')
    #EWF$lst <- c('SPY', 'IEV', 'EWJ', 'EEM', 'TLT')
    #EWF$lst <- c('SPY', 'IEV', 'EWJ', 'EEM', 'TLT', 'IEF', 'IYR', 'RWX', 'GLD', 'DBC')
    EWF$len <- length(EWF$lst)
    output$nText <- renderText(EWF$lst)
    updateTextInput(session, "ticker", value = "")
    
    # 모든 컨트롤 "활성"상태로 변경
    func_controlSwitch("enable")
    
    #
    getSymbols(EWF$lst, src='yahoo', from = '1900-01-01')
    # Bind Price Data
    prices = do.call(merge, lapply(EWF$lst, function(x) Ad(get(x))))
    rets = na.omit(Return.calculate(prices))
    names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
    RETS$lst = rets
    
    
    # correlation
    output$heatmap <- renderD3heatmap({d3heatmap(cor(RETS$lst))})
    
  })
  
  # 자산삭제 이벤트
  observeEvent(input$btn_ewf_delete,{
    EWF$lst <- c()
    EWF$len <- length(EWF$lst)
    print(paste0("EWF$len : ", EWF$len))
    
    # UI에서 없애줌(삭제처리)
    output$nText <- renderText(EWF$lst)
    updateTextInput(session, "ticker", value = "")
    
    # correlation 상관관계 삭제
    output$heatmap <- NULL
    
    # (Cumulative Return, Weight, Raw Data) Tab 탭 숨김
    func_tabSwitch(session, "hide")
    # 모든 컨트롤 "비활성"상태로 변경
    func_controlSwitch("disable")
  })
  
  # 자산선택 이벤트
  observeEvent(input$btn_ewf,{
    
    query <- paste0("SELECT count(*) FROM `GTAA_EWF` WHERE code = '", input$ticker, "'")
    rows <- dbGetQuery(con, query)
    
    if (rows == 0) {
      sendSweetAlert(
        session = session,
        title = "",
        text = "해당하는 자산이 없습니다.",
        type = "error")
    }
    else {
      EWF$lst <- c(EWF$lst, toupper(input$ticker))
      EWF$len <- length(EWF$lst)
      output$nText <- renderText(EWF$lst)
      updateTextInput(session, "ticker", value = "")
      
      if (EWF$len >= 2) {
        # 모든 컨트롤 "활성"상태로 변경
        func_controlSwitch("enable")
        #
        getSymbols(EWF$lst, src='yahoo', from = '1900-01-01')
        # Bind Price Data
        prices = do.call(merge, lapply(EWF$lst, function(x) Ad(get(x))))
        rets = na.omit(Return.calculate(prices))
        names(rets) = unlist(strsplit(names(rets), ".Adjusted"))
        RETS$lst = rets
        # correlation
        output$heatmap <- renderD3heatmap({d3heatmap(cor(RETS$lst))})
      }
    }
    
  })
  
  # 전략 라디오버튼 이벤트
  observeEvent(input$radioBtn1,{#전통적 자산배분전략
    func_TacticalRadioButton(session, input, "radioBtn1", input$radioBtn1, EWF)
  })
  observeEvent(input$radioBtn2,{#위험 기반 자산배분전략
    func_TacticalRadioButton(session, input, "radioBtn2", input$radioBtn2, EWF)
  })
  observeEvent(input$radioBtn3,{#모멘텀 기반 자산배분전략
    func_TacticalRadioButton(session, input, "radioBtn3", input$radioBtn3, EWF)
  })
  observeEvent(input$radioBtn4,{#멀티 팩터 자산배분전략
    func_TacticalRadioButton(session, input, "radioBtn4", input$radioBtn4, EWF)
  })
  
  
  
  # 미리보기 버튼 이벤트
  observeEvent(input$btn_preview,{

    if (RADIOBTN_SELECTED$id == "") {
      sendSweetAlert(
        session = session,
        title = "",
        text = "전략을 선택해주세요.",
        type = "error")
    }
    else {
      #
      MOMENTUM_RATE$value = input[[paste0('numeric_momentum')]]
      MULTIFAC_RATE$value = input[[paste0('numeric_multifac')]]

      # Get weight
      n = EWF$len
      min = input[[paste0('numeric_min')]]
      max = input[[paste0('numeric_max')]]


      # Get Weight
      WTS_COM = list()
      ep = endpoints(RETS$lst, on = 'months') # Rebalancing Frequency
      ep = ep [2:length(ep)]
      rebalancing = subset(ep, index(ep)%% input$sliderInput_rebalancing == 0 )
      #rebalancing = subset(ep, index(ep)%% CNT_rebalancing == 0 )
      ep = c(0, rebalancing, ep[length(ep)])
      wts = list()
      wt_zero = rep(0, ncol(RETS$lst)) %>% setNames(colnames(RETS$lst))


      for(i in (input$sliderInput_lookback+1) : length(ep)) {
        sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , ] # 과거  몇개월의 수익률 데이터
        wt = wt_zero
        covs = cov(sub_ret)

        # Portfoloio selection
        K = TRUE
        temp <- NULL

        if (RADIOBTN_SELECTED$id == "radioBtn1_sel1") {
          #동일비중 포트폴리오
          temp = func_Tactical("EWP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn2_sel1") {
          #최소분산 포트폴리오
          temp = func_Tactical("MVP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn2_sel2") {
          #최대분산효과 포트폴리오
          temp = func_Tactical("MDP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn2_sel3") {
          #위험균형 포트폴리오
          temp = func_Tactical("RPP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn3_sel1") {
          #모멘텀 포트폴리오
          cum = Return.cumulative(sub_ret) # 과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MOMENTUM_RATE$value  # 모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          temp = c(1/EWF$len)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn4_sel1") {
          #멀티팩터x최소분산 포트폴리오
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MULTIFAC_RATE$value  #  모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          n = MULTIFAC_RATE$value
          sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , K ]  # 이게 문제임
          covs = cov(sub_ret)
          temp = func_Tactical("MVP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn4_sel2") {
          #멀티팩터x최대분산효과 포트폴리오
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MULTIFAC_RATE$value  #  모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          n = MULTIFAC_RATE$value
          sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , K ]  # 이게 문제임
          covs = cov(sub_ret)
          temp = func_Tactical("MDP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn4_sel3") {
          #멀티팩터x위험균형 포트폴리오
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MULTIFAC_RATE$value  #  모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          n = MULTIFAC_RATE$value
          sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , K ]  # 이게 문제임
          covs = cov(sub_ret)
          temp = func_Tactical("RPP", n, min, max, covs)
        }

        wt[K] =  temp
        names(wt) = colnames(RETS$lst)
        wts[[i]] = xts(t(wt), order.by = index(RETS$lst[ep[i]]))
      }
      WTS_COM = do.call(rbind, wts)


      # Portfolio Return
      port_gross <- NULL
      if (RADIOBTN_SELECTED$id == "radioBtn1_sel1") {#동일비중 포트폴리오 일때
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

      #
      port_turnover = xts(rowSums(abs(port_gross$BOP.Weight - lag(port_gross$EOP.Weight)), na.rm = TRUE), order.by = index(port_gross$BOP.Weight))
      port_net = port_gross$returns - (port_turnover * input$sliderInput_fee)
      names(port_net) = 'Returns'
      port_net_yr = round(apply.yearly(port_net, Return.cumulative), 5)
      port_net_yr = data.frame(port_net_yr) %>% cbind('Year' = rownames(.), .)
      port_net_yr$Year = substring(port_net_yr$Year, 1, 4)
      rownames(port_net_yr) = NULL

      #  Performance analysis
      Performance_analysis = table.AnnualizedReturns(port_net)# 연평균,표준편차,샤프비율
      PA = rbind(Performance_analysis, maxDrawdown(port_net))# mdd
      PA = rbind(PA, Return.cumulative(port_net))# 누적 수익률
      PA = rbind(PA, UpsideFrequency(port_net, MAR = 0))# 승률
      row.names(PA) = c('Annualized Return', 'Annualized Std Dev', 'Annualized Sharpe (Rf=0%)', 'max Drawdown', 'Cumulative Return', 'UpsideFrequency')#c('연평균수익률','연평균위험','연평균샤프비율','최대낙폭','총수익률','승률')
      Performance_analysis = t(PA)
      ###############################################################################################################
      str = ""
      for (i in 1:EWF$len) {
        if (i == EWF$len) str = paste0(str,EWF$lst[i])
        else              str = paste0(str,EWF$lst[i],",")
      }
      
      query <- paste0("SELECT
                      count(seqid) 
                      FROM GTAA_PREVIEW
                      where tactical = '", RADIOBTN_SELECTED$id,  "'", "and
                      ewf = '", str,  "'", "and
                      rebalancing = ", input$sliderInput_rebalancing
                      )
      rows <- dbGetQuery(con, query)
      #print(query)
      
      
      if (rows == 0) {
        # 미리보기 DB에 데이터 저장
        imsi  = (port_turnover * input$sliderInput_fee)
        for (i in 1:length(imsi)) {
          query <- paste0("insert into GTAA_PREVIEW(tactical, rebalancing, lookback, fee, data, Annualized_Return, ewf) values(",
                          "'", RADIOBTN_SELECTED$id, "',",
                          "'", input$sliderInput_rebalancing, "',",
                          "'", input$sliderInput_lookback, "',",
                          "'", input$sliderInput_fee, "',",
                          "'", imsi[i], "',",
                          "'", Performance_analysis[1], "',",
                          "'", str,"')"
          )
          dbGetQuery(con, query)
          print(query)
        }
        print("[미리보기]insert success!")
      }

      # # 리밸런싱 max값 가져옴
      # query <- paste0("SELECT 
      #                 max(rebalancing) 
      #                 FROM GTAA_PREVIEW 
      #                 where tactical = '", RADIOBTN_SELECTED$id,  "'" 
      #                 )
      # print(query)
      # max_previewdata <- dbGetQuery(con, query)
      # print(max_previewdata)
      
      # 미리보기 표 출력
      query <- paste0("SELECT
                      tactical, rebalancing, sum(data), Annualized_Return
                      FROM GTAA_PREVIEW
                      where tactical = '", RADIOBTN_SELECTED$id,  "'", "and
                      ewf = '", str,  "'", "and
                      rebalancing <= ", input$sliderInput_rebalancing, 
                      " group by rebalancing"
                      )
      rows <- dbGetQuery(con, query)
      print(query)

      # Chart
      output$plot_preview <- plotly::renderPlotly({
        df <- data.frame(rows)
        p <-  plot_ly(data = data.frame(rows), mode = 'line', type = 'scatter') %>%

          layout(title = '미리보기',
                 xaxis = list(title = ""),
                 yaxis = list(title = ""))

        p <- add_trace(p, x = ~rebalancing, y = ~df[,3], name = '세금')
        p <- add_trace(p, x = ~rebalancing, y = ~df[,4], name = '연평균수익률')
        p
      })
      print("[preview]print success!")
      ######################################################################################################################
    }
  })



  # 조회버튼 이벤트
  observeEvent(input$goButton,{

    if (RADIOBTN_SELECTED$id == "") {
      sendSweetAlert(
        session = session,
        title = "",
        text = "전략을 선택해주세요.",
        type = "error")
    }
    else {
      # (Cumulative Return, Weight, Raw Data Tab) 탭 보여짐
      func_tabSwitch(session, "show")

      #
      MOMENTUM_RATE$value = input[[paste0('numeric_momentum')]]
      MULTIFAC_RATE$value = input[[paste0('numeric_multifac')]]

      # Get weight
      n = EWF$len
      min = input[[paste0('numeric_min')]]
      max = input[[paste0('numeric_max')]]


      # Get Weight
      WTS_COM = list()
      ep = endpoints(RETS$lst, on = 'months') # Rebalancing Frequency
      ep = ep [2:length(ep)]
      rebalancing = subset(ep, index(ep)%% input$sliderInput_rebalancing == 0 )
      ep = c(0, rebalancing, ep[length(ep)])
      wts = list()
      wt_zero = rep(0, ncol(RETS$lst)) %>% setNames(colnames(RETS$lst))


      for(i in (input$sliderInput_lookback+1) : length(ep)) {

        sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , ] # 과거  몇개월의 수익률 데이터
        wt = wt_zero
        covs = cov(sub_ret)

        # Portfoloio selection
        K = TRUE
        temp <- NULL

        if (RADIOBTN_SELECTED$id == "radioBtn1_sel1") {
          #동일비중 포트폴리오
          temp = func_Tactical("EWP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn2_sel1") {
          #최소분산 포트폴리오
          temp = func_Tactical("MVP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn2_sel2") {
          #최대분산효과 포트폴리오
          temp = func_Tactical("MDP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn2_sel3") {
          #위험균형 포트폴리오
          temp = func_Tactical("RPP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn3_sel1") {
          #모멘텀 포트폴리오
          cum = Return.cumulative(sub_ret) # 과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MOMENTUM_RATE$value  # 모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          temp = c(1/EWF$len)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn4_sel1") {
          #멀티팩터x최소분산 포트폴리오
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MULTIFAC_RATE$value  #  모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          n = MULTIFAC_RATE$value
          sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , K ]  # 이게 문제임
          covs = cov(sub_ret)
          temp = func_Tactical("MVP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn4_sel2") {
          #멀티팩터x최대분산효과 포트폴리오
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MULTIFAC_RATE$value  #  모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          n = MULTIFAC_RATE$value
          sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , K ]  # 이게 문제임
          covs = cov(sub_ret)
          temp = func_Tactical("MDP", n, min, max, covs)
        }
        else if (RADIOBTN_SELECTED$id == "radioBtn4_sel3") {
          #멀티팩터x위험균형 포트폴리오
          cum = Return.cumulative(sub_ret) #  과거 몇 개월의 총 수익률 데이터
          K = rank(-cum) <= MULTIFAC_RATE$value  #  모집단중 가장 높은 집단 x 개를 추출(추출 = TRUE , 제외 = FALSSE)
          n = MULTIFAC_RATE$value
          sub_ret = RETS$lst[ep[i-input$sliderInput_lookback] : ep[i] , K ]  # 이게 문제임
          covs = cov(sub_ret)
          temp = func_Tactical("RPP", n, min, max, covs)
        }

        wt[K] =  temp
        names(wt) = colnames(RETS$lst)
        wts[[i]] = xts(t(wt), order.by = index(RETS$lst[ep[i]]))
      }
      WTS_COM = do.call(rbind, wts)


      # Portfolio Return
      port_gross <- NULL
      if (RADIOBTN_SELECTED$id == "radioBtn1_sel1") {#동일비중 포트폴리오 일때
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

      #
      port_turnover = xts(rowSums(abs(port_gross$BOP.Weight - lag(port_gross$EOP.Weight)), na.rm = TRUE),
                          order.by = index(port_gross$BOP.Weight))
      port_net = port_gross$returns - (port_turnover * input$sliderInput_fee)
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
      output$port_ret <- plotly::renderPlotly({
        plot_ly(data = data.frame(cumprod(1 + port_net[paste0(input$`range`[1],"::",input$`range`[2])])-1) %>%
                  cbind('Date' = rownames(.), .), x = ~ Date, y = ~ Returns, type = 'scatter', mode = 'lines') %>%
          layout(title = 'Portfolio Cumulative Return',
                 xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
                 yaxis = list(title = ""))
      })

      #  Performance analysis
      Performance_analysis = table.AnnualizedReturns(port_net)# 연평균,표준편차,샤프비율
      PA = rbind(Performance_analysis, maxDrawdown(port_net))# mdd
      PA = rbind(PA, Return.cumulative(port_net))# 누적 수익률
      PA = rbind(PA, UpsideFrequency(port_net, MAR = 0))# 승률
      row.names(PA) = c('Annualized Return', 'Annualized Std Dev', 'Annualized Sharpe (Rf=0%)', 'max Drawdown', 'Cumulative Return', 'UpsideFrequency')#c('연평균수익률','연평균위험','연평균샤프비율','최대낙폭','총수익률','승률')
      Performance_analysis = t(PA)
      # Performance analysis
      output$Performance_analysis = renderTable({round(Performance_analysis,4) %>%
          cbind(' ' = rownames(.), .) %>%
          `rownames<-` (NULL)})# 소수 문제 !!!!!!!!!!!!!!!!

      # Portfolio Yearly Graph
      output$port_ret_yr <- plotly::renderPlotly({
        plot_ly(data = port_net_yr, x = ~ Year, y = ~ Returns, type = "bar") %>%
          layout(title = 'Yearly Return',
                 xaxis = list(title = ""),
                 yaxis = list(title = "", tickformat = '%'))
      })

      # Portfolio monthly Graph
      output$port_ret_yr2 = plotly::renderPlotly({
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

      # Present Weight
      wts_now = t(coredata(round(last(WTS_COM)[, last(WTS_COM) != 0], 4)))
      wts_now = data.frame(ticker = rownames(wts_now), WTS_COM = wts_now)

      output$wts_now <- plotly::renderPlotly({
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
      output$wts_hist <- plotly::renderPlotly({
        df <- data.frame(WTS_COM[paste0(input$`range`[1],"::",input$`range`[2])])
        p <- plot_ly(data =
                       data.frame(WTS_COM[paste0(input$`range`[1],"::",input$`range`[2])]) %>%
                       cbind('Date' = rownames(.), .), mode = 'none', type = 'scatter', stackgroup = 'one'
        ) %>%

        layout(title = 'Historical Portfolio Weight',
               #xaxis = list(title = "", type = 'date', tickformat = '%y-%m'),
               yaxis = list(title = "", tickformat = '%'))

        for (i in 1:EWF$len) {
          p <- add_trace(p, x=~Date, y = df[,i], name = EWF$lst[i])
          #p <- add_trace(p, x=df[,0], y = df[,i], name = EWF$lst[i])
        }
        p
      })

      # Weight Table
      output$wts_table <- DT::renderDataTable({
        data.frame(round(WTS_COM[paste0(input$`range`[1],"::",input$`range`[2])], 4)) %>%
          cbind('Date' = rownames(.), .) %>%
          `rownames<-` (NULL)
      })

      # Raw Return Chart
      output$plot_etf_raw <- plotly::renderPlotly({
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
