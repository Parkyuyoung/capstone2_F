library(shiny)
library(shinyjs)
library(shinyWidgets)
library(markdown)
library(quadprog)
library(DT)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)
library(d3heatmap)
#install.packages("RMySQL")
library(RMySQL)
library(rsconnect)
rsconnect::setAccountInfo(name='gtaa2',
                          token='BE94CCF00A5999C76B82EDEC9E2AF39E',
                          secret='MbNOpL0QVgvRljeQYvaZxtMOM3oqb3bkstEEuSRa')

# DB 접속 설정
con <- dbConnect(MySQL(),
                 host="dudrb1418.codns.com",
                 dbname="GTAA",
                 user="GTAA", password="gtaa")

# 공통변수
EWF <- reactiveValues(lst = c(), len = 0)
RETS <- reactiveValues(lst = list())
EQUAL_WEIGHT_RATE <- reactiveValues(lst = c(), lbl = "")
MOMENTUM_RATE <- reactiveValues(value = 0)
MULTIFAC_RATE <- reactiveValues(value = 0)
MIN_RATE <- reactiveValues(value = 0.0)
MAX_RATE <- reactiveValues(value = 0.0)
RADIOBTN_SELECTED <- reactiveValues(id = "", boolean = FALSE)
#RADIOBTN_SELECTED2 <- reactiveValues(id = "", boolean = FALSE)

# 함수처리
func_tabSwitch <- function(session, id) {
  
  if (id == "show") {
    showTab(inputId = "inTabset", target = "tab2")
    showTab(inputId = "inTabset", target = "tab3")
    showTab(inputId = "inTabset", target = "tab4")
    updateTabsetPanel(session, "inTabset", selected = "tab2")
  }
  else if (id == "hide") {
    hideTab(inputId = "inTabset", target = "tab2")
    hideTab(inputId = "inTabset", target = "tab3")
    hideTab(inputId = "inTabset", target = "tab4")
  }
}
func_controlSwitch <- function(id) {# 컨트롤 활성/비활성 처리하는 함수
  
  if (id == "enable") {
    # lookback, rebalancing, fee 활성화
    enable("numeric_lookback")
    enable("numeric_rebalancing")
    enable("numeric_fee")
    enable("sliderInput_lookback")
    enable("sliderInput_rebalancing")
    enable("sliderInput_fee")
    # 미리보기버튼 활성화
    enable("btn_preview")
    # 전략버튼 활성화
    enable("radioBtn1")
    enable("radioBtn2")
    enable("radioBtn3")
    enable("radioBtn4")
    enable("radioBtn5")
    # 최대,최소 활성화
    enable("numeric_min")
    enable("numeric_max")
    # 조회버튼 활성화
    enable("goButton")
  }
  else if (id == "disable") {
    # numeric 숨김
    for (i in 1:10) {
      hide(paste0("numeric_rate_",i))
    }
    hide("numeric_momentum")#모멘텀기반 자산배분전략
    hide("numeric_multifac")#멀티팩터 자산배분전략
    hide("numeric_min")
    hide("numeric_max")
    
    # lookback, rebalancing, fee 비활성화
    disable("sliderInput_lookback")
    disable("sliderInput_rebalancing")
    disable("sliderInput_fee")
    # 미리보기버튼 비활성화
    disable("btn_preview")
    # 전략버튼 비활성화
    disable("radioBtn1")
    disable("radioBtn2")
    disable("radioBtn3")
    disable("radioBtn4")
    disable("radioBtn5")
    # 조회버튼 비활성화
    disable("goButton")
  }
}
func_Title <- function(id) {
  
  title <- ""
  if (id == "radioBtn1")        title <- "전통적 자산배분전략"
  else if (id == "radioBtn2")   title <- "위험 기반 자산배분전략"
  else if (id == "radioBtn3")   title <- "모멘텀 기반 자산배분전략"
  else if (id == "radioBtn4")   title <- "멀티 팩터 자산배분전략"
  return(title)
}
func_TitleList <- function(id) {
  
  list <- c()
  if (id == "radioBtn1") {
    list <- c("동일비중 포트폴리오 (Equal Weight Portfolio)" = "radioBtn1_sel1")
  }
  else if (id == "radioBtn2") {
    list <- c("최소분산 포트폴리오(Minimum Variance Portfolio)" = "radioBtn2_sel1",
              "최대분산효과 포트폴리오(Maximum Diversifed Portfolio)" = "radioBtn2_sel2",
              "위험균형 포트폴리오(Equal Risk Contribution Portfolio)" = "radioBtn2_sel3")
  }
  else if (id == "radioBtn3") {
    list <- c("모멘텀 포트폴리오(Momentum Portfolio)" = "radioBtn3_sel1")
  }
  else if (id == "radioBtn4") {
    list <- c("모멘텀x최소분산 포트폴리오" = "radioBtn4_sel1",
              "모멘텀x최대분산효과 포트폴리오" = "radioBtn4_sel2",
              "모멘텀x위험균형 포트폴리오" = "radioBtn4_sel3")
  }
  return(list)
}
func_TacticalRadioButton <- function(session, input, id, id2, EWF) {# (전통적,위험기반,모멘텀기반,멀티팩터) radiobuttons 처리
  
  # print(paste0("[clicked]id : ", id))
  # print(paste0("[clicked]id2 : ", id2))
  RADIOBTN_SELECTED$id = id2
  
  # numeric 숨김
  for (i in 1:10) {
    hide(paste0("numeric_rate_",i))
  }
  hide("numeric_momentum")
  hide("numeric_multifac")
  hide("numeric_min")
  hide("numeric_max")
  
  if (id == "radioBtn1") {
    updateRadioButtons(session, 
                       "radioBtn1", 
                       func_Title("radioBtn1"),
                       c(func_TitleList("radioBtn1")),
                       selected = id2)
    updateRadioButtons(session, 
                       "radioBtn2", 
                       func_Title("radioBtn2"),
                       c(func_TitleList("radioBtn2")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn3", 
                       func_Title("radioBtn3"),
                       c(func_TitleList("radioBtn3")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn4", 
                       func_Title("radioBtn4"),
                       c(func_TitleList("radioBtn3")),
                       selected = 0)
    for (i in 1:EWF$len) {
      toggle(paste0("numeric_rate_",i))
    }
  }
  else if (id == "radioBtn2") {
    updateRadioButtons(session, 
                       "radioBtn1", "전통적 자산배분전략", 
                       c(func_TitleList("radioBtn1")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn2", "위험 기반 자산배분전략", 
                       c(func_TitleList("radioBtn2")),
                       selected = id2)
    updateRadioButtons(session, 
                       "radioBtn3", "모멘텀 기반 자산배분전략", 
                       c(func_TitleList("radioBtn3")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn4", "멀티 팩터 자산배분전략", 
                       c(func_TitleList("radioBtn4")),
                       selected = 0)
    if (RADIOBTN_SELECTED$id == "radioBtn2_sel1" || RADIOBTN_SELECTED$id == "radioBtn2_sel2") {
      # 최소분산,최대분산효과 선택했을때
      updateNumericInput(session,
                         "numeric_min",
                         label = "numeric_min",
                         min = 0.00, max = 0.9, step = 0.01, value = 0)
      updateNumericInput(session,
                         "numeric_max",
                         label = "numeric_max",
                         min = 0.00, max = 1, step = 0.01, value = 1)
      toggle("numeric_min")
      toggle("numeric_max")
    }
  }
  
  else if (id == "radioBtn3") {
    updateRadioButtons(session, 
                       "radioBtn1", "전통적 자산배분전략", 
                       c(func_TitleList("radioBtn1")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn2", "위험 기반 자산배분전략", 
                       c(func_TitleList("radioBtn2")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn3", "모멘텀 기반 자산배분전략", 
                       c(func_TitleList("radioBtn3")),
                       selected = id2)
    updateRadioButtons(session, 
                       "radioBtn4", "멀티 팩터 자산배분전략", 
                       c(func_TitleList("radioBtn4")),
                       selected = 0)
    toggle("numeric_momentum")
  }
  else if (id == "radioBtn4") {
    updateRadioButtons(session, 
                       "radioBtn1", "전통적 자산배분전략", 
                       c(func_TitleList("radioBtn1")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn2", "위험 기반 자산배분전략", 
                       c(func_TitleList("radioBtn2")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn3", "모멘텀 기반 자산배분전략", 
                       c(func_TitleList("radioBtn3")),
                       selected = 0)
    updateRadioButtons(session, 
                       "radioBtn4", 
                       "멀티 팩터 자산배분전략", 
                       c(func_TitleList("radioBtn4")),
                       selected = id2)
    if (RADIOBTN_SELECTED$id == "radioBtn4_sel1" || RADIOBTN_SELECTED$id == "radioBtn4_sel2") {
      # 최소분산,최대분산효과 선택했을때
      updateNumericInput(session,
                         "numeric_min",
                         label = "numeric_min",
                         min = 0.00, max = 0.9, step = 0.01, value = 0)
      updateNumericInput(session,
                         "numeric_max",
                         label = "numeric_max",
                         min = 0.00, max = 1, step = 0.01, value = 1)
      toggle("numeric_multifac")
      toggle("numeric_min")
      toggle("numeric_max")
    }
  }
}
func_Tactical <- function(tactical, n, min, max, covmat) {#전략 알고리즘 처리 함수
  
  temp <- NULL
  if (tactical == "EWP") {# Equal Weight Portfolio(동일비중 포트폴리오)
    temp = c(1/EWF$len)
  }
  else if (tactical == "MVP") {# Min Variance Portfolio Function (최소 분산 포트폴리오)
    lb = rep(min, n)  # num[1:5] 0.1 0.1 0.1 0.1 0.1 (MIN)
    ub = rep(max, n)  # num[1:5] 0.3 0.3 0.3 0.3 0.3 (MAX)
    dvec = c(rep(0, n))
    Amat_mv = cbind(rep(1, n), diag(n), -diag(n))
    bvec_mv = c(1, lb, -ub)
    meq = 1
    temp = solve.QP(covmat, dvec, Amat_mv, bvec_mv, meq)$solution
  }
  else if (tactical == "MDP") {
    # Most Diversified Portfolio Fuction (최대 분산효과 포트폴리오)
    Alb = -rep(min, n) %*% matrix(1, 1, n) + diag(n) # Min
    Aub = rep(max, n) %*% matrix(1, 1, n) - diag(n)  # Max
    dvec = c(rep(0,n))
    Amat_mv = cbind(sqrt(diag(covmat)), Alb, Aub)
    bvec_mv = c(1, rep(0, n), rep(0, n))
    meq = 1
    w_mv = solve.QP(covmat,dvec,Amat_mv,bvec_mv,meq)$solution
    temp = (w_mv / sum(w_mv))
  }
  else if (tactical == "RPP") {# Risk Parity Portfolio Function (위험 균형 포트폴리오)
    library(cccp)
    opt = rp(x0 = rep((1/n), n), # 동일비중
             P = covmat,
             mrc = rep((1/n), n)) #동일 위험 비중
    w = getx(opt) %>% drop()
    temp = (w / sum(w))
  }
  return(temp)
}


