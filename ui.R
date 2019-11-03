library(shiny)
library(shinyjs)
library(markdown)
library(shinyWidgets)
library(quadprog)
library(DT)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)
library(d3heatmap)
library(rsconnect)

rsconnect::setAccountInfo(name='gtaa2', 
                          token='BE94CCF00A5999C76B82EDEC9E2AF39E', 
                          secret='MbNOpL0QVgvRljeQYvaZxtMOM3oqb3bkstEEuSRa')


mychoice <- c("SPY","IEV","EWJ","EEM","TLT","IEF","IYR","RWX","GLD","DBC")

shinyUI(navbarPage("Global Tactical Asset Allocation",
                   theme = shinythemes::shinytheme("united"),
                   #
                   # Main Page: Portfolio Return
                   tabPanel("Portfolio",
                            br(),
                            tabsetPanel(id = "inTabset", type = "tabs",
                                        tabPanel("Search", value="tab1", 
                                                 br(),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     dateRangeInput('range', '날짜 범위',
                                                                    start = '2008-01-01',
                                                                    end = Sys.Date(),
                                                                    min = '2008-01-01',
                                                                    max = Sys.Date(),
                                                                    format = "yyyy-mm-dd",
                                                                    separator = " - "),
                                                     pickerInput(inputId = "pickerInput1", '자산 선택',
                                                                 choices = mychoice, 
                                                                 options = list(`actions-box` = TRUE,  size = 12, 
                                                                                `selected-text-format` = "count >= 10"), 
                                                                 multiple = TRUE),
                                                     radioButtons("radioBtn1", 
                                                                  "전략 선택", 
                                                                  c("동일비중 포트폴리오 (Equal Weight Portfolio)" = "radioBtn1_sel1",
                                                                    "최소분산 포트폴리오(Minimum Variance Portfolio)" = "radioBtn1_sel2",
                                                                    "최대분산효과 포트폴리오(Maximum Diversifed Portfolio)" = "radioBtn1_sel3",
                                                                    "위험균형 포트폴리오(Equal Risk Contribution Portfolio)" = "radioBtn1_sel4"
                                                                    #,"모멘텀 포트폴리오(Momentum Portfolio)" = "radioBtn1_sel5"
                                                                    ),
                                                                  selected = "radioBtn1_sel4"),
                                                     lapply(1:length(mychoice), function(i) {
                                                       column(5,
                                                              numericInput(inputId = paste0("numeric_rate_", i),
                                                                           label = paste0("rate", i),
                                                                           min = 0.1, max = 0.9, step = 0.1, value = 0.1, width='150px')
                                                       )
                                                     }),
                                                     fluidPage(
                                                       useShinyjs(),
                                                       uiOutput(outputId = "out2"),
                                                       numericInput(inputId = "numeric_Momentum",
                                                                    label = "numeric_Momentum",
                                                                    min = 1, max = 5, step = 1, value = 1, width='100px')
                                                     ),
                                                     actionButton("goButton", "조회")
                                                   ),
                                                   mainPanel(tabPanel("correlation", "상관관계(correlation)", d3heatmapOutput("heatmap", width = "100%", height="500px")))
                                                 )
                                        ),
                                        tabPanel("Cumulative Return", value="tab2", 
                                                 br(),
                                                 plotlyOutput("port_ret"),
                                                 br(),
                                                 fluidRow(
                                                   column(12, tableOutput( "Performance_analysis"))
                                                 ),
                                                 br(),
                                                 plotlyOutput("port_ret_yr"),
                                                 plotlyOutput("port_ret_yr2"),
                                                 br(),
                                                 fluidRow(
                                                   column(6, DT::dataTableOutput("port_table")),
                                                   column(6, DT::dataTableOutput("port_table_year"))
                                                 ),
                                                 fluidRow(
                                                   column(1, offset = 10,downloadButton("download_monthly", "download(Monthly)")
                                                   )),
                                                 fluidRow(
                                                   column(1, offset = 10,downloadButton("download_yearly", "download(Yearly)")
                                                   ))),
                                        tabPanel("Weight", value="tab3",
                                                 br(),
                                                 plotlyOutput("wts_now"),
                                                 br(),
                                                 plotlyOutput("wts_hist"),
                                                 br(),
                                                 DT::dataTableOutput("wts_table")),
                                        tabPanel("Raw Data", value="tab4",
                                                 br(),
                                                 plotlyOutput("plot_etf_raw"),
                                                 br(),
                                                 DT::dataTableOutput("dataTable_etf_raw"),
                                                 br(),
                                                 br())
                            )
                   ),
                   # Description for strategy
                   tabPanel("Description",
                            tabsetPanel(
                              tabPanel("Strategy",
                                       br(),
                                       strong("Global Dynamic Asset Allocation"),
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
                              tags$li("E-mail : minclasse@gmail.com"),
                              tags$li("github : minclasse"),
                              tags$li("major : Computer Science "),
                              br()
                              
                            ),
                            div(),
                            strong("최영규"),
                            tags$ul(
                              tags$li("Phone nubmer : 010-2019-0700"),
                              tags$li("E-mail : dudrb1418@naver.com"),
                              tags$li("github : dudrb1418"),
                              tags$li("major : Computer Science  "),
                              br()
                              
                            )
                   )
                   
))
