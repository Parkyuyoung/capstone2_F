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

rsconnect::setAccountInfo(name='gdaa', token='0E0C63F55AF67711F880E9B6590DD064', secret='VIzSJM7G0xdwnYisNSFuGe3L31XpTy2N64noU6E3')

mychoices <- c("SPY","IEV","EWJ","EEM","TLT","IEF","IYR","RWX","GLD","DBC")
shinyUI(navbarPage("Global Tactical Asset Allocation",
                   theme = shinythemes::shinytheme("united"),
                   #
                   # Main Page: Portfolio Return
                   tabPanel("Portfolio",
                            br(),
                            tabsetPanel(id = "inTabset", type = "tabs",
                                        tabPanel("Search", value="tab1", 
                                                 br(),
                                                 sidebarPanel(
                                                   dateRangeInput('range4', '날짜 범위 선택',
                                                                  start = '2008-01-01',
                                                                  end = Sys.Date(),
                                                                  min = '2008-01-01',
                                                                  max = Sys.Date(),
                                                                  format = "yyyy-mm-dd",
                                                                  separator = " - "),
                                                   pickerInput(inputId = "pickerInput1", '자산 선택',
                                                               choices = mychoices, 
                                                               options = list(`actions-box` = TRUE,  size = 10, 
                                                                              `selected-text-format` = "count > 5"), 
                                                               multiple = TRUE),
                                                   radioButtons("inRadioButtons1", 
                                                                "전략 선택", 
                                                                c("동일비중"),
                                                                selected = "동일비중",
                                                                inline = TRUE),
                                                   fluidPage(
                                                     useShinyjs(),
                                                     uiOutput(outputId = "out")
                                                   ),
                                                   fluidPage(
                                                     useShinyjs(),
                                                     uiOutput(outputId = "out2")
                                                   ),
                                                   actionButton("goButton", "조회"))
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
                                                 br(),
                                                 fluidRow(
                                                   column(6, DT::dataTableOutput("port_table")),
                                                   column(6, DT::dataTableOutput("port_table_year"))
                                                 ))
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
                             tags$li("E-mail : yuyoung001@naver.com"),
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
                             tags$li("E-mail : dudrb1418@gmail.com"),
                             tags$li("github : dudrb1418"),
                             tags$li("major : Computer Science  "),
                             br()
                             
                           )
                  )
                   
))
