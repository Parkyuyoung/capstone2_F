
source("common.R")

jscode <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

### ui
shinyUI(navbarPage("Global Tactical Asset Allocation",
                   theme = shinythemes::shinytheme("united"),
                   
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
                                                     tags$head(tags$script(HTML(jscode))),
                                                     tagAppendAttributes(
                                                       #textInput("text", NULL, "foo"),
                                                       textInput("ticker", "자산 입력", "SPY"), 
                                                       `data-proxy-click` = "btn_ewf"
                                                     ),
                                                     actionButton("btn_ewf", "입력"),
                                                     actionButton("btn_ewf_delete", "삭제"),
                                                     #actionButton("test_btn_ewf", "테스트"),
                                                     p("  "),
                                                     verbatimTextOutput("nText"),
                                                     br(),
                                                     radioButtons("radioBtn1",  
                                                                  func_Title("radioBtn1"),
                                                                  func_TitleList("radioBtn1"),
                                                                  selected = 0),
                                                     radioButtons("radioBtn2", 
                                                                  func_Title("radioBtn2"),
                                                                  func_TitleList("radioBtn2"),
                                                                  selected = 0),
                                                     radioButtons("radioBtn3", 
                                                                  func_Title("radioBtn3"),
                                                                  func_TitleList("radioBtn3"),
                                                                  selected = 0),
                                                     radioButtons("radioBtn4", 
                                                                  func_Title("radioBtn4"),
                                                                  func_TitleList("radioBtn4"),
                                                                  selected = 0),
                                                     lapply(1:length(mychoice), function(i) {
                                                       column(5,
                                                              numericInput(inputId = paste0("numeric_rate_", i),
                                                                           label = paste0("rate", i),
                                                                           min = 0.1, max = 0.9, step = 0.1, value = 0.1, width='150px')
                                                       )
                                                     }),
                                                     fluidPage(
                                                       useShinyjs(),
                                                       numericInput(inputId = "numeric_momentum", label = "numeric_momentum",
                                                                    min = 1, max = 10, step = 1, value = 1, width='100px')
                                                     ),
                                                     fluidPage(
                                                       useShinyjs(),
                                                       numericInput(inputId = "numeric_multifac", label = "numeric_multifac",
                                                                    min = 1, max = 10, step = 1, value = 3, width='100px')
                                                     ),
                                                     fluidPage(
                                                       useShinyjs(),
                                                       numericInput(inputId = "numeric_min", label = "numeric_min",
                                                                    min = 0.00, max = 0.9, step = 0.1, value = 0)
                                                     ),
                                                     fluidPage(
                                                       useShinyjs(),
                                                       numericInput(inputId = "numeric_max", label = "numeric_max",
                                                                    min = 0.00, max = 1, step = 1, value = 1)
                                                     ),
                                                     p("  "),
                                                     sliderInput('sliderInput_lookback', '룩백', 
                                                                 min = 1, max = 100, step = 1, value = 12),
                                                     sliderInput('sliderInput_rebalancing', '리밸런싱', 
                                                                 min = 1, max = 48, step = 1, value = 3),
                                                     sliderInput('sliderInput_fee', '매매비용', 
                                                                 min = 0.001, max = 0.01, step = 0.001, value = 0.003),
                                                     #actionButton("btn_preview", "미리보기", placement="right"),
                                                     p("  "),
                                                     br(),
                                                     actionButton("goButton", "조회")
                                                   ),
                                                   mainPanel(tabPanel("correlation", "상관관계(correlation)", 
                                                                      d3heatmapOutput("heatmap", width = "100%", height="500px")),
                                                             br(),br(),br(),br(),br(),br(),br(),
                                                             tabPanel("preview", "", 
                                                                      plotlyOutput("plot_preview"),
                                                                      br(),
                                                                      DT::dataTableOutput("dataTable_preview"))),
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
                                                   column(1, offset = 10, downloadButton("download_monthly", "download(Monthly)")
                                                   )),
                                                 fluidRow(
                                                   column(1, offset = 10, downloadButton("download_yearly", "download(Yearly)")
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
