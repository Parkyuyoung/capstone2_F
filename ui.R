#install.packages("plotly")
#install.packages("quantmod")
library(shiny)
library(markdown)
library(shinyWidgets)
library(rsconnect)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)

rsconnect::setAccountInfo(name='gtaa',
                          token='CFD37C0B14B5A9ABDE321E09BE5B1E05',
                          secret='MmVX3nR8+MAJIPxRBCQxn/F1Ka9pZmDHRj+84bGq')

# pkg = c('DT', 'quadprog', 'plotly', 'quantmod', 'PerformanceAnalytics')
# new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
# if (length(new.pkg)) 
#   install.packages(new.pkg, dependencies = TRUE)
# sapply(pkg, require, character.only = TRUE)



shinyUI(
  navbarPage(
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
    ),
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
    
    mainPanel(
    )
  ))
