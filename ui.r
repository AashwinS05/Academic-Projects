#packages required to be loaded
packages = c("shiny","shinythemes","rvest","stringr", "tidytext",
              "sqldf", "ggplot2", "ggthemes", "dplyr","quanteda","tm",
              "readr", "magrittr", "data.table","xgboost","caret","e1071")

#Using function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded

package.check = lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

ui <- navbarPage("AashwinSinghal_Natural_Language_Analysis",
  inverse = TRUE, theme = shinytheme("readable"),
  
  tabPanel("Load Data",
           fluid = TRUE,
           titlePanel("Download and view the reviews from 
                      https://www.cars.com/research/toyota-camry/ "),
           sidebarLayout(
             shiny::sidebarPanel(
               actionButton("action_1",
                            "Go render Table"),
               numericInput("numeric_1",
                            "Choose Rows to Display:",3)),
             mainPanel(
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Reviews Train Data", tableOutput("view_1")),
                 tabPanel("Reviews Test Data", tableOutput("view_2"))
               )
               )
             )
           ),
  tabPanel("Data Preprocessing",
           fluid = TRUE,
           titlePanel(h4("Here I have preprocessed the data in two parts:
                         Part 1. Reviews have been NORMALIZED to remove 'punctions' & 'Lower-cased'
                         Part 2. reviews have been TAGGED according to the presence of 
                            the following words :'service', 'price', 'handling', 'interior' ")),
               tabsetPanel(type = "tabs",
                           tabPanel("Normalized Reviews", fluid = TRUE,
                                    sidebarLayout(
                                      shiny::sidebarPanel(
                                        numericInput("numeric_2",
                                                     "Choose Rows to Display:",3)),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("train", tableOutput("view_3")),
                                                    tabPanel("Test", tableOutput("view_4")))))),
                           tabPanel("Tagged Reviews", fluid = TRUE,
                                    sidebarLayout(
                                      shiny::sidebarPanel(
                                        numericInput("numeric_2",
                                                     "Choose Rows to Display:",3)),
                                      mainPanel(
                                        tabsetPanel(type = "tabs",
                                                    tabPanel("train", tableOutput("view_5")),
                                                    tabPanel("Test", tableOutput("view_6"))
                                                    )
                                        )
                                      )
                                    )
                           )
           ),
  
  tabPanel("Sentiment Analysis - Train Data",
           fluid = TRUE,
           titlePanel(h4(" ")),
           tabsetPanel(type = "tabs",
                       tabPanel("Sentiment Table With AFINN Scores",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Table",
                                                     sidebarLayout(
                                                       shiny::sidebarPanel(
                                                         numericInput("numeric_3",
                                                                      "Choose Rows to Display:",3)),
                                                       mainPanel(shiny::tableOutput("view_7")
                                                                 )
                                                       )
                                                     ),
                                            tabPanel("Vizualization",
                                                     titlePanel(h4("Although we do see that there are outliers as some 5-star reviews 
                                                                    have a highly negative sentiment score, 
                                                                    but still it's a good start! as our sentiment scores are correlated 
                                                                    with user ratings.")) ,
                                                     plotOutput("plot_1")
                                                     )
                                            )
                                ),
                       tabPanel("Analysis - Train Data",
                                tabsetPanel(type = "tabs",
                                            tabPanel("Avg Sentiment Rating VS Avg Star Rating",
                                                     titlePanel(h4("Here it can be see that the average Sentiment rating (Training Data) is pretty close 
                                                                   to the average Star Rating given by the users - this goes to show that the 
                                                                   sentiment analysis has been effective !!! ")),
                                                     tableOutput("view_8")),
                                            tabPanel("Avg Sentiment Rating/Tag vs Avg Star Ratings/Tag Vs Avg Star Rating(train data)", 
                                                     titlePanel(h4(" ")),
                                                     tableOutput("view_9")
                                                     )
                                            )
                                )
                       )
           ),
  tabPanel("Predictive Modelling",
           fluid = TRUE,
           titlePanel("Predicted the star Rating using XGBOOST algorithm"),
           tabPanel("Model Prediction", tableOutput("view_14") , tableOutput("view_15") 
                    )
           ),
  
  tabPanel("TF - IDF",
           fluid = TRUE,
           titlePanel(h4(" ")),
           tabsetPanel(type = "tabs",
                       tabPanel("Table",
                                sidebarLayout(
                                  shiny::sidebarPanel(
                                    numericInput("numeric_5",
                                                 "Choose Rows to Display:",10)),
                                  mainPanel(tabsetPanel(type = "tabs",
                                                        tabPanel("Service", tableOutput("view_10")),
                                                        tabPanel("Handling", tableOutput("view_11")),
                                                        tabPanel("Interior", tableOutput("view_12")),
                                                        tabPanel("Price", tableOutput("view_13"))
                                                        )
                                            )
                                  )
                                ),
                       tabPanel("Vizualization",titlePanel(h4(" ")),
                                plotOutput("plot_2"))
                       )
           )
)