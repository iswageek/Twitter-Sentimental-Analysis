
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

pcalist <- c('PC1','PC2','PC3','PC4','PC5')
numChoices <- c(1000, 2000, 3000, 4000, 5000)
colChoices <- c('positivity','anger','anticipation','disgust','fear','joy',
                'sadness','surprise','trust')
allChoices <- c('PC1','PC2','PC3','PC4','PC5',
                'positivity','anger','anticipation','disgust','fear','joy',
                'sadness','surprise','trust')

shinyUI(
    navbarPage("Twitter Analysis",
               tabPanel("Load Tweets",
                        fluidPage(
                            sidebarLayout(
                                # Sidebar with a slider and selection inputs
                                sidebarPanel(
                                    # Text box
                                    textInput("searchString",
                                              "Search Twitter for:",
                                              "acrotrend"),
                                    selectInput("numTweets", "Number of Tweets:",
                                                choices = numChoices),
                                    checkboxInput("rt_remove", "Eliminate Retweets",
                                                  value=T),
                                    checkboxInput("isUser", "Search is a Screen Name",
                                                  value=F),
                                    actionButton("update", "Search")
                                ),
                                mainPanel(plotOutput("plot"),
                                          verbatimTextOutput("tweetCount")
                                          )
                            )
                        )),
               tabPanel("Sentiments",
                        fluidPage(
                            titlePanel("Sentiment Analysis"),
                            mainPanel(plotOutput("sentiment"))
                        )),
               tabPanel("PCA",
                        fluidPage(
                            titlePanel("Principal Component Analysis"),
                            #mainPanel(plotOutput("pcaplot")),
                            rbokehOutput("pcaplot"),
                            
                            hr(),
                            
                            fluidRow(
                                column(3,
                                    selectInput('xvar', 'X Variable', allChoices)
                                ),
                                column(3,
                                       selectInput('yvar', 'Y Variable', allChoices,
                                                   selected=pcalist[2])
                                       )
                            )
                        )),
               tabPanel("Sample Tweets",
                        fluidPage(
                            sidebarPanel(
                                selectInput('pc','Component to Consider:', pcalist)
                            ),
                            mainPanel(DT::dataTableOutput('tweet_table'),
                                      verbatimTextOutput("loadingFactors"))
                        )),
               tabPanel("Time Graph",
                        fluidPage(
                            titlePanel("Time Graph"),
                            mainPanel(rbokehOutput("timeplot")),
                            #mainPanel(plotOutput("timeplot")),
                            fluidRow(
                                selectInput('yvar_time','Y Variable',allChoices,
                                            selected = allChoices[2])
                            )
                        ))
    )
)