# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  
  
  titlePanel("title panel"),
  sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30)
                             #sliderInput("percent","Number of components in PCR:",min = 2,max = 10,value = 3)
                ),
                
                mainPanel(
                  tabsetPanel(
                    tabPanel("Plot",
                             # fluidRow(...)
                             plotOutput("p1"),
                             plotOutput("p2")
                    )
                  )
                )
                
                #mainPanel("main panel",
                #          column(7,plotOutput(outputId="distPlot"))
                #)
  )))