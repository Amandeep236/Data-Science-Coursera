

library(shiny)
library(dplyr)


shinyUI(fluidPage(
      
      titlePanel(h1("Harmful Weather Events Exploration")),
      
      sidebarLayout(
        
        sidebarPanel(

          br(),
          selectInput("health","1.Population Health",choices = c("Both","Fatality", "Injury"),selected = "Both"),
          br(),
          radioButtons("eco","2.Economic Consequence (isolate)",list("Both","Property", "Crop"),selected = "Both"),
          #submitButton("Update!"),
          #p("Click on the Update button to display the result of the economic consequence."),
          actionButton("action", "Update View"),
          p("Click on the Update button to display the result of the economic consequence."),
          br(),
          h6("Powered by :"),
          tags$img(src = "RStudio.png", height = 50, width = 50)
        ),
        
        mainPanel(
          
          tabsetPanel(
          tabPanel("About",includeMarkdown("README.md")),  
          tabPanel("Plot",column(10,plotOutput("Plot"),plotOutput("Plot2"))),
          #tabPanel("Data", tableOutput("Data"))
          tabPanel('Data',
                   column(4,wellPanel(selectInput("Topeventforpopulationhealth",
                                                   "Top events for population health :",
                                                   c("Top 10", "Top 20", "Top 50"), selected = "Top 10"))),
                   column(4,wellPanel(selectInput("TopeventforEcoloss","Top events for Economic loss :",
                                                   c("Top 10", "Top 20", "Top 50"), selected = "Top 10"))),
                   column(7,tableOutput("Data1"),tableOutput("Data2")))
           ))
        
        
        
        )))
      




