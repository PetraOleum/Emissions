library(shiny)
library(ggplot2)
library(scales)
library(tidyverse)
library(readxl)

Emissions.Sector <- read_excel("Emissions1990-2010.xlsx", 
                                        sheet = "Sector")
Emissions.Fuel <- read_excel("Emissions1990-2010.xlsx", 
                             sheet = "Fuel")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   titlePanel("NZ emissions 1990-2010"),
   
   sidebarLayout(
      sidebarPanel(
      ),
      
      mainPanel(
      )
   )
)

server <- function(input, output) {
   
}

shinyApp(ui = ui, server = server)

