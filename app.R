library(shiny)
library(ggplot2)
library(scales)
library(tidyverse)
library(readxl)

Emissions.Sector <- read_excel("Emissions1990-2010.xlsx", 
                                        sheet = "Sector")
Emissions.Fuel <- read_excel("Emissions1990-2010.xlsx", 
                             sheet = "Fuel")
Sector.Tot <- filter(Emissions.Sector, Source == "Total")

sectors.a <- unique(Emissions.Sector$Sector)
sectors <- unique(Emissions.Sector$Sector)[-1]

# gather(comf, key="Year", value="Co2eqv", as.character(1990:2010))

Sector.Fuels <- function(sector) {
  sf <- filter(Emissions.Sector,  Sector == sector)
  if (nrow(sf) > 1) {
    sf <- filter(sf, !grepl("Total", Source))
  }
  return(sf)
}



# UI ----
ui <- fluidPage(
   
   titlePanel("NZ greenhouse gas emissions 1990-2010"),
   
   sidebarLayout(
      # Sidebar ----
      sidebarPanel(
      ),
      
      # Main panel ----
      mainPanel(
        tabsetPanel(id="tabs",
                   tabPanel("by-sector", 
                            plotOutput("secPlot")
                            )
                    )
      )
   )
)

server <- function(input, output) {
  SecOverall <- reactive({
    st <- gather(Sector.Tot, key="Year",
                 value="Co2eqv", as.character(1990:2010)) %>% 
      mutate(Year = as.numeric(Year), 
             Sector = factor(Sector, levels=unique(Sector)))
    # Add logic for filtering out years, sectors here
    return(st)
  })
   
  output$secPlot <- renderPlot({
    sov <- SecOverall()
    ggplot(sov, aes(x=Year, y=Co2eqv, linetype=Sector, color=Sector)) + 
      geom_line(size=1.5) + guides(linetype=guide_legend(keywidth = 3)) +
      scale_y_log10() -> s.p
    return(s.p)
  })
}

shinyApp(ui = ui, server = server)

