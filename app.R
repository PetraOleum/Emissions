library(shiny)
library(ggplot2)
library(scales)
library(tidyverse)
library(readxl)

# Original data from 
# https://catalogue.data.govt.nz/dataset/
# new-zealands-energy-outlook/resource/3caad31e-aa96-4c94-8f54-1000518a0690?inner_span=True

Emissions.Sector <- read_excel("Emissions1990-2010.xlsx", 
                                        sheet = "Sector")
Emissions.Fuel <- read_excel("Emissions1990-2010.xlsx", 
                             sheet = "Fuel")
Sector.Tot <- filter(Emissions.Sector, Source == "Total")

sectors.a <- unique(Emissions.Sector$Sector)
sectors <- unique(Emissions.Sector$Sector)[-1]

fuels <- Emissions.Fuel$Fuel

# gather(comf, key="Year", value="Co2eqv", as.character(1990:2010))

Sector.Fuels <- function(sector) {
  sf <- filter(Emissions.Sector,  Sector == sector)
  if (nrow(sf) > 1) {
    sf <- filter(sf, !grepl("Total", Source))
  }
  return(sf)
}



# UI ----
ui <- navbarPage("NZ Emissions", collapsible = TRUE,
   # Sector tab ----
   tabPanel("By sector",
                h3("Emissions by sector"),
                plotOutput("secPlot"),
                fluidRow(
                 column(5, offset=1,
                        h4("Options"),
                    sliderInput("secyears", "Date range", 
                      min=1990, max=2010, value=c(1990, 2010),
                      sep="", step=1),
                    selectInput("secscale", "Scale", 
                      choices=c("y", "sqrt(y)", "log10(y)"), selected="y"),
                    checkboxInput("seclegend", "Show legend", value=TRUE),
                    checkboxInput("secminc", "Set minimum", value=FALSE),
                    conditionalPanel("input.secminc",
                     numericInput("secmin", label=NULL, value=NA)),
                    checkboxInput("secmaxc", "Set maximum", value=FALSE),
                    conditionalPanel("input.secmaxc",
                     numericInput("secmax", label=NULL, value=NA))
                        ),
                 column(5, offset=1,
                      h4("Sectors"),
                     checkboxGroupInput("secSectors", NULL, 
                      choices=sectors.a, selected = sectors.a)
                    )
                )             
        ),
     # Fuel tab ----
     tabPanel("By fuel",
              h3("Emissions by fuel"),
              plotOutput("fuPlot"),
                fluidRow(
                 column(5, offset=1,
                        h4("Options"),
                    sliderInput("fuyears", "Date range", 
                      min=1990, max=2010, value=c(1990, 2010),
                      sep="", step=1),
                    selectInput("fuscale", "Scale", 
                      choices=c("y", "sqrt(y)", "log10(y)"), selected="y"),
                    checkboxInput("fulegend", "Show legend", value=TRUE),
                    checkboxInput("fuminc", "Set minimum", value=FALSE),
                    conditionalPanel("input.fuminc",
                     numericInput("fumin", label=NULL, value=NA)),
                    checkboxInput("fumaxc", "Set maximum", value=FALSE),
                    conditionalPanel("input.fumaxc",
                     numericInput("fumax", label=NULL, value=NA))
                        ),
                 column(5, offset=1,
                      h4("Fuels"),
                     checkboxGroupInput("fuFuels", NULL, 
                      choices=fuels, selected = fuels)
                    )
                )             
              )
)

server <- function(input, output) {
  SecOverall <- reactive({
    st <- gather(Sector.Tot, key="Year",
                 value="Co2eqv", as.character(1990:2010)) %>% 
      mutate(Year = as.integer(Year), 
             Sector = factor(Sector, levels=unique(Sector))) %>%
    # Logic for filtering out years, sectors here
      filter(Year %in% input$secyears[1]:input$secyears[2], 
             Sector %in% input$secSectors) %>%
      mutate(Year = as.Date(paste0(Year, "-01-01")))
    return(st)
  })
   
  output$secPlot <- renderPlot({
    sov <- SecOverall()
    ggplot(sov, aes(x=Year, 
                    y=Co2eqv, linetype=Sector, color=Sector)) -> s.p
    s.p + geom_line(size=1.5, na.rm=TRUE) -> s.p
    s.p + guides(linetype=guide_legend(keywidth = 5)) -> s.p
    s.p + theme_linedraw() -> s.p
    s.p + theme(legend.position = "right", 
                legend.title = element_blank()) -> s.p
    s.p + ylab("CO₂ equivalent (kt)") -> s.p
    if (!input$seclegend) {
      s.p + theme(legend.position="none") -> s.p
    }
    mmx <- c(ifelse(input$secminc, input$secmin, NA),
             ifelse(input$secmaxc, input$secmax, NA))
    if (input$secscale == "sqrt(y)") {
      s.p + scale_y_sqrt(breaks=pretty_breaks(n=5), limits=mmx) -> s.p
    } else if (input$secscale == "log10(y)") {
      s.p + scale_y_log10(breaks=pretty_breaks(n=5), limits=mmx) -> s.p
    } else {
      s.p + scale_y_continuous(breaks=pretty_breaks(n=5), limits=mmx) -> s.p
    }
    return(s.p)
  })
  
  FuOverall <- reactive({
    st <- gather(Emissions.Fuel, key="Year",
                 value="Co2eqv", as.character(1990:2010)) %>% 
      mutate(Year = as.integer(Year), 
             Fuel = factor(Fuel, levels=unique(Fuel))) %>%
    # Logic for filtering out years, fuels here
      filter(Year %in% input$fuyears[1]:input$fuyears[2], 
             Fuel %in% input$fuFuels) %>%
      mutate(Year = as.Date(paste0(Year, "-01-01")))
    return(st)
  })
  
  output$fuPlot <- renderPlot({
    fov <- FuOverall()
    ggplot(fov, aes(x=Year, 
                    y=Co2eqv, linetype=Fuel, color=Fuel)) -> f.p
    f.p + geom_line(size=1.5, na.rm=TRUE) -> f.p
    f.p + guides(linetype=guide_legend(keywidth = 5)) -> f.p
    f.p + theme_linedraw() -> f.p
    f.p + theme(legend.position = "right", 
                legend.title = element_blank()) -> f.p
    f.p + ylab("CO₂ equivalent (kt)") -> f.p
    if (!input$fulegend) {
      f.p + theme(legend.position="none") -> f.p
    }
    mmx <- c(ifelse(input$fuminc, input$fumin, NA),
             ifelse(input$fumaxc, input$fumax, NA))
    if (input$fuscale == "sqrt(y)") {
      f.p + scale_y_sqrt(breaks=pretty_breaks(n=5), limits=mmx) -> f.p
    } else if (input$fuscale == "log10(y)") {
      f.p + scale_y_log10(breaks=pretty_breaks(n=5), limits=mmx) -> f.p
    } else {
      f.p + scale_y_continuous(breaks=pretty_breaks(n=5), limits=mmx) -> f.p
    }
    return(f.p)
  })
}

shinyApp(ui = ui, server = server)

