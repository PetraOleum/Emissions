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
ui <- navbarPage("NZ Emissions", collapsible = TRUE,
   # Sector tab ----
   tabPanel("Sector",
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
     tabPanel("By fuel")
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
}

shinyApp(ui = ui, server = server)

