library(tidyverse)
library(leaflet)
library(tigris)
library(shiny)
library(shinythemes)

# Data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
states <- states(cb=T)


ui <- fluidPage(theme = shinytheme("readable"),

    titlePanel("Government Spending on Kids"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("year", 
                        label = "Select a year",
                        min = 1997, 
                        max = 2016, 
                        value = 50,
                        sep = "", 
                        ticks = FALSE),
            selectInput("var", 
                        label = "Select a variable",
                        choices = c("K-12 Education" = "PK12ed",
                                    "Higher Education" = "highered",
                                    "Pell Grants" = "pell",
                                    "Head Start (Private)" = "HeadStartPriv",
                                    "Libraries" = "lib",
                                    "Parks & Recreation" = "parkrec",
                                    "TANF" = "TANF",
                                    "SNAP" = "SNAP",
                                    "Social Security" = "socsec",
                                    "Unemployment" = "unemp",
                                    "Worker's Compensation" = "wcomp",
                                    "Medicaid & CHIP" = "Medicaid_CHIP",
                                    "Public Health" = "pubhealth",
                                    "Household & Community Development" = "HCD"
                        ), 
                        selected = 1),
            radioButtons("adjust",
                         label = "Select an adjustment", 
                         choices = list("Raw" = "raw",
                                        "Inflation adjusted" = "inf_adj",
                                        "Inflation adjusted, per child (in $1000s)" = "inf_adj_perchild"),
                         selected = "raw"),
            br(),
            helpText("Data from the Urban Institute c/o tidykids package"),
            helpText("#TidyTuesday, created by @kllycttn")
        ),

        mainPanel(
            leafletOutput("leaf", height = "700px"),
        )
    )
)

server <- function(input, output) {
    
    data_filter <- reactive({
        kids_data <- kids %>% 
            filter(variable == input$var) %>% 
            filter(year == input$year) 
        kids_data <- geo_join(states, kids_data, "NAME", "state", how = "inner")
        subset(kids_data, !is.na(input$adjust))
    })
    output$leaf <- renderLeaflet({
        data <- data_filter()
        popup_kids <- paste0(as.character(data$NAME), ", Annual Spending: $", as.character(round(data[[input$adjust]], digits = 2)))
        pal <- colorNumeric("PuBuGn", domain=data[[input$adjust]])
        leaflet() %>% 
            addProviderTiles("CartoDB.Positron") %>%
            setView(-98.483330, 38.712046, zoom = 4) %>% 
            addPolygons(data = data_filter(), 
                        fillOpacity = 0.7, 
                        fillColor = ~pal(data[[input$adjust]]), 
                        weight = 0.2, 
                        smoothFactor = 0.2, 
                        label = popup_kids,
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addLegend(pal = pal, 
                      values = data[[input$adjust]], 
                      position = "topright", 
                      labFormat = labelFormat(prefix = "$"),
                      opacity = 1,
                      title = "Amount Spent")
         })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
