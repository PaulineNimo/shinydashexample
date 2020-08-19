#####
#libraries
library(shiny)
library(shinydashboard)
library(shinyauthr)
library(shinyjs)
library(rgdal)
library(leaflet)
library(dplyr)
library(RColorBrewer)

#####
#ui
ui <- dashboardPage(title = "Authentication App", skin = "black",
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
        ),
        collapsed = TRUE
    ),
    dashboardBody(
        fluidRow(
            box(title = "Controls", width = 3, 
                #selectInput("country", "Select Country", c("Choose:")),
                selectInput("indicator", "Select Indicator", c("Choose:")),
                sliderInput("year", "Select Year", value = 0, min = 0, max = 0, sep = ""),
                collapsible = T, collapsed = F),
            box(title = "Map", width = 9,
                leafletOutput("mapPlot", height = 500), collapsible = T, collapsed = F)
        )
    )
)

#####
#server
server <- function(input, output, session) {
    data <- read.csv(paste0(getwd(),"/data/Statistical Capacity Indicators.csv"), stringsAsFactors = F, header = T)
    #country_list <- unique(data$Country.Name)
    #observe({
    #    updateSelectInput(session, "country", choices = country_list)
    #})
    indicator_list <- unique(data$Series.Name)
    observe({
        updateSelectInput(session, "indicator", choices = indicator_list)
    })
    year_list <- as.numeric(substr(colnames(data)[-c(1:4)],2,5))
    observe({
        updateSliderInput(session, "year", value = max(year_list), min = min(year_list), max = max(year_list), step = 1)
    })
    map <- readOGR(paste0(getwd(),"/data/Africa_Cont_country.geojson"))
    
    data_fil <- reactive({
        sel_data <- data %>% 
            filter(Series.Name == input$indicator) %>%
            select(Country.Name ,contains(as.character(input$year)))
        sel_df <- left_join(map@data, sel_data, by = c("CNTRY_NAME" = "Country.Name"))
        sel_df <- sel_df[,-c(1:7)]
        sel_df[,2] <- as.numeric(sel_df[,2])
        return(sel_df)
    })
    observe({
        req(map)
        pal <- colorBin("BuPu", domain = data_fil()[,2], bins = 5)
        labels <- sprintf(
            "<strong>%s</strong><br/>%g",
            data_fil()[,1], data_fil()[,2]
        ) %>% lapply(htmltools::HTML)
        output$mapPlot <- renderLeaflet({ 
            leaflet(map) %>% 
                setView(lng = 18.5, lat = 3.5, zoom = 3) %>%
                addTiles() %>%
                addPolygons(
                    fillColor = ~pal(data_fil()[,2]),
                    weight = 1,
                    opacity = 1,
                    color = 'white',
                    dashArray = '1',
                    fillOpacity = 0.7,
                    highlight = highlightOptions(
                        weight = 2,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                    label = labels,
                    labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto")
                ) %>%
                addLegend(pal = pal, values = ~data_fil()[,2], opacity = 0.7, title = NULL,
                          position = "bottomright")
        })
    })
   
}

#####
#shinyapp
shinyApp(ui, server)