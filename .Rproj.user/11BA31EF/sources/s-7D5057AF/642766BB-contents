#
# Ships Data that shows distance traveled at different times
#

# libraries
library(shiny)
library(shinythemes)
library(readr)
library(tidyverse)
library(leaflet)
library(geodist)

# reading in csv file
data <- read_csv("data/ships_04112020.zip")

# Define UI for application
ui <- fluidPage(
    #min theme for shiny
    theme = shinytheme("cerulean"),

    # Application title
    titlePanel("Marine Data Analysis"),

    # Row with two sections
    fluidRow(
        # first column for summary and graphs
        column(5,
               tabsetPanel(
                   # summary tab
                   tabPanel(h5(strong("Summary")),
                            br(),
                            selectInput("select_vessel", label = "Select Vessel Type:", choices = unique(data$ship_type), width = "80%"),
                            br(),
                            selectInput("select_ship", label = "Select Ship:", choices = "", width = "80%"),
                            br(),
                            div(
                                textOutput("dist_sailed"), br(),
                                textOutput("destination"), br(),
                                textOutput("speed")
                            )
                            ),
                   # charts tab
                   tabPanel(h5(strong("Graphs")),
                            br(),
                            radioButtons("stats", label = "Select Option to Display Summary Statistics:", choices = c("Speed", "Distance", "Status","Destination")),
                            plotOutput("graph")
                   )
               )
               ),
        # column for map and table
        column(7,
               tabsetPanel(
                   # map tab
                   tabPanel(h5(strong("Map")),
                            leafletOutput("marine_map", height = "590px")),
                   # table tab
                   tabPanel(h5(strong("Data")),
                            h4("Data for Selected Ship"),
                            div(DT::dataTableOutput("data_table"), style = "font-size:80%")
                            )
               )
               )
    )
)

# Define server logic
server <- function(input, output, session) {
    # select input update ship depending on ship type
    observeEvent(input$select_vessel, {
        vessel_names = data$SHIPNAME[data$ship_type %in% input$select_vessel]
        updateSelectInput(session, "select_ship", choices = unique(vessel_names))
    })

    # map initial render
    output$marine_map <- renderLeaflet({
        leaflet(data) %>%
            addTiles(group = "OSM", options = tileOptions(minZoom = 1, maxZoom = 17)) %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    })
    # data filter on ship type
    observeEvent(input$select_ship,{
        output$data_table = DT::renderDataTable({
            ship_data()[ship_data()$SHIPNAME %in% input$select_ship,]
        })
    })
    # functions
    ship_data <- reactive({
        # calculate distance in meters between observations
        data %>%
            filter(SHIPNAME %in% input$select_ship) %>%
            arrange(DATETIME, .by_group = T) %>%
            mutate(obs_dist = geodist(cbind(LON,LAT), sequential = T, pad = T, measure = "geodesic")) %>%
            mutate(is_parkchar = ifelse(is_parked == 0,"No","Yes"))
    })
    # adding information to ship data
    ship_filter <- reactive({
        # filter ship data
        sel_ship <- ship_data() %>%
            filter(SHIPNAME %in% input$select_ship) %>%
            arrange(DATETIME)

        # get largest distance covered
        end_position <- sel_ship %>%
            filter(obs_dist == max(obs_dist, na.rm = T)) %>%
            arrange(desc(DATETIME)) %>%
            filter(row_number() == 1) %>%
            mutate(popup = "Terminal")

        # add information on starting point and markers
        start_position <- sel_ship[which(sel_ship$DATETIME == end_position$DATETIME) - 1,]
        start_position$popup <- "Initial"
        filt_data <- rbind(start_position,end_position)
        filt_data$labelopt <- paste("Position: ", filt_data$popup, "<br/>",
                           "Flag: ", filt_data$FLAG, "<br/>",
                           "Time: ", filt_data$DATETIME, "<br/>",
                           "Speed: ", filt_data$SPEED, " knots", "<br/>",
                           "Destination: ", filt_data$DESTINATION, "<br/>",
                           "Course: ", filt_data$COURSE, " degrees"
        )
        return(filt_data)
    })
    # update map with markers and line
    map_render <- reactive({
        leafletProxy("marine_map", data = ship_filter()) %>%
            clearMarkers() %>%
            addMarkers(lng = ~LON, lat = ~LAT, popup = ~labelopt,
                       popupOptions = popupOptions(keepInView = T, closeOnClick = F, closeButton = F)) %>%
            addPolylines(lng = ~LON, lat = ~LAT) %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))
    })
    # update text on summary tab
    info_render <- reactive({
        print(ship_filter()$labelopt)

        info_txt <- data.frame(
            dist_sailed = paste0("The most recent longest distance sailed by ",input$select_ship," in two minutes is ",formatC(ship_filter()[2,"obs_dist"], format="d", big.mark=","), " metres.
                                 This happened between ", as.character(ship_filter()$DATETIME[1]), " and ", as.character(ship_filter()$DATETIME[2]), " UTC."),
            destination = paste0("The ship's destination is  ",ship_filter()[2,"DESTINATION"], "."),
            speed = paste0("The ship's speed was ",ship_filter()[2,"SPEED"], " knots.")
        )
        return(info_txt)
    })
    # filter the ships by selected and get most recent longest distance
    observeEvent(input$select_ship,{
        req(input$select_ship)
        #filter by ship
        ship_filter()
        #re-render map
        map_render()
        #change information
        output$dist_sailed <- renderText({info_render()$dist_sailed})
        output$destination <- renderText({info_render()$destination})
        output$speed <- renderText({info_render()$speed})
    })

    # charts
    observeEvent(input$stats, {
        if(input$stats == "Speed"){
            output$graph <- renderPlot({
                ggplot(ship_data(), aes(DATETIME, SPEED)) +
                    geom_point() +
                    ggtitle("Ship Speed over Time") +
                    xlab("Time") +
                    ylab("Speed in knots")
            })
        } else if(input$stats == "Distance"){
            output$graph <- renderPlot({
                ggplot(ship_data(), aes(DATETIME, obs_dist)) +
                    geom_point() +
                    ggtitle("Ship Distance Sailed over Time") +
                    xlab("Time") +
                    ylab("Distance in metres")
            })
        } else if(input$stats == "Status"){
            output$graph <- renderPlot({
                ggplot(ship_data(), aes(DATETIME, as.factor(is_parked))) +
                    geom_point() +
                    ggtitle("Ship Parked Status over Time") +
                    xlab("Time") +
                    ylab("Ship Parked Status")
            })
        } else if(input$stats == "Destination"){
            output$graph <- renderPlot({
                ggplot(ship_data(), aes(DATETIME, DESTINATION)) +
                    geom_point() +
                    ggtitle("Ship Destination over Time") +
                    xlab("Time") +
                    ylab("Ship Destination")
            })
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)
