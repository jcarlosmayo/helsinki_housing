library(shiny)
library(ggmap)
library(leaflet)

shinyServer(function(input, output, session) {
    
    react_housing_data <- reactive({
        data <- subset(housing, price >= input$price_slider[1] & price <= input$price_slider[2] &
                        size >= input$size_slider[1] & size <= input$size_slider[2] &
                        rooms >= input$room_slider[1] & rooms <= input$room_slider[2])
        
        
        
        if (input$housing_type != "All") {
            data <- subset(data, type == tolower(input$housing_type))
        }
        
        data
    }) # END REACTIVE HOUSING CONTENT
    
    # MAP PLOT
    output$map_plot <- renderLeaflet({
        
        m = leaflet() %>% addTiles(urlTemplate = "http://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png",
                                   attribution = 'Tiles courtesy of <a target="_blank" href="http://openstreetmap.se/" target="_blank">OpenStreetMap Sweden</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> | 
                                        Data extracted using <a target="_blank" href="https://www.kimonolabs.com/">Kimono labs</a>,
                                        geolocation obtained via <a target=_"blank" href="https://github.com/dkahle/ggmap">
                                        ggmaps</a>')
        
        m = m %>% fitBounds(lng1 = min(react_housing_data()$lon), lat1 = min(react_housing_data()$lat),
                            lng2 = max(react_housing_data()$lon), lat2 = max(react_housing_data()$lat))
        
        m = m %>% addCircleMarkers(react_housing_data()$lon, react_housing_data()$lat,
                                   radius = 1.5, color = "#981DB2", weight = react_housing_data()$rooms, fill = FALSE,
                                   popup = paste(react_housing_data()$price, "EUR</br>",
                                                 react_housing_data()$size, "m<sup>2</sup></br>",
                                                 react_housing_data()$rooms, "rooms"))
        m  # Display map
        
        
    }) # END OF MAP PLOT
    
    # Number of rentals in Helsinki
    output$n_helsinki <- renderText({
        nrow(subset(react_housing_data(), city=="Helsinki"))
    })
    
    # Number of rentals in Helsinki
    output$n_espoo <- renderText({
        nrow(subset(react_housing_data(), city=="Espoo"))
    })
    
    # Number of rentals in Helsinki
    output$n_vantaa <- renderText({
        nrow(subset(react_housing_data(), city=="Vantaa"))
    })
    
    # Average price of rental in Helsinki
    output$avg_price_sqm_helsinki <- renderText({
        data <- subset(react_housing_data(), city=="Helsinki")
        round(mean(data$price_sqm),2)
    })
    
    # Average price of rental in Espoo
    output$avg_price_sqm_espoo <- renderText({
        data <- subset(react_housing_data(), city=="Espoo")
        round(mean(data$price_sqm),2)
    })
    
    # Average price of rental in Vantaa
    output$avg_price_sqm_vantaa <- renderText({
        data <- subset(react_housing_data(), city=="Vantaa")
        round(mean(data$price_sqm),2)
    })
    
    output$plot_histogram <- renderPlot(
        hist(react_housing_data()$price, axes = TRUE,
             breaks = 100, 
             xlab = "PRICE", ylab = "", main ="",
             col = "#981DB2"))
    
})