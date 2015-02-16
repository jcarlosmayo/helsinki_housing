shinyUI(
    fluidPage(
        theme = "bootstrap.css", # Theme http://bootswatch.com/spacelab/
        
        # LEAFLET MAP
        leafletOutput("map_plot"),
        
        # BRIEF CLARIFICATION
        fluidRow(column(5, offset = 1,
                        tags$p(class ="text-muted", "To remove extreme outliers only the central 95th quantile is displayed")
                        )
                 ), # END FLUID ROW
        
         br(), br(), # Space between the map and the rest of the content  
        
        # FLUID ROW FOR USER INPUT AND REACTIVE FIGURES
        fluidRow(
            
            column(2, offset = 1,
                   
                   # PRICE
                   sliderInput("price_slider", "PRICE EUR", 
                               min = quantile(housing$price, 0.025),
                               max = quantile(housing$price, 0.975),
                               value = c(quantile(housing$price, 0.025),
                                         quantile(housing$price, 0.975)), 
                               step=50),
                   
                   br(), # Space between input sliders
                   
                   # NUMBER OF ROOMS
                   sliderInput("room_slider", 
                               label = "NUMBER OF ROOMS:",
                               step = 1,
                               min = min(housing$rooms), max = max(housing$rooms),
                               value = c(min(housing$rooms), max(housing$rooms))),
                   
                   p(class="text-muted",
                     "0 rooms correspond to a room in a shared apartment")
            ),

            column(2,
                   # SIZE
                   sliderInput("size_slider", "SIZE SQUARE METRES", 
                               min = round(quantile(housing$size, 0.025), 1),
                               max = round(quantile(housing$size, 0.975), 1),
                               value = c(quantile(housing$size, 0.025),
                                         quantile(housing$size, 0.975))),
                   
                   br(), # Space between input slider and selection box
                   
                   # HOUSING TYPE
                   selectInput("housing_type", 
                               label = "HOUSING TYPE",
                               choices = list("All", "Apartment", "Terrace",
                                              "Semi-detached", "Single house"),
                               selected = "All")
            ),
            
            # Create a div where to display reactive figures
            tags$div(id ="center-content",
                        column(1, offset=1,
                            h3("HELSINKI"),
                            h1(textOutput("n_helsinki")),
                            h5("AVAILABLE"),
                            br(),
                            h1(textOutput("avg_price_sqm_helsinki")),
                            h5("EUR / SQM")
                            ),
            
                        column(1, offset=1,
                            h3("ESPOO"),
                            h1(textOutput("n_espoo")),
                            h5("AVAILABLE"),
                            br(),
                            h1(textOutput("avg_price_sqm_espoo")),
                            h5("EUR / SQM")
                            ),
            
                        column(1, offset=1,
                            h3("VANTAA"),
                            h1(textOutput("n_vantaa")),
                            h5("AVAILABLE"),
                            br(),
                            h1(textOutput("avg_price_sqm_vantaa")),
                            h5("EUR / SQM")
                            )
                     
                     ) # END DIV
            
            ), # END SLIDERS FLUID ROW  
        
        # PRICE HISTOGRAM
        fluidRow(
            plotOutput("plot_histogram", height = "200px")
            )
    
        ) # END FLUID PAGE
    ) # END SHINY