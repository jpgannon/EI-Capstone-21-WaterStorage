
# Define server
server <- function(input, output) {
    #----------------
    # read in cleaned watershed data
    # ---------------
    
    #MU: standardized well ws3 data to mm H2O
    standardized_Well_WS3 <-  reactive({
        ws3_upper_wells %>% 
            mutate(standardized_well_1 = ((WS3_N1_corr_depth * 10) * input$poros)) %>% 
            mutate(standardized_well_2 = ((WS3_N2_corr_depth * 10) * input$poros)) %>% 
            mutate(standardized_deep_well = ((WS3_42_4_d2_corr_depth * 10) * input$poros)) %>%
            select(TIMESTAMP, standardized_well_1, standardized_well_2, standardized_deep_well)
    })
    #Mu: standardized well ws9 data to mm H2O
    standardized_Well_WS9 <-  reactive({
        ws9_upper_wells %>% 
            mutate(standardized_well_1 = ((HB156_corr_depth * 10) * input$poros)) %>% 
            mutate(standardized_well_2 = ((HB179s_corr_depth * 10) * input$poros)) %>% 
            mutate(standardized_deep_well = ((HB176d_corr_depth * 10) * input$poros)) %>%
            select(TIMESTAMP, standardized_well_1, standardized_well_2, standardized_deep_well)
    })
    
    #MU: standardized snow ws3 data to mm H2O
    standardized_SnowHr_WS3 <-  reactive({
        ws3_upper_snowdat_hr %>% 
            mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10)))
    })
    
    #MU: standardized snow ws9 data to mm H2O
    standardized_SnowHr_WS9 <- reactive({
        ws9_upper_snowdat_hr %>%
            mutate(standardized_snow = (VWC_average * (Depthscaled_Avg * 10)))
        
    })
    
    #MU: standardized precip data ws3 to mm H2O
    
    
    #MU: joined ws3 data
    ws3_standard <- reactive ({
        full_join(standardized_Well_WS3(), standardized_SnowHr_WS3(), by = "TIMESTAMP") %>% 
            pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>% 
            filter(date() > 2020-12-16)
    })
    
    ws9_standard <- reactive ({
        full_join(standardized_Well_WS9(), standardized_SnowHr_WS9(), by = "TIMESTAMP") %>% 
            pivot_longer(!TIMESTAMP, names_to = "Water", values_to = "mm") %>% 
            filter(date() > 2020-12-16)
    })
    output$table <- DT::renderDataTable({DT::datatable(standardized_SnowHr_WS3(), #MU: When we do the calculations we can put them in one dataset and output that.
                                                       class = "display", #MU: this is the style of the table
                                                       caption = 'Table 1: This table shows x.', #MU: adds a caption to the table
                                                       filter = "top")
    })#MU: This places the filter at the top of the table
    #MU: This is a placeholder table for when we finish cleaning the data and can input summarized values
    output$plot1 <- renderPlot({
        ws3_standard() %>% 
            filter(mm > 0) %>% 
            ggplot(aes(x = TIMESTAMP, y = mm, fill=Water ))+
            geom_area()#+
        #scale_x_datetime(labels=date_format("%Y-%m-%d"), breaks = date_breaks("week"))
    })
    # output$plot2 <- renderPlot({
    #   ws9_standard() %>% 
    #     filter(mm > 0) %>% 
    #     ggplot(aes(x = TIMESTAMP, y = mm, fill=Water ))+
    #     geom_area()#+
    #   #scale_x_datetime(labels=date_format("%Y-%m-%d"), breaks = date_breaks("week"))
    # })
    #  
    output$porosPlot <- renderPlot({
        x <- seq(from = 0, to = 100, by = 0.1)
        y <- x*input$poros + input$change
        plot(x,y)
    })
    
    
    # Plot map of station locations using leaflet
    #---------------------------------------------
    
    m <-leaflet() %>% 
        addProviderTiles("OpenTopoMap", options = providerTileOptions(noWrap = TRUE)) %>% 
        addMarkers(lng= -71.7185, lat = 43.9403, popup = "Hubbard Brook Experimental Forest")
    
    output$map <- renderLeaflet(
        m
    )
    
    #---------------------------------------------
} # END Server function
#---------------------------------------------
#---------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)