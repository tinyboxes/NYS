# Server for Maternity Trends in NY State App

server <-  function(input, output, session) {
  
  ### Outputs ###
  
  # need category input and add measure drop down box
  output$metric <- renderUI({
    
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'metric', 
                   label = 'Metric',
                   choices = unique(maternity$Measure[maternity$Category == input$cat]))
    
  })
  
  output$hospital <- renderUI({
    
    # Input: Dropdown Box for Hospital
    selectizeInput(inputId='hospital', label = 'Hospital',
                   choices = unique(maternity$HospitalName[maternity$County == input$county]))
    print('test')

  })
  
  # tab 1
  
  yearMetric <- reactive({
    
    merged %>%
      dplyr::filter(., Year %in% input$year & Measure %in% input$metric) %>%
      select(County, n, t)
    
  })
  
  # Color Palette for map
  bins <- c(0, 50, 100, 200, 500, 1000, 2500, 5000, Inf)
  pal <- colorBin("YlGnBu", domain = total$t, bins = bins)
  
  # Format popup data for leaflet map.
  popup_dat <- paste0('<strong>County: </strong>', NY$NAME, 
                      '<br><strong>Population: </strong>', NY$POP2010)
  
  output$basicMap <- renderLeaflet({
    
    leaflet(NY) %>%
      setView(lng = -76.0, lat = 42.75, zoom = 6.25) %>%
      addTiles() %>%
      addPolygons(fillColor = 'blue',
                  weight = 1,
                  opacity = 1,
                  color = 'white',
                  dashArray = '3',
                  fillOpacity = 0.5, 
                  highlight = highlightOptions(weight = 3,
                                               color = 'white',
                                               dashArray = '',
                                               fillOpacity = 0.2,
                                               bringToFront = TRUE),
                  popup = popup_dat)
    
  })
  
  output$countyMetric <- renderLeaflet({
    
    countyMetric <- leaflet(yearMetric()) %>%
      addProviderTiles('CartoDB.Positron') %>% 
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addPolygons(data=NY,
                  color= ~cpal(n),
                  fillOpacity = 0.5,
                  popup = ~paste('<b>County:</b>',County,'<br>',
                                 '<b>Count: </b>', n, '<br>', 
                                 '<b>Percent of Total Births</b>', n/t,'<br>'))
  })
  
  output$births <- renderLeaflet({
    
    # filter by User input measure
    # mypal <- colorQuantile(palette=c("green","yellow","orange","red"),
    #                    domain=as.data.frame(dplyr::select(yearMetric(), n)))
    # print("did you get this far?")
    
    leaflet(yearMetric()) %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lat = 42.75, lng = -76.00, zoom = 6.1) %>%
      addPolygons(NY, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.3,
                  fillColor = ~mypal(n),
                  popup = paste("Region: ", County, "<br>",
                                "Count: ", n, "<br>")) %>%
      addLegend(position = "bottomleft", pal = mypal, values = n,
                title = "Total Births",
                opacity = 1)
    
  })
  
  # tab 2
  
  # reactive filter
  percent <- reactive({
    
    merged %>%
      dplyr::filter(., Measure %in% input$metric2 & Year %in% input$year2) %>%
      group_by(., County) %>%
      dplyr::summarise(p = round(n/t*100, 2))
    
  })
  
  output$countyPlot = renderPlot({
    
    ggplot(percent(), 
           aes(x = County, y = p, fill = County)) + 
      geom_bar(stat = 'identity', position = 'dodge') +
      xlab('New York County') + 
      ylab('Percent of Total County Births') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      legend(position = 'bottom')
    
  })
  
  # tab 4
  
  countyHosp <- reactive({
    
    maternity %>%
      filter(., Category %in% input$cat4 & County %in% input$county4)
    
  })
  
  hosp <- reactive({
    
    maternity %>%
      filter(., Category %in% input$cat4 & HospitalName %in% input$hospital)
    
  })
  
  output$hospitalHist = renderPlot({
    
    ggplot(hosp(),
           aes(x = Year, y = Count, group = Measure)) +
      geom_bar(aes(fill = Measure), stat  = 'identity', position = 'dodge') +
      scale_x_discrete('Year', limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
      theme(legend.position="bottom")
    
  })
  
  #by Hospital
  output$hospitalPlot = renderPlot({
    
    ggplot(countyHosp(), 
           aes(x = HospitalName, y = Percent)) + 
      geom_bar(stat = 'identity', position = 'dodge') +
      xlab('Hospital') + 
      ylab('Percent of Total Hospital Births')
    
  })
  
  session$onSessionEnded(stopApp)
}