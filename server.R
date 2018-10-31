# Server for Maternity Trends in NY State App

server <-  function(input, output, session) {
  
  ### Outputs ###

  # need category input and add measure drop down box
  output$metric <- renderUI({
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'metric', 
                   label = 'Metric',
                   choices = unique(countyMaternity$Measure
                                    [countyMaternity$Category == input$cat]))
  })
  
  output$metric2 <- renderUI({
    
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'metric2', 
                   label = 'Metric',
                   choices = unique(countyMaternity$Measure
                                    [countyMaternity$Category == input$cat2]))
  })
  
  output$metric3 <- renderUI({
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'metric3', 
                   label = 'Metric',
                   choices = unique(countyMaternity$Measure
                                    [countyMaternity$Category == input$cat3]))
  })
  
  output$metric4 <- renderUI({
    #reactive input select hospital after selecting county
    # Input: Dropdown Box for Hospital
    selectizeInput(inputId='metric4', label = 'Metric',
                   choices = unique(countyMaternity$Measure[countyMaternity$Category == input$cat4]))
  })
  
  output$hosp1 <- renderUI({
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'hosp1', 
                   label = 'Hospital 1',
                   choices = unique(countyMaternity$HospitalName))
  })
  
  output$hosp2 <- renderUI({
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'hosp2', 
                   label = 'Hospital 2',
                   choices = unique(countyMaternity$HospitalName))
  })
  
  ### Intro page ###
  
  # Color Palette for map
  
  
  # Format popup data for leaflet map.
  popup_dat <- paste0('<strong>County: </strong>', NY$NAME, 
                      '<br><strong>Population: </strong>', NY$POP2010)
  
  output$basicMap <- renderLeaflet({
    
    leaflet(NY) %>%
      setView(lng = -76.0, lat = 42.75, zoom = 6.45) %>%
      addTiles() %>%
      addPolygons(fillColor ='blue' ,
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
  
  ### tab 1
  
  ### map for County metrics
  
  map <- reactive({
    shape <-  readOGR('./Shapes/Counties_Shoreline.shp')
    NY <- spTransform(shape, CRS('+proj=longlat +ellps=GRS80'))
    })
    
  output$countyMap <- renderLeaflet({
    m <- map()
    
    # reactive subset of shapefile data
    sub <- countyMaternity %>%
      filter(., Year %in% input$year) %>%
      mutate(., NAME = County) %>%
      group_by(., NAME, Measure) %>%
      summarise(., Total = sum(Count)) %>%
      spread(., key = Measure, value = Total) %>%
      ungroup() %>%
      mutate_at(., vars(NAME), as.factor)
    
    m@data <- m@data %>%
      full_join(., sub, by='NAME')
    
    m$variableplot <- as.numeric(m@data[, input$metric])
    
    pal <- colorBin("YlOrRd", domain = m$variableplot, bins = 7)
    
    labels <- sprintf('%s: %g', m$County, m$variableplot) %>%
      lapply(htmltools::HTML)
    
    leaflet(m) %>%
      addProviderTiles('CartoDB.Positron') %>% 
      setView(lng = -76.0, lat = 42.75, zoom = 6) %>%
      addPolygons(opacity = 1,
                  weight = 0.5,
                  color= ~pal(variableplot),
                  dashArray = '3',
                  fillOpacity = 0.7,
                  #label = labels,
                  popup = ~paste('<b>County:</b>', m@data$NAME ,'<br>',
                                 '<b>Count: </b>', m$variableplot, '<br>'),
                  highlight = highlightOptions(weight = 3,
                                               color = 'white',
                                               fillOpacity = 1,
                                               bringToFront = TRUE)) %>%
      addLegend(position = 'bottomright', pal = pal, values = ~variableplot,
                title = 'Births per county',
                opacity = 1)
  })
 
  p <- reactive ({
    merged %>%
      dplyr::filter(., Measure %in% input$metric & Year %in% input$year)
  })
  a <- reactive({
    stateAve %>%
      dplyr::filter(., Measure %in% input$metric & Year %in% input$year)
  })
  
  output$countyPlot <- renderPlot({
    ggplot(data = p(), aes(x = County, y = n, fill = County)) + 
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_hline(yintercept = a()$ave, color="blue") +
      ylab('New York State Counties') + 
      xlab('County Births') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill=FALSE)
  })
  
  # output$text1 <- renderText({paste('You have selected', input$metric)})
  # output$text2 <- renderText({paste('<b>Highest County:</b>', p()$County[1],
  #                                   '<br>','<b>Count:', p()$n[1], '<br>',
  #                                   '<b>Lowest County:</b>', p()$County[-1],
  #                                   '<br>','<b>Count:', p()$n[-1], '<br>',
  #                                   '<b>State Average:</b>', a()$ave)})
    
    
  output$text <- renderUI({
    
    t <- p() %>%
        dplyr::filter(., Measure %in% input$metric & Year %in% input$year) %>%
        arrange(desc(n))
  
    top <- head(t, 1)
    bottom <- tail(t, 1)
    
    str1 <- paste('<b>You have selected:</b>', input$metric)
    str2 <- paste('<b>Highest County:</b>', top$County,
                  '<br><b>Count:</b>', top$n, 
                  '<br><b>Lowest County:</b>', bottom$County,
                  '<br><b>Count:</b>', bottom$n, 
                  '<br><b>State Average:</b>', a()$ave)
    HTML(paste(str1, str2, sep = '<br/>'))
  })
  
  ### tab 2
  # yearly trend for counties vs state average
  ten <- reactive ({
    t <- merged %>%
      dplyr::filter(., Measure %in% input$metric2 & Year %in% input$year2) %>%
      arrange(desc(n))
  })
  
  ### bottom/top Counties
  output$state <- renderPlot({
    
    if (input$top == 'Top') {
      b <- head(ten(), 10)
    } else {
      b <- tail(ten(), 10)
    }
      
    d <- stateAve %>%
        dplyr::filter(., Measure %in% input$metric2 & Year %in% input$year2)

    ggplot(data = b, aes(x = County, y = n, fill = County)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      geom_hline(yintercept = d$ave, color="blue") +
      theme(legend.position='right') +
      xlab('New York State Counties') + 
      ylab('County Births') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill=FALSE)
  })
  
  ### for county vs state
  # c <- reactive({
  #   
  #   c <- merged %>%
  #     dplyr::filter(., Measure == input$metric2) %>%
  #     dplyr::filter(., County == input$county2) %>%
  #     select(., County, Year, Measure, n) %>%
  #     sor
  #   
  # })

  ### tab 3
  output$hospitalMap <- renderGvis({
    # filter input data
    h <- hospital %>%
      dplyr::filter(., Measure %in% input$metric3 & Year %in% input$year3 )
    
    # map data
    gvisGeoChart(h, locationvar = 'HospitalName',
                 sizevar='Count',
                 options=list(region='US-NY',displayMode='markers',
                              resolution='metros',
                              colorAxis= "{colors:[\'orange\', \'yellow\', \'green\', \'blue\']}",
                              magnifyingGlass='{enable: true, zoomFactor: 10.0}')
                 )
  })
  
  ### tab 4
  
  # hospital vs hospital vs state ave
  
  output$hospitalLine <- renderPlot({
    hosp <-  countyMaternity %>%
        dplyr::filter(., Measure %in% input$metric4)
    
    s <- stateAve %>%
      dplyr::filter(., Measure %in% input$metric4)
    
    h <- hosp %>%
      dplyr::filter(., HospitalName %in% input$hosp1 | HospitalName %in% input$hosp2)
  
    ggplot(data = h, aes(x = Year, y = Count, color = HospitalName)) +
      geom_line(size=1.2) +
      geom_line(data = s, aes(x=Year, y = ave), linetype='dashed', size=1.2) +
      scale_x_discrete('Year', limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
      theme(legend.position="bottom")
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}