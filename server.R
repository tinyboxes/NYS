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
  
  output$metric2 <- renderUI({
    
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'metric2', 
                   label = 'Metric',
                   choices = unique(maternity$Measure[maternity$Category == input$cat]))
    
  })
  
  output$metric3 <- renderUI({
    
    # reactive input select metrics to be graph
    selectizeInput(inputId = 'metric3', 
                   label = 'Metric',
                   choices = unique(maternity$Measure[maternity$Category == input$cat]))
    
  })
  
  output$hospital <- renderUI({
    
    #reactive input select hospital after selecting county
    # Input: Dropdown Box for Hospital
    selectizeInput(inputId='hospital', label = 'Hospital',
                   choices = unique(
                     maternity$HospitalName[maternity$County %in% input$county4])
    )
    
  })
  
  ### Intro page ###
  
  # Color Palette for map
  bins <- c(0, 100, 200, 500, 1000, 2500, 5000, 10000, Inf)
  pal <- colorBin("YlGnBu", domain = total$t, bins = bins)
  
  # Format popup data for leaflet map.
  popup_dat <- paste0('<strong>County: </strong>', NY$NAME, 
                      '<br><strong>Population: </strong>', NY$POP2010)
  
  output$basicMap <- renderLeaflet({
    
    leaflet(NY) %>%
      setView(lng = -76.0, lat = 42.75, zoom = 6.55) %>%
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


  ### tab 1
  
  # '%!in%' <- function(x,y)!('%in%'(x,y))
  
  # map for metric, percent percents
  output$countyMetric <- renderLeaflet({
    
    m <- merged %>%
      dplyr::filter(., Year %in% input$year & Measure %in% input$metric & County != 'Statewide') %>%
      select(County, n, t)
    
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>% 
      setView(lng = -76.0, lat = 42.75, zoom = 6) %>%
      addPolygons(data = NY,
                  opacity = 1,
                  weight = 0.5,
                  color= ~pal(m$n),
                  fillOpacity = 0.5,
                  popup = ~paste('<b>County:</b>', NY@data$NAME,'<br>',
                                 '<b>Count: </b>', m$n, '<br>', 
                                 '<b>Percent of Total Births</b>', 
                                 round(m$n/m$t*100, 2),'<br>'),
                  highlight = highlightOptions(weight = 3,
                                               color = 'white',
                                               fillOpacity = 1,
                                               bringToFront = TRUE)) %>%
      addLegend(position = 'bottomright', pal = pal, values = m$n,
                title = 'Births per county',
                opacity = 1)
    
  })
  
  ### tab 2
  
  output$countyPlot <- renderPlot({
    
    p <- merged %>%
      dplyr::filter(., Measure %in% input$metric2 & Year %in% input$year2 & County != 'Statewide') %>%
      group_by(., County) %>%
      dplyr::summarise(p = round(n/t*100, 2))
    
    ggplot(data = p, 
           aes(x = County, y = p, fill = County)) + 
      geom_bar(stat = 'identity', position = 'dodge') +
      xlab('New York County') + 
      ylab('Percent of Total County Births') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      guides(fill=FALSE)
    
  })
  
  ### tab 3
  
  output$hospitalMap <- renderGvis({
    
    # filter input data
    h <- hospital %>%
      dplyr::filter(., Measure %in% input$metric3 & Year %in% input$year3 )
    
    
    # map data
    gvisGeoChart(h, locationvar = 'HospitalName',
                 sizevar='Count',
                 options=list(region='US-NY',displayMode='markers',
                              resolution="metros",
                              colorAxis="{values:[200,400,600,800],
                              colors:[\'red', \'pink\', \'orange',\'green']}",
                              magnifyingGlass="{enable: true, zoomFactor: 10.0}"
                 )
    )
    
  })
  
  ### tab 4
  
  hosp <- reactive ({
    
    hosp <- maternity %>%
      dplyr::filter(., Category == input$cat4) %>%
      dplyr::filter(., HospitalName == input$hospital)
    
  })
  
  
  output$hospitalHist = renderPlot({
    
    ggplot(hosp(),
           aes(x = Year, y = Count, group = Measure)) +
      geom_bar(aes(fill = Measure), stat  = 'identity', position = 'dodge') +
      scale_x_discrete('Year', limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
      theme(legend.position="bottom")
    
  })
  
  output$hospitalLine <- renderPlot({
    
    ggplot(hosp()) +
      geom_line(aes(x = Year, y = Count, group = Measure, color = Measure)) +
      scale_x_discrete('Year', limits=c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)) +
      theme(legend.position="bottom")
    
  })
  
}