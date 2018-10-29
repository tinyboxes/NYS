# UI for Maternity Trends in NY State App

library(shiny)

ui <- navbarPage(title = 'New York State Maternity and Birth Trends',
                 navbarMenu('About Project', 
                            tabPanel(title = 'Introduction',
                                     fluid = TRUE,
                                     fluidRow(
                                       h3('Introduction and Notes on the Data Set'),
                                       leafletOutput('basicMap'),
                                       h2('Map should be above'),
                                       print('Yes Map')
                                     )
                            )
                 ), # navbarMenu
                 tabPanel( title = tagList(shiny::icon('map'), 'County Map'),
                           fluid = TRUE,
                           column(4,
                                  wellPanel(
                                    helpText('To view county trends in maternity health and births in 
                                             New York State begin by selecting year, category and metric.'),
                                    # Input: Dropdown Box to Select Year
                                    selectizeInput(inputId = 'year', label = 'Year',
                                                   choices = 2008:2016),
                                    # Input: Dropdown Box for Category
                                    selectizeInput(inputId='cat', label = 'Category',
                                                   choices = unique(maternity$Category)),
                                    # Add metrics input after selecting category
                                    uiOutput('metric')
                                    )
                           ),
                           column(8,
                                  fluidRow(
                                    h3('Maternity and Birth Trends By County'),
                                    leafletOutput('countyMetric'),
                                    h2('Map should be above'))
                           )
), # tab 1
tabPanel(title = tagList(shiny::icon('bar-chart'), 'by County'),
         fluid = TRUE,
         sidebarLayout(
           # sidebar panel to select inputs
           sidebarPanel(
             helpText('To view county trends in maternity health and births in New York State 
                      begin by selecting a year and category.'),
             # Input: Dropdown Box to Select Year
             selectizeInput(inputId = 'year2', label = 'Year',
                            choices = 2008:2016),
             # Input: Dropdown Box for Category
             selectizeInput(inputId='cat2', label = 'Category',
                            choices = unique(maternity$Category)),
             # Add metrics input after selecting category
             uiOutput('metric2')
             ),
           mainPanel(
             fluidRow(
               plotOutput('countyPlot')))
         )
                 ), # tab panel 2
tabPanel(title = tagList(shiny::icon('map'), 'Hospital Map'),
         fluid = TRUE,
         sidebarLayout(
           # sidebar panel to select inputs
           sidebarPanel(
             helpText('To view hospital trends in maternity health and births in New York State 
                      begin by selecting a year and category.'),
             # Input: Dropdown Box for Category
             selectizeInput(inputId = 'year3', label = 'Year',
                            choices = 2008:2016),
             selectizeInput(inputId='cat3', label = 'Category',
                            choices = unique(maternity$Category),
                            selected = 'Route & Method'),
             uiOutput('metric3')),
           mainPanel(
             fluidRow(
               htmlOutput('hospitalMap')
             ))
           )
), # tab panel 3
tabPanel(title = tagList(shiny::icon('bar-chart'), 'by Hospital'),
         fluid = TRUE,
         sidebarLayout(
           # sidebar panel to select inputs
           sidebarPanel(
             helpText('To view hospital trends in maternity health and births in New York State 
                      hospital, begin by selecting category, county and hospital.'),
             # Input: Dropdown Box for Category
             selectizeInput(inputId='cat4', label = 'Category',
                            choices = unique(maternity$Category),
                            selected = 'Route & Method'),
             # Input: Dropdown Box for County
             selectizeInput(inputId='county4', label = 'County',
                            choices = unique(maternity$County),
                            selected = 'Kings'),
             uiOutput('hospital')
             ),
           mainPanel(
             column(5,
                    plotOutput('hospitalHist')
             ),
             column(5,
                    plotOutput('hospitalLine')
             )
           )
         )
) # last tab
) # end navbarPage
