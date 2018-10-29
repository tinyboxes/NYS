# UI for Maternity Trends in NY State App

library(shiny)

ui <- navbarPage(title = 'New York State Maternity and Birth Trends',
                 navbarMenu('About Project', 
                            tabPanel(title = 'Introduction',
                                     fluid = TRUE,
                                     fluidRow(
                                      column(12,
                                             includeMarkdown('Intro.md'))
                                      ),
                                     fluidRow(
                                       br(),
                                       column(3,
                                              br(),
                                              p('Exlpore the map of New York State. 
                                                Familiarize yourself with the different state counties.'),
                                              helpText('Note: Population information is from 2010 census data.')),
                                       column(9,
                                              leafletOutput('basicMap'))
                                     )
                            ),
                            tabPanel(title = 'About the App',
                                     fluidRow(
                                       column(12,
                                              includeMarkdown('About.md'))
                                     )
                            )
                 ), # navbarMenu
                 tabPanel( title = tagList(shiny::icon('map'), 'County Map'),
                           fluid = TRUE,
                           column(3,
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
                           column(9,
                                  fluidRow(
                                    h1('Maternity and Birth Trends By County'),
                                    leafletOutput('countyMetric'),
                                    p('Explore the map and see the different metric values for each county.'))
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
                                h1('Maternity and Birth Trends By County'),
                                plotOutput('countyPlot'),
                                p('The chart above shows how a metric varies by county in a given year.'),
                                p('Note: As you change each metric, the scale on y-axis changes. '))
                             )
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
                                h1('Maternity and Birth Trends By Hospital'),
                                uiOutput('hospitalMap'),
                                p('Explore specific hospital by clicking on a marker.'))
                              )
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
                               fluidRow(
                                 h1('Maternity and Birth Trends By County'),
                                 column(5,
                                        plotOutput('hospitalHist')
                                 ),
                                 column(5,
                                        plotOutput('hospitalLine'))
                                ),
                               fluidRow(
                                 p('Plots above show how each metric varies over the years for each hospital.'))
                               )
                             )
                           ) # last tab
                 ) # end navbarPage
