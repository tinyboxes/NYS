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
                                       column(4,
                                              br(),
                                              column(3,
                                                img(src = './NYS.jpg', style='width:110%; border:0px;')),
                                              p('Exlpore the map of New York State. 
                                                Familiarize yourself with the different state counties.'),
                                              helpText('Note: Population information is from 2010 census data.'),
                                              p('Data from: '),
                                              img(src = './health.jpeg', style='width:90%; border:0px'),
                                              br(),
                                              br(),
                                              img(src = './its.png', style='width:100%; border:0px')
                                              ),
                                       column(8,
                                              leafletOutput('basicMap')
                                              ))
                            ), # nav tab 1
                            tabPanel(title = 'About the App',
                                     fluidRow(
                                       column(12,
                                              includeMarkdown('About.md'))
                                     )
                            ) # nav tab 2
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
                                                   choices = unique(countyMaternity$Category)
                                                   ),
                                    # Add metrics input after selecting category
                                    uiOutput('metric'),
                                    br(),
                                    htmlOutput('text')
                                    )
                                  ),
                           column(9,
                                  fluidRow(
                                    column(9,
                                           p('Trends in New York State by Metric'),
                                           leafletOutput('countyMap'))
                                  ),
                                  fluidRow(
                                    column(9, 
                                         p('County Numbers vs State Average'),
                                         plotOutput('countyPlot'))
                                  ),
                                  fluidRow(
                                    p('Explore the map and see the different metric values for each county.'),
                                    p('The chart above shows how a metric varies by county in a given year.')
                                    )
                           )
                 ), # tab 1
                 tabPanel(title = tagList(shiny::icon('bar-chart'), 'by County'),
                          fluid = TRUE,
                          sidebarLayout(
                            # sidebar panel to select inputs
                            sidebarPanel(
                              helpText('To view the top or bottom ten counties for each metric and year
                                        begin by selecting a year and category.'),
                              # Input: Dropdown Box to Select Year
                              selectizeInput(inputId = 'year2', label = 'Year',
                                            choices = 2008:2016,
                                            selected = 2010),
                              # Input: Dropdown Box for Category
                              selectizeInput(inputId='cat2', label = 'Category',
                                            choices = unique(countyMaternity$Category),
                                            select = 'Route & Method'),
                              # Add metrics input after selecting category
                              uiOutput('metric2'),
                              # selectizeInput(inputId='county2', label = 'County',
                              #                choices = unique(countyMaternity$County)
                              #                ),
                              radioButtons(inputId = 'top', label = 'Top or Bottom 10', 
                                           choices = c('Top', 'Bottom'))
                              ),
                            mainPanel(
                                p('Comparison of County Total vs State Average'),
                                plotOutput('state'),
                                p('Note: As you change each metric, the scale on y-axis changes. Blue line indicate State Average for the year.')
                                )
                            )
                 ), # tab panel 2
                 tabPanel(title = tagList(shiny::icon('bar-chart'), '2010: Metric Comparison'),
                          fluid = TRUE,
                          sidebarLayout(
                            # sidebar panel to select inputs
                            sidebarPanel(
                              p('This is a scatter plot analysis of 2010 data.'),
                              p('See if there are any correlations between metrics.'),
                              p('Select two measures from the dropdown boxes.'),
                              # Input: Dropdown Box to Select Year
                              uiOutput('m1'),
                              # Add metrics input after selecting category
                              uiOutput('m2')
                              ),
                            mainPanel(
                              p('Comparison of 2010 Metric Data'),
                              plotOutput('scatterPlot')
                              )
                          )
                ), # tab panel 2.1
                tabPanel(title = tagList(shiny::icon('map'), 'Hospital Map'),
                          fluid = TRUE,
                          # sidebar panel to select inputs
                          column(3,
                                  p('To view hospital trends in maternity health and births in New York State 
                                           begin by selecting a year and category.'),
                                  # Input: Dropdown Box for Category
                                  selectizeInput(inputId = 'year3', label = 'Year',
                                                choices = 2008:2016),
                                  selectizeInput(inputId='cat3', label = 'Category',
                                                choices = unique(countyMaternity$Category),
                                                selected = 'All Deliveries'),
                                  uiOutput('metric3'),
                                  br(),
                                  htmlOutput('text2')
                                ),
                          column(9,
                                 p('Maternity and Birth Trends By Hospital'),
                                 htmlOutput('hospitalMap'),
                                 p('Explore specific hospital by clicking on a marker.')
                                 )
                  ), # tab panel 3
                  tabPanel(title = tagList(shiny::icon('bar-chart'), 'by Hospital'),
                           fluid = TRUE,
                           sidebarLayout(
                             # sidebar panel to select inputs
                             sidebarPanel(
                               helpText('To view hospital trends in maternity health and births in New York State 
                                        hospital, begin by selecting category, county and hospital.'),
                               # selectizeInput(inputId='year4', label = 'Year',
                               #                choices = 2008:2016
                               #                ),
                               # Input: Dropdown Box for Category
                               selectizeInput(inputId='cat4', label = 'Category',
                                              choices = unique(countyMaternity$Category),
                                              selected = 'All Deliveries'
                                              ),
                               uiOutput('metric4'),
                               # Input: Dropdown Box for Hospitals
                               uiOutput('hosp1'),
                               uiOutput('hosp2')
                               ),
                             mainPanel(
                               fluidRow(
                                p('Comparing Hospital Trends'),
                                  column(12,
                                        plotOutput('hospitalLine'))
                                ),
                               fluidRow(
                                 p('Plot above show how each metric varies over the years for each hospital.'),
                                 p('For comparison, the state average is plotted as well.'))
                               )
                             )
                  ) # last tab
) # end navbarPage
