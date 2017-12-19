if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(shiny,
               leaflet,
               shinydashboard,
               shinyWidgets,
               lubridate)



ui <- dashboardPage(title="Weather Dashboard",

                    dashboardHeader(title= "Weather Dashboard 2.0"),
                    dashboardSidebar(
                        includeCSS("styles.css"),
                        conditionalPanel(
                            condition="input.conditionedPanels==1",
                            fluidRow(
                                div(
                                    style='height: 25px;',
                                    switchInput('knmi_switch',
                                                label='KNMI',
                                                size='small',
                                                value=TRUE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                    style='height: 25px;',
                                    switchInput('owm_switch',
                                                label='OWM',
                                                size='small',
                                                value=FALSE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                    style='height: 25px;',
                                    switchInput('metoffice_switch',
                                                label='MetOffice',
                                                size='small',
                                                value=TRUE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                    style='height: 25px;',
                                    switchInput('wind_rt',
                                                label='Wind RT',
                                                size='small',
                                                value=TRUE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                    style='height: 25px;',
                                    switchInput('windparks_Eneco',
                                                label='Wind Eneco',
                                                size='small',
                                                value=TRUE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                    style='height: 25px;',
                                    switchInput('windparks_External',
                                                label='Wind Other',
                                                size='small',
                                                value=FALSE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                    style='height: 25px;',
                                    switchInput('wind_direction',
                                                label='Direction',
                                                size='small',
                                                value=FALSE,
                                                labelWidth='60px',
                                                handleWidth='30px')
                                ),
                                div(
                                  style='height: 25px;',
                                  switchInput('Meteosat_clouds',
                                              label='Clouds',
                                              size='small',
                                              value=FALSE,
                                              labelWidth='60px',
                                              handleWidth='30px')
                                ),
                                div(
                                  style='height: 25px;',
                                  switchInput('Meteosat_rain',
                                              label='Rain',
                                              size='small',
                                              value=FALSE,
                                              labelWidth='60px',
                                              handleWidth='30px')
                                )
                            ),
                            HTML("<br/>"),
                            style="padding: 0px;",
                            actionButton('refresh_data',
                                         "Refresh"),
                            fluidRow(
                                column(7,
                                       offset=0,
                                       style="padding:6px;",
                                       div(style="height: 33px;",
                                           dateInput("date_input",
                                                     NULL,
                                                     value=Sys.Date())
                                       ),
                                       div(style="height: 33px;",
                                           numericInput("hour_input",
                                                        NULL,
                                                        value=Sys.time() %>% with_tz('Europe/Amsterdam') %>% hour)
                                       )
                                ),
                                column(5,
                                       offset=0,
                                       style="padding:10px;",
                                       div(style="height: 51px;",
                                           materialSwitch('current_time',
                                                          'Live?',
                                                          value=TRUE,
                                                          status='success')
                                       ),
                                       div(style="height: 20px;",
                                           textOutput("compared_time")
                                       )
                                )
                            ),
							pickerInput('model',
                                    label='Model',
                                    choices = c('GFS',
                                                'HIRLAM'),
                                    selected = 'GFS'),
                            pickerInput('observable',
                                        label='Observable',
                                        choices=c('Temperature',
                                                  'Windspeed',
                                                  'Air pressure',
                                                  'Radiation'),
                                        choicesOpt = list(icon = c("glyphicon glyphicon-copyright-mark",
                                                                   "glyphicon-flag",
                                                                   "glyphicon glyphicon-dashboard",
                                                                   "glyphicon-certificate")),
                                        selected='Temperature')


                        ),
                        conditionalPanel(condition="input.conditionedPanels==2",
                                         helpText("Dos")

                        ),
                        conditionalPanel(condition="input.conditionedPanels==3",
                                         helpText("Tres!")
                        )
                    ),
                    dashboardBody({

                        tabsetPanel(
                            id = "conditionedPanels",
                            tabPanel('One',
                                     value=1,
                                     style="padding=0px;",
                                     fluidRow(
                                         column(9,
                                                offset=0,
                                                style='padding: 0px;',
                                                leafletOutput("map",
                                                              height=850,
                                                              width="100%")
                                         ),
                                         column(3,
                                                offset=0,
                                                style='padding: 0px;',
                                                align='center',
                                                box(title='IGCC',
                                                    solidHeader=FALSE,
                                                    collapsible=TRUE,
                                                    color='lime',
                                                    width=12,
                                                    heigth=300,
                                                    fluidRow(
                                                        plotOutput('igcc_plot',
                                                                   height="250",
                                                                   width="90%")
                                                    )
                                                ),
                                                box(title='KNMI history',
                                                    solidHeader=FALSE,
                                                    collapsible=TRUE,
                                                    collapsed=TRUE,
                                                    color='black',
                                                    width=12,
                                                    heigth=300,
                                                    fluidRow(
                                                        plotOutput('knmi_history_plot',
                                                                   height="250",
                                                                   width="90%")
                                                    )
                                                ),
                                                box(title='MetOffice history',
                                                    solidHeader=FALSE,
                                                    collapsible=TRUE,
                                                    collapsed=TRUE,
                                                    color='orange',
                                                    width=12,
                                                    heigth=300,
                                                    fluidRow(
                                                        plotOutput('metoffice_history_plot',
                                                                   height="250",
                                                                   width="90%")
                                                    )
                                                )
                                         )
                                     )
                            ),
                            tabPanel('Two',
                                     value=2,
                                     helpText("Test2")
                            ),
                            tabPanel('Three',
                                     value=3,
                                     helpText("Test3")
                            )
                        )
                    })
)
