if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(shiny,
               leaflet,
               shinydashboard,
               DT,
               shinyWidgets)



ui <- dashboardPage(title="Weather Dashboard",
                    dashboardHeader(title= "Weather Dashboard 2.0"),
                    dashboardSidebar(
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
                                                value=TRUE,
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
                                )
                            ),
                            HTML("<br/>"),
                            fluidRow(
                                column(8,
                                       offset=0,
                                       style="padding:6px;",
                                       div(style="height: 33px;",
                                           dateInput("date_input",
                                                     NULL,
                                                     value="2017-11-24")
                                       ),
                                       div(style="height: 33px;",
                                           numericInput("hour_input",
                                                        NULL,
                                                        value=7)
                                       )
                                ),
                                column(4,
                                       offset=0,
                                       style="padding:10px;",
                                       materialSwitch('current_time',
                                                      'Live?',
                                                      value=FALSE,
                                                      status='success')
                                )
                            ),
                            pickerInput('observable',
                                        'Observable',
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
                                     # DT::dataTableOutput('dt1'),
                                     style="padding=0px;",
                                     leafletOutput("map",
                                                   height=850,
                                                   width="100%")
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
