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
                        conditionalPanel(condition="input.conditionedPanels==1",

                                         # box(title="time relative to now",
                                         helpText("Unos"),
                                         checkboxGroupInput("sources",
                                                            NULL,
                                                            list("KNMI"="knmi_source",
                                                                 "OWM"="owm_source")),
                                         fluidRow(
                                             column(8,
                                                    offset=0,
                                                    style="padding:6px;",
                                                    div(style="height: 30px;",
                                                        dateInput("date_input",
                                                                  NULL,
                                                                  value="2017-11-24")
                                                    ),
                                                    div(style="height: 30px;",
                                                        numericInput("hour_input",
                                                                     NULL,
                                                                     value=7)
                                                    )
                                             ),
                                             column(4,
                                                    offset=0,
                                                    style="padding:0px;",
                                                    materialSwitch('current_time',
                                                                   'Live?',
                                                                   value=TRUE,
                                                                   status='success')
                                             )
                                         ),
                                         pickerInput('observable',
                                                     'Observable',
                                                     choices=c('Temperature',
                                                               'Windspeed',
                                                               'Air Pressure',
                                                               'Radiation'),
                                                     choicesOpt = list(icon = c("glyphicon-cog",
                                                                                "glyphicon-play",
                                                                                "glyphicon-ok-sign",
                                                                                "glyphicon-arrow-rig")),
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
                                     helpText("Test"),
                                     DT::dataTableOutput('dt1'),
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
