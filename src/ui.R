if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load(shiny,
               leaflet,
               shinydashboard,
               DT)



ui <- dashboardPage(title="Weather Dashboard",
                    dashboardHeader(title= "Weather Dashboard 2.0"),
                    dashboardSidebar(
                        conditionalPanel(condition="input.conditionedPanels==1",

                                         # box(title="time relative to now",
                                         helpText("Unos")

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
                                     DT::dataTableOutput('dt1')
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
