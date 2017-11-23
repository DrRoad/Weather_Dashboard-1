if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               DT)


source("functions.R")
source("declarations.R")

server <- function(input, output, session) {

    df <- reactive({
        return(import_data())
    })

    output$dt1 <- DT::renderDataTable({
        df()
    })

}
