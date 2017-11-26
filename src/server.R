if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               DT,
               raster)


source("functions.R")
source("declarations.R")

server <- function(input, output, session) {

    df <- reactive({
        df <- import_data()
        return(df)
    })

    output$dt1 <- DT::renderDataTable({
        df()
    })

    df_model <- reactive({
        df
    })


    # MAP ----
    output$map <- renderLeaflet({
        "Rendering Leaflet" %>% print

        a <- leaflet() %>%
            addTiles() %>%
            fitBounds(3.151613,53.670926,7.623049,50.719332)

        return(a)
    })
}
