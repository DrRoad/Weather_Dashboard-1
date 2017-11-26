if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               DT,
               raster,
               lubridate)


source("functions.R")
source("declarations.R")

server <- function(input, output, session) {

    df_raw <- reactive({
        df_raw <- import_data()
        return(df_raw)
    })
    df <- reactive({
        df_raw <- df_raw()
        compared_time <- compared_time()

        df_raw %>% head %>% print
        compared_time %>% print
        df <- df_raw[df_raw$datetime == compared_time, ]
        return(df)
    })

    compared_time <- reactive({
        # Depending on the switch, get the current time or the tietime the trader wants
        if(input$current_time) {
            # Get the current time, truncate it to hours so that we have the current hour
            compared_time <- Sys.time() %>%
                trunc('hour')
        } else {
            # Get time wanted by user by combining date and time.
            # Force to Localtime (assuming trader is located/thinking in Europe/Amsterdam time)
            # Convert to UTC to compare in dataframe coming from Datahub (which is in UTC)

            compared_time <- (input$date_input %>% as.POSIXct + input$hour_input * 60 * 60) %>%
                force_tz('Europe/Amsterdam') %>%
                with_tz('UTC')
        }
        return(compared_time)
    })

    output$dt1 <- DT::renderDataTable({
        df_model()
        df()
    })

    df_model <- reactive({
        df_model <- df()
        conversion_list_GFS[[input$variable]] %>% print
        df_model <- raster_maker(df_model, conversion_list_GFS[[input$variable]])
        return(df_model)
    })


    # MAP ----
    output$map <- renderLeaflet({
        "Rendering Leaflet" %>% print

        a <- leaflet() %>%
            addTiles() %>%
            fitBounds(3.151613,53.670926,7.623049,50.719332)

        return(a)
    })

    observeEvent({residual_grid_f(); input$model}, {
        leafletProxy('map') %>%
            clearGroup('model') %>%
            # clearGroup('contourlines') %>%
            # clearGroup('HL') %>%
            addRasterImage(df_model(),
                           color = cpalet_HIRLAM_background(),
                           opacity = 0.5,
                           group='model') %>%

            addLegend(pal = cpalet_HIRLAM_background(),
                      values =domain_HIRLAM() %>% rev,
                      title = input$observable,
                      layerId='model_legend') %>%
            addLegend(pal = cpalet_circlemarkers(),
                      values = domain_diff() %>% rev,
                      title = "Difference",
                      layerId='circlemarkers_legend')
    })
}
