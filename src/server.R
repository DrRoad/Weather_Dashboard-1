if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               DT,
               raster,
               lubridate)


source("functions.R")
source("declarations.R")

server <- function(input, output, session) {

    # ReactiveValues ----
    rv_sources <- reactiveValues(
        knmi=TRUE
    )


    df_raw <- reactive({
        # Import the raw data
        df_raw <- import_data()
        return(df_raw)
    })
    df <- reactive({
        # From the raw data, get only the time that we want to show in the dashboard.
        # This will be the main dataframe from now on
        df_raw <- df_raw()
        compared_time <- compared_time()

        # df_raw %>% head %>% print
        # compared_time %>% print
        df <- df_raw[df_raw$datetime == compared_time, ]
        # Convert to numeric if there is no warning. Station names will give a warning, therefore stay strings
        df <- lapply(df, function(x) tryCatch(x %>% as.numeric, warning =function(warning) x)) %>% data.frame
        return(df)
    })
    compared_time <- reactive({
        # Determine the interesting time we want to show. Is used to filter df_raw into df
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

#     output$dt1 <- DT::renderDataTable({
#         df_model_raster()
#         df()
#     })

    df_model_raster <- reactive({
        df <- df()
        if(nrow(df)==0) {return()}
        observable_gfs <- conversion_list_GFS[[input$observable]]
        df_model_raster <- raster_maker(df, observable_gfs)
        return(df_model_raster)
    })
    df_knmi <- reactive({
        df <- df()
        observable_knmi <- conversion_list_KNMI[[input$observable]]
        observable_gfs <- conversion_list_GFS[[input$observable]]
        df_knmi <- df[!df[[observable_knmi]] %>% is.na,
                      c(observable_knmi, observable_gfs, 'knmi_lat', 'knmi_lon', 'knmi_name')]
        df_knmi <- df_knmi[!duplicated(df_knmi), ]
        names(df_knmi)[c(1,2)] <- c('knmi', 'gfs')
        df_knmi$dif <- df_knmi$knmi %>% as.numeric - df_knmi$gfs

        return(df_knmi)
    })

    # colorpalettes, domains and other boring stuff ----
    domain_model <- reactive ({
        list("Windspeed"=c(0, 25),
             "Temperature"=c(-10, 35),
             "Air pressure"=c(950, 1050),
             "Radiation"=c(-150, 150))[[input$observable]]
    })
    cpalet_model_background <- reactive({
        options <- list("Windspeed"=colorNumeric(colorRampPalette(c("white", "white", "#66ffff", "#00ff99", "#00ff00", "#ffff00", "#ff9900", "#ff3300", "#ff0066", "#ff00ff"))(200),
                                                 domain = domain_model()),
                        "Temperature"=colorNumeric(palette = colorRampPalette(c("blue", "#66ffff", "white", "#ff9900", "red"))(200),
                                                   domain = domain_model()),
                        "Air pressure"=colorNumeric(palette = colorRampPalette(c("chartreuse", "lightpink", "aquamarine"))(200) ,
                                                    domain = domain_model()),
                        "Radiation"=colorBin(c("#3119b7", "#ffffff00", "yellow1"),
                                             domain=domain_model(),
                                             na.color="transparent",
                                             bins=11))
        options[[input$observable]]

    })
    domain_diff <- reactive({
        list("Windspeed"=c(-5, 5),
             "Temperature"=c(-6, 6),
             "Air pressure"=c(-5, 5),
             "Radiation"=c(-150, 150))[[input$observable]]

    })
    cpalet_circlemarkers <- reactive({
        options <- list("Windspeed"=colorNumeric(palette = colorRampPalette(c("red", "#ff9900", "white", "#d8ff00" ,"limegreen"))(200) ,
                                                 domain = domain_diff()),
                        "Temperature"=colorNumeric(palette = colorRampPalette(c("blue", "#66ffff", "white", "#ff9900", "red"))(200) ,
                                                   domain = domain_diff()),
                        "Air pressure"=colorNumeric(palette = colorRampPalette(c("red", "#ff9900", "white", "#d8ff00", "limegreen"))(200) ,
                                                    domain = domain_diff()),
                        "Radiation"=colorNumeric(palette = colorRampPalette(c("red", "#ff9900", "white", "#d8ff00" ,"limegreen"))(200) ,
                                                 domain = domain_diff()))
        options[[input$observable]]

    })



    # MAP ----
    output$map <- renderLeaflet({
        "Rendering Leaflet" %>% print

        a <- leaflet() %>%
            addTiles() %>%
            fitBounds(3.151613,53.670926,7.623049,50.719332)

        return(a)
    })

    observeEvent({df_model_raster()}, {
        df_model_raster <- df_model_raster()
        leafletProxy('map') %>%
            clearGroup('model') %>%
            # clearGroup('contourlines') %>%
            # clearGroup('HL') %>%
            addRasterImage(df_model_raster,
                           color = cpalet_model_background(),
                           opacity = 0.5,
                           group='model') %>%

            addLegend(pal = cpalet_model_background(),
                      values =domain_model() %>% rev,
                      title = input$observable,
                      layerId='model_legend') %>%
            addLegend(pal = cpalet_circlemarkers(),
                      values = domain_diff() %>% rev,
                      title = "Difference",
                      layerId='circlemarkers_legend')
    })
    observeEvent({df_knmi()}, {
        leafletProxy('map') %>%
        clearGroup('KNMI_markers')
        rv_sources$knmi %>% print

        if (rv_sources$knmi){
            print("Plotting KNMI markers")
            df_knmi <- df_knmi()

            df_knmi %>% head %>% print

            leafletProxy('map') %>%
                # clearGroup("KNMI_markers") %>%
                addCircleMarkers(lat = df_knmi$knmi_lat,
                                 lng = df_knmi$knmi_lon,
                                 radius = 8,
                                 weight = 1,
                                 popup=paste0("KNMI", "<br>",
                                              "stationname: ", df_knmi$knmi_name, "<br>",
                                              "KNMI:: ", df_knmi$knmi %>% round(2),"<br>",
                                              "Model: ", df_knmi$gfs %>% round(2)),
                                 fillColor = cpalet_circlemarkers()(df_knmi$dif),
                                 color='black',
                                 fillOpacity=1,
                                 opacity = 1,
                                 group='KNMI_markers')
        }

    })
}
