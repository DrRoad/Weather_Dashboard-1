if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               DT,
               raster,
               lubridate,
               RMySQL)


source("functions.R")
source("declarations.R")

server <- function(input, output, session) {

    # Dataframes build up ----
    df_raw_sql <- reactive({
        df_raw_sql <- import_data_sql()
        return(df_raw_sql)
    })
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

    # Dataframes to be used in the dashboard ----
    df_model_raster <- reactive({
        df <- df()
        if(nrow(df)==0) {print("Nothing");return()}
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
        names(df_knmi)[c(1, 2)] <- c('knmi', 'gfs')
        df_knmi$dif <- df_knmi$knmi %>% as.numeric - df_knmi$gfs

        return(df_knmi)
    })
    df_owm <- reactive({
        df <- df()
        observable_owm <- conversion_list_OWM[[input$observable]]
        observable_gfs <- conversion_list_GFS[[input$observable]]
        df_owm <- df[!df[[observable_owm]] %>% is.na,
                     c(observable_owm, observable_gfs, 'owm_lat', 'owm_lon', 'owm_name')]
        df_owm <- df_owm[!duplicated(df_owm), ]
        names(df_owm)[c(1, 2)] <- c('owm', 'gfs')
        df_owm$dif <- df_owm$owm %>% as.numeric - df_owm$gfs

        return(df_owm)
    })
    df_metoffice <- reactive({
        df <- df()
        observable_metoffice <- conversion_list_MetOffice[[input$observable]]
        observable_gfs <- conversion_list_GFS[[input$observable]]
        df_metoffice <- df[!df[[observable_metoffice]] %>% is.na,
                     c(observable_metoffice, observable_gfs, 'metoffice_lat', 'metoffice_lon', 'metoffice_name')]
        df_metoffice <- df_metoffice[!duplicated(df_metoffice), ]
        names(df_metoffice)[c(1, 2)] <- c('metoffice', 'gfs')
        df_metoffice$dif <- df_metoffice$metoffice %>% as.numeric - df_metoffice$gfs

        return(df_metoffice)
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
                           color=cpalet_model_background(),
                           opacity=0.5,
                           group='model') %>%
            addLegend(pal=cpalet_model_background(),
                      values=domain_model() %>% rev,
                      title=input$observable,
                      layerId='model_legend') %>%
            addLegend(pal=cpalet_circlemarkers(),
                      values=domain_diff() %>% rev,
                      title="Difference",
                      layerId='circlemarkers_legend')
    })
    observeEvent({df_knmi(); input$knmi_switch}, {
        leafletProxy('map') %>%
            clearGroup('KNMI_markers')

        if (!input$knmi_switch) {
            # No need to do anything else
            return()
        }
        print("Plotting KNMI markers")
        df_knmi <- df_knmi()

        leafletProxy('map') %>%
            # clearGroup("KNMI_markers") %>%
            addCircleMarkers(lat=df_knmi$knmi_lat,
                             lng=df_knmi$knmi_lon,
                             radius=8,
                             weight=1,
                             popup=paste0("KNMI", "<br>",
                                          "stationname: ", df_knmi$knmi_name, "<br>",
                                          "KNMI:: ", df_knmi$knmi %>% round(2),"<br>",
                                          "Model: ", df_knmi$gfs %>% round(2)),
                             fillColor=suppressWarnings(cpalet_circlemarkers()(df_knmi$dif)),
                             color='black',
                             fillOpacity=1,
                             opacity=1,
                             group='KNMI_markers')
    })
    observeEvent({df_owm(); input$owm_switch}, {
        leafletProxy('map') %>%
            clearGroup('OWM_markers')

        if (!input$owm_switch) {
            # No need to do anything else
            return()
        }
        print("Plotting OWM markers")
        df_owm <- df_owm()

        leafletProxy('map') %>%
            # clearGroup("KNMI_markers") %>%
            addCircleMarkers(lat=df_owm$owm_lat,
                             lng=df_owm$owm_lon,
                             radius=8,
                             weight=1,
                             popup=paste0("OWM", "<br>",
                                          "stationname: ", df_owm$owm_name, "<br>",
                                          "OWM: ", df_owm$owm %>% round(2),"<br>",
                                          "Model: ", df_owm$gfs %>% round(2)),
                             fillColor=suppressWarnings(cpalet_circlemarkers()(df_owm$dif)),
                             color='orange',
                             fillOpacity=1,
                             opacity=1,
                             group='OWM_markers')
    })
    observeEvent({df_metoffice(); input$metoffice_switch}, {
        leafletProxy('map') %>%
            clearGroup('MetOffice_markers')

        if (!input$metoffice_switch) {
            # No need to do anything else
            return()
        }
        print("Plotting MetOffice markers")
        df_metoffice <- df_metoffice()

        leafletProxy('map') %>%
            # clearGroup("KNMI_markers") %>%
            addCircleMarkers(lat=df_metoffice$metoffice_lat,
                             lng=df_metoffice$metoffice_lon,
                             radius=8,
                             weight=1,
                             popup=paste0("MetOffice", "<br>",
                                          "stationname: ", df_metoffice$metoffice_name, "<br>",
                                          "MetOffice:: ", df_metoffice$metoffice %>% round(2),"<br>",
                                          "Model: ", df_metoffice$gfs %>% round(2)),
                             fillColor=suppressWarnings(cpalet_circlemarkers()(df_metoffice$dif)),
                             color='white',
                             fillOpacity=1,
                             opacity=1,
                             group='MetOffice_markers')
    })
    observeEvent({input$windparks_Eneco}, {
        leafletProxy('map') %>%
            clearGroup('eneco_windparks')
        if (!input$windparks_Eneco) {
            # Done here!
            return()
        }

        icons_size <- icons(
            iconUrl=windparkiconurl,
            iconHeight = 20 * Windparks$max_MW / Windparks$max_MW %>% max + 20,
            iconWidth = 20 * Windparks$max_MW / Windparks$max_MW %>% max + 20
        )
        leafletProxy('map') %>%
            addMarkers(lat = Windparks$lat,
                       lng = Windparks$lon,
                       icon = icons_size,
                       popup=paste0(Windparks$Location, "<br>",
                                    "capacity: ", Windparks$max_MW," MW<br>"),
                       group="eneco_windparks")

    })
    observeEvent({input$windparks_External}, {
        leafletProxy('map') %>%
            clearGroup('external_windparks')
        if (!input$windparks_External) {
            # Done here!
            return()
        }

        icons_size <- icons(
            iconUrl=windparkiconurl_grey,
            iconHeight = 20 * external_windparks$max_MW / external_windparks$max_MW %>% max + 20,
            iconWidth = 20 * external_windparks$max_MW / external_windparks$max_MW %>% max + 20
        )
        leafletProxy('map') %>%
            addMarkers(lat = external_windparks$lat,
                       lng = external_windparks$lon,
                       icon = icons_size,
                       popup=paste0(external_windparks$PARK, "<br>",
                                    "Owner: ", external_windparks$OWNER, "<br>",
                                    "capacity: ", external_windparks$max_MW," MW<br>"),
                       group="external_windparks")

    })
}
