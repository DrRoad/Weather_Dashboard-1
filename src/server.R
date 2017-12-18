if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               leaflet,
               raster,
               lubridate,
               RMySQL,
               ggplot2,
               scales,
               stringr,
               reshape2,
               magrittr)


source("functions.R")
source("declarations.R")

rv <- reactiveValues(knmi_station_history = NULL,
                     metoffice_station_history=NULL)

server <- function(input, output, session) {
    # autoInvalidates ----
    autoInvalidate_data_fetch_sql <- reactiveTimer(5 * 60 * 1000, session)
    autoInvalidate_IGCC <- reactiveTimer(4 * 60 * 1000, session)
    autoinvalidate_MeteoSat <- reactivetimer(3 * 60 * 1000, session)
    # Dataframes build up ----
    df_raw_sql <- reactive({
        # To update every x minutes, there is this autoInvalidate
        autoInvalidate_data_fetch_sql()
        input$refresh_data

        df_raw_sql <- withProgress(
            # This part takes care of showing the notifcation when data is fetched
            message='Importing data from DataHub',
            detail='Kind regards, Mathias',
            value=NULL,
            style='old',
            {
                # The actual data fetching
                import_data_sql(model=input$Model)
            })
        return(df_raw_sql)
    })
	Meteosat_sql <- reactive({
        # To update every x minutes, there is this autoInvalidate
	    autoinvalidate_MeteoSat()
        input$refresh_data

        Meteosat_sql <- withProgress(
            # This part takes care of showing the notifcation when data is fetched
            message='Importing MeteoSat data from DataHub',
            detail='Kind regards, Luuk',
            value=NULL,
            style='old',
            {
                # The actual data fetching
                import_data_sql(model=input$Model)
            })
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
        df_raw <- df_raw_sql()
        compared_time <- compared_time()

        # df_raw %>% head %>% print
        # compared_time %>% print
        df <- df_raw[df_raw$datetime == compared_time, ]
        # Convert to numeric if there is no warning. Station names will give a warning, therefore stay strings
        df <- lapply(df, function(x) tryCatch(x %>% as.numeric, warning =function(warning) x)) %>% data.frame
        return(df)
    })
    compared_time <- reactive({
        # Get this into the autorefresh, so that the time will be updated when you are leaving it in live modus
        autoInvalidate_data_fetch_sql()
        # Determine the interesting time we want to show. Is used to filter df_raw into df
        # Depending on the switch, get the current time or the tietime the trader wants
        if(input$current_time) {
            # Get the current time, truncate it to hours so that we have the current hour
            compared_time <- Sys.time() %>%
                with_tz('UTC') %>%
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

	model_observable <- reactive({
		if (input$Model == 'GFS') {
			return(conversion_list_GFS[[input$observable]])}
		if (input$Model == 'HIRLAM') {
			return(conversion_list_HIRLAM[[input$observable]])}
	})
    # Dataframes to be used in the dashboard ----
    df_model_raster <- reactive({
        df <- df()
        if(nrow(df)==0) {print("Nothing in raw df");return(data.frame())}
        observable_fc <- model_observable()
        df_model_raster <- raster_maker(df, observable_fc)
        return(df_model_raster)
    })
    df_knmi <- reactive({
        df <- df()
        observable_knmi <- conversion_list_KNMI[[input$observable]]
        observable_fc <- model_observable()
        df_knmi <- df[!df[[observable_knmi]] %>% is.na,
                      c(observable_knmi, observable_fc, 'knmi_lat', 'knmi_lon', 'knmi_name')]
        df_knmi <- df_knmi[!duplicated(df_knmi), ]
        names(df_knmi)[c(1, 2)] <- c('knmi', 'model')
        df_knmi$dif <- df_knmi$knmi %>% as.numeric - df_knmi$model

        return(df_knmi)
    })
    df_owm <- reactive({
        df <- df()
        observable_owm <- conversion_list_OWM[[input$observable]]
        observable_fc <- model_observable()
        df_owm <- df[!df[[observable_owm]] %>% is.na,
                     c(observable_owm, observable_fc, 'owm_lat', 'owm_lon', 'owm_name')]
        df_owm <- df_owm[!duplicated(df_owm), ]
        names(df_owm)[c(1, 2)] <- c('owm', 'model')
        df_owm$dif <- df_owm$owm %>% as.numeric - df_owm$model

        return(df_owm)
    })
    df_metoffice <- reactive({
        df <- df()
        observable_metoffice <- conversion_list_MetOffice[[input$observable]]
        observable_fc <- model_observable()
        df_metoffice <- df[!df[[observable_metoffice]] %>% is.na,
                     c(observable_metoffice, observable_fc, 'metoffice_lat', 'metoffice_lon', 'metoffice_name')]
        df_metoffice <- df_metoffice[!duplicated(df_metoffice), ]
        names(df_metoffice)[c(1, 2)] <- c('metoffice', 'model')
        df_metoffice$dif <- df_metoffice$metoffice %>% as.numeric - df_metoffice$model

        return(df_metoffice)
    })
	df_MeteoSat_raw <- reactive({
		autoInvalidate_data_fetch_sql()
        input$refresh_data
		if (!input$Meteosat_rain & !input$Meteosat_clouds){return(data.frame())}
		Meteosat_raw <- Meteosat_sql()
		return(Meteosat_raw)
	})
	df_Meteosat_clouds <- reactive({
		if (!input$Meteosat_clouds){return(data.frame())}
        df_MeteoSat_raw <- df_MeteoSat_raw()
        frame.MeteoSat = cbind.data.frame(df_MeteoSat_raw$lon, df_MeteoSat_raw$lat)
        coordinates(frame.MeteoSat) <- ~df_MeteoSat_raw$lon + df_MeteoSat_raw$lat
        df_Meteosat_cot_raster <- raster_maker(frame.MeteoSat, df_MeteoSat_raw[, 'cot'])
		return(df_Meteosat_cot_raster)
    })
    df_Meteosat_rain <- reactive({
        if (!input$Meteosat_rain){return(data.frame())}
        df_MeteoSat_raw <- df_MeteoSat_raw()
        frame.MeteoSat = cbind.data.frame(df_MeteoSat_raw$lon, df_MeteoSat_raw$lat)
        coordinates(frame.MeteoSat) <- ~df_MeteoSat_raw$lon + df_MeteoSat_raw$lat
        df_Meteosat_precip_raster <- raster_maker(frame.MeteoSat, df_MeteoSat_raw[, 'precip'])
		return(df_Meteosat_precip_raster)
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
            clearGroup('model')
        if(df_model_raster %>% typeof == 'list') {
            # Returned empty from function before
            return()
        }
        leafletProxy('map') %>%
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

	observeEvent({df_Meteosat_rain(); input$Meteosat_rain}, {
        df_Meteosat_rain <- df_Meteosat_rain()
        leafletProxy('map') %>%
            clearGroup('MeteoSat_precip')
        if(df_Meteosat_rain %>% typeof == 'list') {
            # Returned empty from function before
            return()
        }
        leafletProxy('map') %>%
            # clearGroup('contourlines') %>%
            # clearGroup('HL') %>%
            addRasterImage(log10(frame_MeteoSat_precip()),
                               color=colors_MeteoSat_precip,
                               opacity=0.45,
                               group='MeteoSat_precip')
    })
	observeEvent({df_Meteosat_clouds(); input$Meteosat_clouds}, {
        df_Meteosat_clouds <- df_Meteosat_clouds()
        leafletProxy('map') %>%
            clearGroup('MeteoSat_cot')
        if(df_Meteosat_clouds %>% typeof == 'list') {
            # Returned empty from function before
            return()
        }
        leafletProxy('map') %>%
            # clearGroup('contourlines') %>%
            # clearGroup('HL') %>%
            addRasterImage(log10(frame_MeteoSat_cot()),
                               color=colors_MeteoSat_cot,
                               opacity=0.45,
                               group='MeteoSat_cot')
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
                                          "Model: ", df_knmi$model %>% round(2)),
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
                                          "Model: ", df_owm$model %>% round(2)),
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
                                          "Model: ", df_metoffice$model %>% round(2)),
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
                                    "capacity: ", Windparks$max_MW," MW<br>",
                                    plot(mtcars)),
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
    observeEvent({input$wind_direction}, {
        leafletProxy('map') %>%
            clearGroup('wind_direction')
        if (!input$wind_direction) {
            # Done here!
            return()
        }
        df <- df()
        df_wind <- df[df$lat %% 1 ==0 & df$lon %% 1 == 0, ]
        icon <- icons(wind_directions_location[if (input$Model == 'GFS'){df_wind$gfs_wind_direction}
											   else if (input$Model == 'HIRLAM'){df_wind$hirlam_wind_direction} %>%
                                                   divide_by(22.5) %>%
                                                   round(0) %>%
                                                   multiply_by(22.5) %>%
                                                   round(0) %% 360 %>%
                                                   as.character],
                      iconHeight=30,
                      iconWidth=30)
        leafletProxy('map') %>%
            addMarkers(lng=df_wind$lon,
                       lat=df_wind$lat,
                       icon=icon,
                       popup=NULL,
                       group='wind_direction')


    })

    observeEvent({input$map_marker_click}, {
        click <- input$map_marker_click
        groups_that_can_click <- c('KNMI_markers', 'MetOffice_markers')
        if(is.null(click) | !click$group %in% groups_that_can_click) {return()}
        if (click$group == 'KNMI_markers') {
            # Get the stationname from the current df_knmi
            # And it should be in there, otherwise it cannot be shown/clicked
            df_knmi <- df_knmi()
            # Change it in entire scope, so the next observeEvent is triggered by the change you do here
            rv$knmi_station_history <<- df_knmi[df_knmi$knmi_lat == click$lat &
                                                    df_knmi$knmi_lon == click$lng, 'knmi_name']
        } else if (click$group == 'MetOffice_markers') {
            # Same as above, but then for metoffice
            df_metoffice <- df_metoffice()
            rv$metoffice_station_history <<- df_metoffice[df_metoffice$metoffice_lat == click$lat &
                                                              df_metoffice$metoffice_lon == click$lng, 'metoffice_name']
        }
    })

    # Complementary stuff ----
    output$compared_time <- renderText({
        compared_time() %>%
            with_tz('Europe/Amsterdam') %>%
            strftime("%d %b %H:%M", tz='Europe/Amsterdam')
    })
    output$knmi_history_plot <- renderPlot({
        if(is.null(rv$knmi_station_history)) {
            # No plot necessary
            return()
        }
        # Datetime of begin/end of the day
        datetimes <- get_datetimes_history()
        # Get all rows for the specific KNMI station since beginning of this day
        stmt <- sprintf("SELECT * FROM knmi_data_source WHERE stationname = '%s' AND datetime >= '%s'",
                        rv$knmi_station_history,
                        datetimes$datetime_begin
        )
        df_knmi_history_plot <- run.query(stmt)$result
        # Make it datetime, and Europe/Amsterdam
        df_knmi_history_plot$datetime <- df_knmi_history_plot$datetime %>% as.POSIXct() %>% with_tz('Europe/Amsterdam')
        p <- ggplot()
        p <- p + geom_line(data=df_knmi_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_KNMI_plot[[input$observable]]),
                           color='red')
        # Determine the lat/lon to join KNMI on with GFS
        knmi_lat <- round(df_knmi_history_plot[1, 'lat'] / 0.25, 0) * 0.25
        knmi_lon <- round(df_knmi_history_plot[1, 'lon'] / 0.25, 0) * 0.25
        df_gfs_history_plot <- get_gfs_history(knmi_lat, knmi_lon, datetimes)
        p <- p + geom_line(data=df_gfs_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_GFS[[input$observable]]),
                           color='black')
        df_gfs_history_plot_apx <- get_gfs_history_apx(knmi_lat, knmi_lon, datetimes)
        p <- p + geom_line(data=df_gfs_history_plot_apx,
                           aes_string(x='datetime',
                                      y=conversion_list_GFS[[input$observable]]),
                           color='black',
                           linetype='dashed')
	      knmi_lat <- round(df_knmi_history_plot[1, 'lat'] / 0.1, 0) * 0.1
        knmi_lon <- round(df_knmi_history_plot[1, 'lon'] / 0.1, 0) * 0.1
        df_hirlam_history_plot <- get_hirlam_history(knmi_lat, knmi_lon, datetimes)
        p <- p + geom_line(data=df_hirlam_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_HIRLAM[[input$observable]]),
                           color='green')
        p <- p + ggtitle(rv$knmi_station_history) + ylab(input$observable) + scale_x_datetime(expand=c(0,0))
        return(p)
    })
    output$metoffice_history_plot <- renderPlot({
        if(is.null(rv$metoffice_station_history)) {
            # No plot necessary
            return()
        }
        # Datetime of begin/end of the day
        datetimes <- get_datetimes_history()
        # Get all rows for the specific metoffice station since beginning of this day
        stmt <- sprintf(stmt_metoffice_history %>% strwrap(width=10000, simplify=TRUE),
                        rv$metoffice_station_history,
                        datetimes$datetime_begin,
                        datetimes$datetime_end
        )
        df_metoffice_history_plot <- run.query(stmt)$result
        # Make it datetime, and Europe/Amsterdam
        df_metoffice_history_plot$datetime <- df_metoffice_history_plot$datetime %>% as.POSIXct() %>% with_tz('Europe/Amsterdam')
        p <- ggplot()
        p <- p + geom_line(data=df_metoffice_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_metoffice_plot[[input$observable]]),
                           color='red')
        # Determine the lat/lon to join metoffice on with GFS
        metoffice_lat <- round(df_metoffice_history_plot[1, 'lat'] / 0.25, 0) * 0.25
        metoffice_lon <- round(df_metoffice_history_plot[1, 'lon'] / 0.25, 0) * 0.25
        df_gfs_history_plot <- get_gfs_history(metoffice_lat, metoffice_lon, datetimes)
        p <- p + geom_line(data=df_gfs_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_GFS[[input$observable]]),
                           color='black')
        df_gfs_history_plot_apx <- get_gfs_history_apx(metoffice_lat, metoffice_lon, datetimes)
        p <- p + geom_line(data=df_gfs_history_plot_apx,
                           aes_string(x='datetime',
                                      y=conversion_list_GFS[[input$observable]]),
                           color='black',
                           linetype='dashed')
		metoffice_lat <- round(df_metoffice_history_plot[1, 'lat'] / 0.1, 0) * 0.1
        metoffice_lon <- round(df_metoffice_history_plot[1, 'lon'] / 0.1, 0) * 0.1
        df_hirlam_history_plot <- get_hirlam_history(metoffice_lat, metoffice_lon, datetimes)
        p <- p + geom_line(data=df_hirlam_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_HIRLAM[[input$observable]]),
                           color='green')
        p <- p + ggtitle(rv$metoffice_station_history) + ylab(input$observable) + scale_x_datetime(expand=c(0,0))
        return(p)
    })

    # IGCC ----
    IGCC_data <- reactive({
        autoInvalidate_IGCC()
        df_IGCC <- withProgress(
            # This part takes care of showing the notifcation when data is fetched
            message='Fetching IGCC data',
            detail='Always and truly, Mathias',
            value=NULL,
            style='old',
            {
                # The actual data fetching
                get_IGCC_data()
            })
        return(df_IGCC)
    })
    IGCC_data_export <- reactive({
        df_IGCC <- IGCC_data()
        export <- names(df_IGCC)[grepl(names(df_IGCC), pattern='export_vol')]
        IGCC_data_export <- df_IGCC[, c('datetime', export)]
        export <- export %>% str_sub(1, 2) %>% toupper
        names(IGCC_data_export) <- c('datetime', export)

        IGCC_data_export <- IGCC_data_export%>% melt(id.vars='datetime') %>% na.omit

        return(IGCC_data_export)
    })
    IGCC_data_import <- reactive({
        df_IGCC <- IGCC_data()
        import <- names(df_IGCC)[grepl(names(df_IGCC), pattern='import_vol')]
        IGCC_data_import <- df_IGCC[, c('datetime', import)]
        import <- import %>% str_sub(1, 2) %>% toupper
        names(IGCC_data_import) <- c('datetime', import)
        IGCC_data_import[, import] <- -1. * IGCC_data_import[, import]
        IGCC_data_import <- IGCC_data_import%>% melt(id.vars='datetime') %>% na.omit
        return(IGCC_data_import)
    })
    output$igcc_plot <- renderPlot({
        ggplot() +
            geom_bar(data=IGCC_data_import(),
                     aes(x=datetime,
                         y=value,
                         group=variable,
                         fill=variable),
                     width=15*60,
                     color='black',
                     stat='identity') +
            geom_bar(data=IGCC_data_export(),
                     aes(x=datetime,
                         y=value,
                         group=variable,
                         fill=variable),
                     width=15*60,
                     color='black',
                     stat='identity') +
            scale_fill_manual(values=coloring_IGCC) +
            scale_x_datetime(limits=c(Sys.Date() %>% as.POSIXct %>% with_tz('Europe/Amsterdam') %>% trunc('days') %>% as.POSIXct,
                                     (Sys.Date() +1) %>% as.POSIXct) %>% with_tz('Europe/Amsterdam')  %>% trunc('days') %>% as.POSIXct,
                             breaks=date_breaks('2 hours'),
                             expand=c(0, 0),
                             minor_breaks=date_breaks('1 hours'),
                             labels=date_format("%H", tz='Europe/Amsterdam')) +
            geom_hline(aes(yintercept=0), size=2) +
            ylab('MW') +
            xlab('Time') +
            annotate("text",
                     x = Sys.Date() %>% as.POSIXct,
                     y = Inf,
                     vjust = 1,
                     hjust=0,
                     label = "Exported"
            ) +
            annotate("text",
                     x = Sys.Date() %>% as.POSIXct,
                     y = -Inf,
                     vjust=-.1,
                     hjust=0,
                     label="Imported"
            ) + theme(legend.position = 'bottom') +
            guides(fill = guide_legend(nrow=1))
    })
}
