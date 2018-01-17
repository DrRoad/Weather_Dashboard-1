import_data_sql_model <- function(model, max_hours_back=4, max_hours_forward=1) {

    # Min and Max datetime for the query
    minimal_datetime <- (Sys.time() %>%
                             trunc('hour') - max_hours_back * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    maximal_datetime <- (Sys.time() %>%
                             trunc('hour') + max_hours_forward * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    # basically, give everything between min and max datetime
    table_name <- get_table_name_view(minimal_datetime, maximal_datetime, model)
    if (model == 'HIRLAM') {
        stmt <- sprintf("select * from (select @start_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72) as p) start_value ,
                    (select @end_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72) as p) end_value,
                    (select @start_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) start_value1 ,
                    (select @end_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) end_value1, weatherforecast.%s;",
                        minimal_datetime,
                        maximal_datetime,
                        minimal_datetime,
                        maximal_datetime,
                        table_name)
    }
    if (model == 'GFS') {
        stmt <- sprintf("select * from (select @start_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 432) as p) start_value ,
					(select @end_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 432) as p) end_value,
					(select @start_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) start_value1 ,
					(select @end_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) end_value1, weatherforecast.%s;",
                        minimal_datetime,
                        maximal_datetime,
                        minimal_datetime,
                        maximal_datetime,
                        table_name)
    }
    print(minimal_datetime)
    print(maximal_datetime)
    print(table_name)
    print(stmt)
    a = run.query(stmt, 'MOAD')  #Mother of all Data
    return(a$result)
}

get_table_name_view <- function(minimal_datetime, maximal_datetime, model) {
    if (model == 'HIRLAM') {
        start_value_hirlam <- time_diff(minimal_datetime, basetime, 72) %>% as.integer
        end_value_hirlam <- time_diff(maximal_datetime, basetime, 72) %>% as.integer
        start_value_other <- time_diff(minimal_datetime, basetime, 48) %>% as.integer
        end_value_other <- time_diff(maximal_datetime, basetime, 48) %>% as.integer
        if (start_value_hirlam < end_value_hirlam) {
            if (start_value_other < end_value_other) {
                table_name = 'weather_sources_view_hirlam_11'
            } else {table_name = 'weather_sources_view_hirlam_10'}
        } else {
            if (start_value_other < end_value_other) {
                table_name = 'weather_sources_view_hirlam_01'
            } else {table_name = 'weather_sources_view_hirlam_00'}
        }
    }
    if (model == 'GFS') {
        start_value_gfs <- time_diff(minimal_datetime, basetime, 432) %>% as.integer
        end_value_gfs <- time_diff(maximal_datetime, basetime, 432) %>% as.integer
        start_value_other <- time_diff(minimal_datetime, basetime, 48) %>% as.integer
        end_value_other <- time_diff(maximal_datetime, basetime, 48) %>% as.integer
        if (start_value_gfs < end_value_gfs) {
            if (start_value_other < end_value_other) {
                table_name = 'weather_sources_view_gfs_11'
            } else {table_name = 'weather_sources_view_gfs_10'}
        } else {
            if (start_value_other < end_value_other) {
                table_name = 'weather_sources_view_gfs_01'
            } else {table_name = 'weather_sources_view_gfs_00'}
        }
    }
    return(table_name)
}

import_data_sql_meteosat <- function(max_hours_back=1, max_hours_forward=1) {
    minimal_datetime <- (Sys.time() %>%
                             trunc('hour') - max_hours_back * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    maximal_datetime <- (Sys.time() %>%
                             trunc('hour') + max_hours_forward * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")

    stmt <- sprintf("SELECT * from weatherforecast.meteosat_data_source where partition_col>=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48 and partition_col<=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48",
                    minimal_datetime,
                    maximal_datetime)

    a <- run.query(stmt, 'MeteoSat')
    return(a$result)
}

import_data_sql_modelrun_compare <- function(model, max_hours_back=4, max_hours_forward=5, days_back=3) {

    # Min and Max datetime for the query
    minimal_datetime <- (Sys.time() %>%
                             trunc('hour') - max_hours_back * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    maximal_datetime <- (Sys.time() %>%
                             trunc('hour') + max_hours_forward * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    minimal_model_date <- (Sys.Date() - days_back) %>% strftime("%Y-%m-%d")
    if (model == 'GFS') {
        stmt <- sprintf(stmt_gfs_modelruns %>% strwrap(width=10000, simplify=TRUE),
                        minimal_datetime,
                        maximal_datetime,
                        minimal_model_date)
    } else if (model == 'HIRLAM') {
        stmt <- sprintf(stmt_hirlam_modelruns %>% strwrap(width=10000, simplify=TRUE),
                        minimal_datetime,
                        maximal_datetime,
                        minimal_model_date)
    }
    a = run.query(stmt, 'modelrun_compare')
    return(a$result)
}

get_ID_data <- function(date=Sys.Date()) {
    minimal_datetime <- date %>%
        as.POSIXct %>%
        with_tz('Europe/Amsterdam') %>%
        trunc('days') %>%
        with_tz('UTC') %>%
        strftime('%Y-%m-%d %H:%M:%S')
    maximal_datetime <- (date + 1) %>%
        as.POSIXct %>%
        with_tz('Europe/Amsterdam') %>%
        trunc('days') %>%
        with_tz('UTC') %>%
        strftime('%Y-%m-%d %H:%M:%S')

    stmt <- sprintf(stmt_ID_data %>% strwrap(width=10000, simplify=TRUE),
            minimal_datetime,
            maximal_datetime)
    a <- run.query(stmt, 'ID_data')
    return(a$result)
}

run.query <- function(stmt, short_name = 'Empty. Fill me!') {
    # press start on stopwatch
    ptm <- proc.time()
    # make connection
    conn <- dbConnect(
        drv = RMySQL::MySQL(),
        db = "weatherforecast",
        host = "172.16.1.4",
        port = 3307,
        username = "eetanalytics",
        password = "eet@123")
    on.exit(dbDisconnect(conn), add=TRUE)
    # Do the actual query
    result <- suppressWarnings(dbGetQuery(conn, stmt))
    # time logging
    time <- round(as.numeric((proc.time() - ptm)["elapsed"]), 2)
    print(sprintf("Query took %.2f seconds (%s)", time, short_name))
    return(list(
        result=result,
        time=time
    ))
}

import_data <- function() {
    df <- read.csv(data_path, stringsAsFactors=FALSE)
    df[df == "NULL"] <- NA
    df$datetime <- df$datetime %>% as.POSIXct(tz='UTC')
    return(df)
}

raster_maker <- function(data, observable){
    frame.xy_f = cbind.data.frame(data$lon, data$lat)
    # coordinates(frame.xy_f) <- ~data$lon + data$lat

    frame_f <- cbind.data.frame(frame.xy_f, data[[observable]])
    residual_grid <- rasterFromXYZ(frame_f)
    proj4string(residual_grid) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    return(residual_grid)
}

get_IGCC_data <- function() {
    stmt <- sprintf(stmt_igcc %>% strwrap(width=10000, simplify=TRUE),
                    Sys.time() %>%
                        with_tz("Europe/Amsterdam") %>%
                        trunc('days') %>%
                        with_tz('UTC') %>%
                        strftime('%Y-%m-%d %H:%M:%S'))
    df_IGCC <- run.query(stmt)$result
    df_IGCC$datetime <- df_IGCC$datetime %>%
        strptime(format="%Y-%m-%d %H:%M:%S", tz='UTC') %>%
        with_tz('Europe/Amsterdam') %>%
        as.POSIXct
    df_IGCC$datetime <- df_IGCC$datetime + 7.5 * 60
    return(df_IGCC)
}

get_datetimes_history <- function() {
    # Datetime of begin/end of the day
    #begintime <- Sys.time() - hours(12)
    #endtime <- Sys.time() + hours(12)
    datetime_begin <- Sys.time() %>%
        with_tz('Europe/Amsterdam') %>%
        trunc('days') %>%
        with_tz('UTC') %>%
        strftime('%Y-%m-%d %H:%M:%S')
    datetime_end <- Sys.time() %>%
        with_tz('Europe/Amsterdam') %>%
        ceiling_date('days') %>%
        with_tz('UTC') %>%
        strftime('%Y-%m-%d %H:%M:%S')
    datetime_apx <- (Sys.time() %>%
        trunc('days') -
        18*60*60) %>%
        strftime('%Y-%m-%d')
    return(list(datetime_begin=datetime_begin, datetime_end=datetime_end, datetime_apx=datetime_apx))
}

get_historic_observation_data <- function(click, group, datetimes, name) {
    stmt <- switch(group,
                   knmi=sprintf("SELECT * FROM knmi_data_source WHERE stationname = '%s' AND datetime >= '%s'",
                                name,
                                datetimes$datetime_begin),
                   owm=sprintf("SELECT * FROM owm_data_source WHERE name = '%s' AND datetime >= '%s'",
                               name,
                               datetimes$datetime_begin),
                   metoffice=sprintf(stmt_metoffice_history %>% strwrap(width=10000, simplify=TRUE),
                                     name,
                                     datetimes$datetime_begin,
                                     datetimes$datetime_end)
    )
    df_observation_history <- run.query(stmt)$result
    df_observation_history$datetime <- df_observation_history$datetime %>%
        as.POSIXct() %>%
        with_tz('Europe/Amsterdam')

    return(df_observation_history)
}

get_gfs_history <- function(lat, lon, datetimes) {
    # Construct the stmt by filling in the blanks in the base stmt
    stmt <- sprintf(stmt_gfs_history %>% strwrap(width=10000, simplify=TRUE),
                    datetimes$datetime_begin,
                    datetimes$datetime_end,
                    lat,
                    lon,
                    datetimes$datetime_begin,
                    datetimes$datetime_end)
    df_gfs_history_plot <- run.query(stmt)$result
    df_gfs_history_plot$datetime <- df_gfs_history_plot$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_gfs_history_plot)
}

get_hirlam_history <- function(lat, lon, datetimes) {
  # Construct the stmt by filling in the blanks in the base stmt
  stmt <- sprintf(stmt_hirlam_history %>% strwrap(width=10000, simplify=TRUE),
                  datetimes$datetime_begin,
                  datetimes$datetime_end,
                  lat,
                  lon)
  df_hirlam_history_plot <- run.query(stmt)$result
  if (df_hirlam_history_plot %>% nrow == 0) {
      print("Taking second HIRLAM")
      stmt <- sprintf(stmt_hirlam_history_2 %>% strwrap(width=10000, simplify=TRUE),
                      datetimes$datetime_begin,
                      # datetimes$datetime_end,datetimes$datetime_begin,
                      datetimes$datetime_end,
                      lat,
                      lon)
      df_hirlam_history_plot <- run.query(stmt)$result
  }
  df_hirlam_history_plot$datetime <- df_hirlam_history_plot$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
  return(df_hirlam_history_plot)
}

get_gfs_history_apx <- function(lat, lon, datetimes) {
    stmt <- sprintf(stmt_gfs_history_apx %>% strwrap(width=10000, simplify=TRUE),
                    datetimes$datetime_begin,
                    datetimes$datetime_end,
                    lat,
                    lon,
                    datetimes$datetime_apx)
    df_gfs_history_plot_apx <- run.query(stmt)$result
    df_gfs_history_plot_apx$datetime <- df_gfs_history_plot_apx$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_gfs_history_plot_apx)
}

create_observation_history_plot <- function(click, datetimes, df, observable) {
    # group can either be knmi, owm or metoffice
    group <- click$group %>% str_split('_') %>% unlist %>% head(1) %>% tolower
    name <- click$id

    df_observation_history <- get_historic_observation_data(click, group, datetimes, name)

    p <- ggplot()
    p <- p + geom_line(data=df_observation_history,
                       aes_string(x='datetime',
                                  y=conversion_list_observations_plot[[group]][[observable]]),
                       color='red')

    # Determine the lat/lon to join observations with GFS
    gfs_lat_plot <- round(click$lat / 0.25, 0) * 0.25
    gfs_lon_plot <- round(click$lng / 0.25, 0) * 0.25
    df_gfs_history_plot <- get_gfs_history(gfs_lat_plot, gfs_lon_plot, datetimes)
    p <- p + geom_line(data=df_gfs_history_plot,
                       aes_string(x='datetime',
                                  y=conversion_list_GFS[[observable]]),
                       color='black')
    df_gfs_history_plot_apx <- get_gfs_history_apx(gfs_lat_plot, gfs_lon_plot, datetimes)
    p <- p + geom_line(data=df_gfs_history_plot_apx,
                       aes_string(x='datetime',
                                  y=conversion_list_GFS[[observable]]),
                       color='black',
                       linetype='dashed')
    hirlam_lat_plot <- round(click$lat / 0.1, 0) * 0.1
    hirlam_lon_plot <- round(click$lng / 0.1, 0) * 0.1
    df_hirlam_history_plot <- get_hirlam_history(hirlam_lat_plot, hirlam_lon_plot, datetimes)
    p <- p + geom_line(data=df_hirlam_history_plot,
                       aes_string(x='datetime',
                                  y=conversion_list_HIRLAM[[observable]]),
                       color='green')

    p <- p + ggtitle(name) + ylab(observable) + scale_x_datetime(expand=c(0,0))
    if (observable == 'Windspeed') {
        p <- p + scale_y_continuous(expand=c(0,0), limits=c(0, ggplot_build(p)$layout$panel_ranges[[1]]$y.range[[2]]))
    }
    return(p)
}

time_diff <- function(time1, basetime, mod_number, units='hours') {

    difftime(time1, basetime, units=units) %>% as.numeric %>% mod(mod_number) %>% as.character %>% return

}

change_input_to_column_name <- function(input_date, model, observable) {
    sprintf("%s_%s",
            conversion_list_model[[model]][[observable]],
            input_date %>%
                gsub("-", '.', .) %>%
                gsub(" ", '_', .)
    )
}

convert_click_to_coordinates <- function(e) {
    if(is.null(e)) return(NULL)
    list(lon=round(e$lng, 3), lat=round(e$lat, 3))
}

create_graphs_from_raw_ID_data <- function(df_ID_data_raw, unique_datetimes) {
    unique_datetimes <- df_ID_data_raw$datetime %>% unique

    list_graphs <- lapply(seq(1, length(unique_datetimes)), FUN=function(i) {
        d <- df_ID_data_raw[df_ID_data_raw$datetime == unique_datetimes[i], c('country_from', 'country_to', 'value')]
        names(d) <- c('from', 'to', 'w')
        g <- graph_from_data_frame(d)
    })
    return(list_graphs)
}

create_initial_empty_ID_data <- function(countries, unique_datetimes) {
    lapply(seq(1, length(unique_datetimes)), FUN=function(i) {
        df_temp <- data.frame(row.names=countries)
        df_temp[, countries] <- 0
        df_temp
    })
}

calculate_all_paths <- function(list_graphs, ID_data, df_ID_data_raw) {
    aggregate_ID <- aggregate(df_ID_data_raw$value, by=list(df_ID_data_raw$datetime), sum)$x
    for (i in seq(1, length(unique_datetimes))) {
        if (aggregate_ID[[i]] == 0) {next}
        g <- list_graphs[[i]]
        for (country_to in countries) {
            for (country_from in countries) {
                if (country_from == country_to) {
                    next
                }

                paths <- all_simple_paths(g, from=country_from, to=country_to)
                volumes <- sapply(paths,
                                  FUN = function(x)
                                      # This function calculates the possible volume between countries.
                                      # In case of price stuff, that should be included here
                                      min(E(g, path=x)$w)
                )
                ID_data[[i]][country_from, country_to] <- max(volumes)

            }
        }
    }
    return(ID_data)
}

