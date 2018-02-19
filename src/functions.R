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
    a$result[,'nan_vector'] <- NA
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

import_data_sql_meteosat <- function(time,max_hours_back, max_hours_forward) {
    minimal_datetime <- (time %>%
                             trunc('hour') - max_hours_back * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    maximal_datetime <- (time %>%
                             trunc('hour') + max_hours_forward * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    if(max_hours_back == 0.2) {
        stmt <- sprintf("SELECT * from weatherforecast.meteosat_data_source where partition_col>=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48 and partition_col<=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48 AND datetime='%s'",
                        minimal_datetime,
                        maximal_datetime,
                        time)
    }
    else {
        stmt <- sprintf("SELECT * from weatherforecast.meteosat_data_source where partition_col>=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48 and partition_col<=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48",
                        minimal_datetime,
                        maximal_datetime)
    }
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
                                     datetimes$datetime_end),
                   windy=sprintf(stmt_windy_history %>% strwrap(width=10000, simplify=TRUE),
                                     name,
                                     datetimes$datetime_begin,
                                     datetimes$datetime_end)
    )
    df_observation_history <- run.query(stmt, sprintf('historic observation (%s)', group))$result
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
    df_gfs_history_plot <- run.query(stmt, 'gfs history')$result
    df_gfs_history_plot$datetime <- df_gfs_history_plot$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_gfs_history_plot)
}

get_iconeu_history <- function(lat, lon, datetimes) {
    # Construct the stmt by filling in the blanks in the base stmt
    stmt <- sprintf(stmt_iconeu_history %>% strwrap(width=10000, simplify=TRUE),
                    datetimes$datetime_begin,
                    datetimes$datetime_end,
                    lat,
                    lon,
                    datetimes$datetime_begin,
                    datetimes$datetime_end)
    df_iconeu_history_plot <- run.query(stmt, 'iconeu history')$result
    df_iconeu_history_plot$datetime <- df_iconeu_history_plot$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_iconeu_history_plot)
}

get_ecmwf_history <- function(lat, lon, datetimes) {
    # Construct the stmt by filling in the blanks in the base stmt
    stmt <- sprintf(stmt_ecmwf_history %>% strwrap(width=10000, simplify=TRUE),
                    datetimes$datetime_begin,
                    datetimes$datetime_end,
                    lat,
                    lon,
                    datetimes$datetime_begin,
                    datetimes$datetime_end)
    df_ecmwf_history_plot <- run.query(stmt, 'ecmwf history')$result
    df_ecmwf_history_plot$datetime <- df_ecmwf_history_plot$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_ecmwf_history_plot)
}

get_hirlam_history <- function(lat, lon, datetimes) {
    # Construct the stmt by filling in the blanks in the base stmt
    stmt <- sprintf(stmt_hirlam_history %>% strwrap(width=10000, simplify=TRUE),
                    datetimes$datetime_begin,
                    datetimes$datetime_end,
                    lat,
                    lon)
    df_hirlam_history_plot <- run.query(stmt, 'hirlam history')$result
    if (df_hirlam_history_plot %>% nrow == 0) {
        print("Taking second HIRLAM")
        stmt <- sprintf(stmt_hirlam_history_2 %>% strwrap(width=10000, simplify=TRUE),
                        datetimes$datetime_begin,
                        # datetimes$datetime_end,datetimes$datetime_begin,
                        datetimes$datetime_end,
                        lat,
                        lon)
        df_hirlam_history_plot <- run.query(stmt,'hirlam history2')$result
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
    df_gfs_history_plot_apx <- run.query(stmt,'gfs history apx')$result
    df_gfs_history_plot_apx$datetime <- df_gfs_history_plot_apx$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_gfs_history_plot_apx)
}

create_observation_history_plot <- function(click, datetimes, df, observable) {
    # group can either be knmi, owm, metoffice or windy
    group <- click$group %>% str_split('_') %>% unlist %>% head(1) %>% tolower
    name <- click$id

    df_observation_history <- get_historic_observation_data(click, group, datetimes, name)
    p <- ggplot()
    p <- p + geom_line(data=df_observation_history,
                       aes_string(x='datetime',
                                  y=conversion_list_observations_plot[[group]][[observable]],color=shQuote('one')))

    # Determine the lat/lon to join observations with GFS
    gfs_lat_plot <- round(click$lat / 0.25, 0) * 0.25
    gfs_lon_plot <- round(click$lng / 0.25, 0) * 0.25
    df_gfs_history_plot <- get_gfs_history(gfs_lat_plot, gfs_lon_plot, datetimes)
    p <- p + geom_line(data=df_gfs_history_plot,
                       aes_string(x='datetime',
                                  y=conversion_list_GFS[[observable]],color=shQuote('two')))
    df_gfs_history_plot_apx <- get_gfs_history_apx(gfs_lat_plot, gfs_lon_plot, datetimes)
    p <- p + geom_line(data=df_gfs_history_plot_apx,
                       aes_string(x='datetime',
                                  y=conversion_list_GFS[[observable]],color=shQuote('three')),
                       linetype='dashed')
    hirlam_lat_plot <- round(click$lat / 0.1, 0) * 0.1
    hirlam_lon_plot <- round(click$lng / 0.1, 0) * 0.1
    df_hirlam_history_plot <- get_hirlam_history(hirlam_lat_plot, hirlam_lon_plot, datetimes)
    p <- p + geom_line(data=df_hirlam_history_plot,
                       aes_string(x='datetime',
                                  y=conversion_list_HIRLAM[[observable]],color=shQuote('four')))
    if (group == 'windy') {
        iconeu_lat_plot <- click$lat
        iconeu_lon_plot <- click$lng
        df_iconeu_history_plot <- get_iconeu_history(iconeu_lat_plot, iconeu_lon_plot, datetimes)
        p <- p + geom_line(data=df_iconeu_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_ICONEU[[observable]],color=shQuote("five")))
        df_ecmwf_history_plot <- get_ecmwf_history(iconeu_lat_plot, iconeu_lon_plot, datetimes)
        p <- p + geom_line(data=df_ecmwf_history_plot,
                           aes_string(x='datetime',
                                      y=conversion_list_ECMWF[[observable]],color=shQuote("six")))

        p <- p + ggtitle(name) + ylab(observable) +
            scale_x_datetime(expand=c(0,0)) +
            theme(legend.position='bottom') +
            scale_colour_manual(name='',
                                values= c(one='red',two='black',three='#999999',four='green',five='#009E73',six='#56B4E9'),
                                labels = c('ICONEU','HIRLAM','obs.','ECMWF','GFS-APX','GFS'))
        } else {
    p <- p + ggtitle(name) + ylab(observable) +
             scale_x_datetime(expand=c(0,0)) +
             theme(legend.position='bottom') +
             scale_colour_manual(name='',
                                 values= c(one='red',two='black',three='#999999',four='green'),
                                 labels = c('HIRLAM','obs.','GFS-APX','GFS'))
    }
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
