# import_data_sql <- function(max_hours_back = 4, max_hours_forward=1,model) {

    # # Min and Max datetime for the query
    # minimal_datetime <- (Sys.time() %>%
                             # trunc('hour') - max_hours_back * 60 * 60) %>%
        # strftime("%Y-%m-%d %H:%M:%S")
    # maximal_datetime <- (Sys.time() %>%
                             # trunc('hour') + max_hours_forward * 60 * 60) %>%
        # strftime("%Y-%m-%d %H:%M:%S")
    # # basically, give everything between min and max datetime
    # stmt <- sprintf("select * from (select @start_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72) as p) start_value ,
                    # (select @end_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72) as p) end_value,
                    # (select @start_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) start_value1 ,
                    # (select @end_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) end_value1, weather_sources_view_hirlam;",
                    # minimal_datetime,
                    # maximal_datetime,
                    # minimal_datetime,
                    # maximal_datetime)
    # a = run.query(stmt)
    # return(a$result)
# }

import_data_sql_model <- function(model, max_hours_back=4, max_hours_forward=1) {

    # Min and Max datetime for the query
    minimal_datetime <- (Sys.time() %>%
                             trunc('hour') - max_hours_back * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    maximal_datetime <- (Sys.time() %>%
                             trunc('hour') + max_hours_forward * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    # basically, give everything between min and max datetime
    if (model == 'HIRLAM') {
        stmt <- sprintf("select * from (select @start_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72) as p) start_value ,
                    (select @end_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72) as p) end_value,
                    (select @start_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) start_value1 ,
                    (select @end_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) end_value1, weather_sources_view_hirlam;",
                        minimal_datetime,
                        maximal_datetime,
                        minimal_datetime,
                        maximal_datetime)
    }
    if (model == 'GFS') {
        stmt <- sprintf("select * from (select @start_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 432) as p) start_value ,
					(select @end_partition_value:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 432) as p) end_value,
					(select @start_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) start_value1 ,
					(select @end_partition_value1:=(floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48) as p) end_value1, weatherforecast.weather_sources_view_gfs;",
                        minimal_datetime,
                        maximal_datetime,
                        minimal_datetime,
                        maximal_datetime)
    }
    a = run.query(stmt)
    return(a$result)
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

    a <- run.query(stmt)
    return(a$result)
}

run.query <- function(stmt) {
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
    result <- dbGetQuery(conn, stmt)
    # time logging
    time <- round(as.numeric((proc.time() - ptm)["elapsed"]), 2)
    print(sprintf("Query took %.2f seconds", time))
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
    # print(stmt)
    df_observation_history <- run.query(stmt)$result
    df_observation_history %>% head %>% print
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
                    lon)
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

    lat_column <- sprintf("%s_lat", group)
    lon_column <- sprintf("%s_lon", group)
    name_column <- sprintf("%s_name", group)

    df <- df[!is.na(df[[name_column]]), ]
    name <- df[(df[[lat_column]] == click$lat) &
                   (df[[lon_column]] == click$lng),
               name_column]

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
