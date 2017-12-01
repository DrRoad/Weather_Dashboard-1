import_data_sql <- function(max_hours_back = 4, max_hours_forward=1) {

    # Min and Max datetime for the query
    minimal_datetime <- (Sys.time() %>%
                             trunc('hour') - max_hours_back * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    maximal_datetime <- (Sys.time() %>%
                             trunc('hour') + max_hours_forward * 60 * 60) %>%
        strftime("%Y-%m-%d %H:%M:%S")
    # basically, give everything between min and max datetime
    stmt <- sprintf("SELECT * from weather_sources_view WHERE datetime >= '%s' AND datetime <= '%s'",
                    minimal_datetime,
                    maximal_datetime)
    a = run.query(stmt)
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
    df_IGCC$Date <- df_IGCC$Date %>%
        strptime(format="%Y-%m-%d %H:%M:%S", tz='UTC') %>%
        with_tz('Europe/Amsterdam') %>%
        as.POSIXct
    df_IGCC$Date <- df_IGCC$Date + 7.5 * 60
    return(df_IGCC)
}

get_datetimes_history <- function() {
    # Datetime of begin/end of the day
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

get_gfs_history_apx <- function(lat, lon, datetimes) {
    stmt <- sprintf(stmt_gfs_history_apx %>% strwrap(width=10000, simplify=TRUE),
                    datetimes$datetime_apx,
                    datetimes$datetime_begin,
                    datetimes$datetime_end,
                    lat,
                    lon)
    df_gfs_history_plot_apx <- run.query(stmt)$result
    df_gfs_history_plot_apx$datetime <- df_gfs_history_plot_apx$datetime %>% as.POSIXct %>% with_tz('Europe/Amsterdam')
    return(df_gfs_history_plot_apx)
}
