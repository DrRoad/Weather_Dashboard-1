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
