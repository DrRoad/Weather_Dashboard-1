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
