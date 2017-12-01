# To make sure the times that are obtain from the system are always in UTC, independent
# on which environment you are working
Sys.setenv(TZ='GMT')

base_path = "../data/"

data_path = file.path(base_path, "data_hot_tub.csv")

conversion_list_HIRLAM <<- list("Windspeed"="windspeed",
                                "Temperature"="Temperature_height_above_ground",
                                "Air pressure"="Pressure_altitude_above_msl",
                                "Radiation"="Radiation")

conversion_list_GFS <<- list("Windspeed"="gfs_wind_speed",
                             "Temperature"="gfs_temp",
                             "Air pressure"="gfs_air_pressure",
                             "Radiation"="gfs_radiation")
conversion_list_KNMI <<- list("Windspeed"="knmi_wind",
                              "Temperature"="knmi_temp",
                              "Air pressure"="knmi_air_pressure",
                              "Radiation"="knmi_radiation")
conversion_list_OWM <<- list("Windspeed"="owm_wind",
                             "Temperature"="owm_temp",
                             "Air pressure"="owm_air_pressure",
                             "Radiation"="owm_radiation")
conversion_list_MetOffice <<- list("Windspeed"="metoffice_wind",
                                   "Temperature"="metoffice_temp",
                                   "Air pressure"="metoffice_air_pressure",
                                   "Radiation"="metoffice_radiation")
conversion_list_KNMI_plot <<- list("Windspeed"="ff",
                                   "Temperature"="ta",
                                   "Air pressure"="pp",
                                   "Radiation"="qg")
conversion_list_metoffice_plot <<- list("Windspeed"="wind_speed",
                                        "Temperature"="temperature",
                                        "Air pressure"="pressure",
                                        "Radiation"="pressure") # Not available
# Windparkfile
Windparks_filename <- file.path(base_path, 'Windparks/windparks_Eneco.csv')
Windparks <- Windparks_filename %>% read.csv %>% data.frame

external_windparks_filename <- file.path(base_path, "Windparks/windparks_External.csv")
external_windparks <- external_windparks_filename %>% read.csv %>% data.frame

# Windparks icons
windparkiconurl <- "../data/Windparks/wfarm.png"
windparkiconurl_grey <- "../data/Windparks/wfarm_grey.png"

# Wind direction icons
directions <- seq(0, 360, 22.5) %>% round(0)
wind_directions_location <- c("../data/arrows/arrow_icon_%03d.png" %>% sprintf(directions))
names(wind_directions_location) <- directions

coloring_IGCC <- c("DE" = "white",
                   "NL" = "orange",
                   "BE" = "red",
                   "FR" = "blue",
                   "DK" = "green",
                   "AT" = "violet",
                   "CH" = "black",
                   "CZ" = "khaki")

# Query statements ----
stmt_gfs_history <- "SELECT gfs.datetime as datetime,
                            gfs.2_metre_temperature_level_2 as gfs_temp,
                            gfs.10_metre_wind_speed_level_10 as gfs_wind_speed,
                            gfs.downward_short_wave_radiation_flux_level_0 as gfs_radiation,
                            gfs.surface_pressure_level_0 as gfs_air_pressure
FROM gfs_data_source gfs INNER JOIN
(
    SELECT datetime, lat, lon, MIN(hours_ahead) as hours_ahead
    FROM gfs_data_source
    WHERE datetime >= '%s' AND datetime < '%s' AND lat = %.2f AND lon = %.2f
    GROUP BY datetime, lat, lon
) gfs2 on gfs.lon = gfs2.lon AND gfs.lat = gfs2.lat and gfs.datetime = gfs2.datetime and gfs.hours_ahead = gfs2.hours_ahead"

stmt_gfs_history_apx <- "SELECT datetime,
                                2_metre_temperature_level_2 as gfs_temp,
                                10_metre_wind_speed_level_10 as gfs_wind_speed,
                                downward_short_wave_radiation_flux_level_0 as gfs_radiation,
                                surface_pressure_level_0 as gfs_air_pressure
                         FROM gfs_data_source
                         WHERE
                             datetime >= '%s' AND
                             datetime < '%s' AND
                             hours_ahead >= 17 AND
                             hours_ahead <= 41 AND
                             lat = %.2f AND
                             lon = %.2f AND
                             model_date = '%s' AND
                             model_run = 6
                         ORDER BY datetime"

stmt_igcc <- "SELECT mk1.*
FROM mkonline_data_source mk1 INNER JOIN
(
    SELECT Date, MAX(processed_time) as processed_time
    FROM mkonline_data_source
    WHERE Date >= '%s'
    GROUP BY Date
) mk2 on mk1.Date = mk2.Date AND mk1.processed_time = mk2.processed_time ORDER BY Date"

stmt_metoffice_history = "SELECT mo1.*
FROM metoffice_data_source mo1 INNER JOIN
(
    SELECT datetime, MAX(processed_time) as processed_time, name
    FROM metoffice_data_source
    WHERE name = '%s' AND datetime >= '%s' AND datetime <= '%s'
    GROUP BY datetime
) mo2 on mo1.datetime = mo2.datetime AND mo1.processed_time = mo2.processed_time and mo1.name = mo2.name ORDER BY datetime"
