# To make sure the times that are obtain from the system are always in UTC, independent
# on which environment you are working

Sys.setenv(TZ='GMT')

base_path = "../data/"

data_path = file.path(base_path, "data_hot_tub.csv")

# conversion lists models
conversion_list_GFS <<- list("Windspeed"="gfs_wind_speed",
                             "Temperature"="gfs_temp",
                             "Air pressure"="gfs_air_pressure",
                             "Radiation"="gfs_radiation")
conversion_list_HIRLAM <<- list("Windspeed"="hirlam_wind_speed",
                                "Temperature"="hirlam_temp",
                                "Air pressure"="hirlam_air_pressure",
                                "Radiation"="hirlam_radiation")

# conversion lists observations
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
conversion_list_OWM_plot <<- list("Windspeed"="wind_speed",
                                   "Temperature"="main_temp",
                                   "Air pressure"="main_pressure",
                                   "Radiation"="main_pressure") # Not available
conversion_list_metoffice_plot <<- list("Windspeed"="wind_speed",
                                        "Temperature"="temperature",
                                        "Air pressure"="pressure",
                                        "Radiation"="pressure") # Not available

conversion_list_model <<- list('GFS' = conversion_list_GFS,
                               'HIRLAM' = conversion_list_HIRLAM)
conversion_list_observations_plot <<- list("knmi"=conversion_list_KNMI_plot,
                                           "owm"=conversion_list_OWM_plot,
                                           "metoffice"=conversion_list_metoffice_plot
)
# Windparkfile
Windparks_filename <- file.path(base_path, 'Windparks/windparks_Eneco.csv')
Windparks <- Windparks_filename %>% read.csv %>% data.frame

external_windparks_filename <- file.path(base_path, "Windparks/windparks_External.csv")
external_windparks <- external_windparks_filename %>% read.csv %>% data.frame

# Windparks icons
windparkiconurl <- "../data/Windparks/wfarm.png"
windparkiconurl_grey <- "../data/Windparks/wfarm_grey.png"
windparkiconurl_wind_rt <- "../data/Windparks/wfarm_orange.png"

# Wind direction icons
directions <- seq(0, 360, 22.5) %>% round(0)
wind_directions_location <- c("../data/arrows/arrow_icon_%03d.png" %>% sprintf(directions))
names(wind_directions_location) <- directions

wind_rt_location <- run.query("SELECT
    longitude as lon,
    latitude as lat,
    aggregateId,
    nominalpower as nominal_power
FROM (select * FROM breeze.breeze_power_data_source GROUP BY longitude, latitude) breeze
INNER JOIN (SELECT * FROM mapping.pl_breeze_mapping) mapping
    ON breeze.aggregateid = mapping.breezeId
LEFT JOIN (SELECT * FROM mapping.nominal_power) nominal_power
    ON mapping.pl = nominal_power.pl",'Wind RT locations')$result

# Query statements ----
basetime <- as.POSIXct('2017-11-29 00:00:00')
stmt_gfs_history <- "SELECT gfs.datetime as datetime,
    gfs.2_metre_temperature_level_2 as gfs_temp,
    gfs.10_metre_wind_speed_level_10 as gfs_wind_speed,
    gfs.downward_short_wave_radiation_flux_level_0 as gfs_radiation,
    gfs.surface_pressure_level_0 as gfs_air_pressure
    FROM l_gfs_data_source gfs INNER JOIN
    (
        SELECT datetime, lat, lon, MIN(hours_ahead) as hours_ahead
        FROM l_gfs_data_source


        WHERE datetime >='%s'
              AND datetime <'%s'
              AND lat = %.2f
              AND lon = %.2f
        GROUP BY datetime, lat, lon
    ) gfs2 ON gfs.lon = gfs2.lon
              AND gfs.lat = gfs2.lat
              AND gfs.datetime = gfs2.datetime



              AND gfs.hours_ahead = gfs2.hours_ahead"

stmt_gfs_history_apx <- "SELECT datetime,
                                2_metre_temperature_level_2 as gfs_temp,
                                10_metre_wind_speed_level_10 as gfs_wind_speed,
                                downward_short_wave_radiation_flux_level_0 as gfs_radiation,
                                surface_pressure_level_0 as gfs_air_pressure
FROM l_gfs_data_source


WHERE datetime >='%s' AND
      datetime <'%s' AND
      hours_ahead >= 17 AND
      hours_ahead <= 41 AND
      lat = %.2f AND
      lon = %.2f AND
      model_date = '%s' AND
      model_run = 6"

stmt_hirlam_history <- "SELECT hirlam.datetime as datetime,
                               hirlam.2_metre_temperature - 273.15 as hirlam_temp,
                               hirlam.10_metre_wind_speed as hirlam_wind_speed,
                               hirlam.global_radiation_flux as hirlam_radiation,
                               hirlam.pressure as hirlam_air_pressure
FROM l_hirlam_data_source hirlam INNER JOIN
(
    SELECT datetime, lat, lon, MIN(hours_ahead) as hours_ahead
    FROM l_hirlam_data_source


    WHERE datetime >='%s' AND
          datetime <'%s' AND
          lat = %.2f AND
          lon = %.2f
    GROUP BY datetime, lat, lon
) hirlam2 on hirlam.lon = hirlam2.lon AND hirlam.lat = hirlam2.lat and hirlam.datetime = hirlam2.datetime and hirlam.hours_ahead = hirlam2.hours_ahead"

stmt_hirlam_history_2 <- "SELECT hirlam.datetime as datetime,
                                 hirlam.2_metre_temperature - 273.15 as hirlam_temp,
                                 hirlam.10_metre_wind_speed as hirlam_wind_speed,
                                 hirlam.global_radiation_flux as hirlam_radiation,
                                 hirlam.pressure as hirlam_air_pressure
FROM l_hirlam_data_source hirlam INNER JOIN
(
    SELECT datetime, lat, lon, MIN(hours_ahead) as hours_ahead
    FROM l_hirlam_data_source


    WHERE datetime>='%s' AND
          datetime <'%s' AND
          lat = %.2f AND
          lon = %.2f
    GROUP BY datetime, lat, lon
    ) hirlam2 on hirlam.lon = hirlam2.lon AND hirlam.lat = hirlam2.lat and hirlam.datetime = hirlam2.datetime and hirlam.hours_ahead = hirlam2.hours_ahead"


stmt_meteosat <- "SELECT * FROM weatherforecast.meteosat_data_source
WHERE partition_col>=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48
AND partition_col<=floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 48"

stmt_metoffice_history = "SELECT mo1.*
FROM metoffice_data_source mo1 INNER JOIN
(
    SELECT datetime, MAX(processed_time) as processed_time, name
    FROM metoffice_data_source
    WHERE name = '%s' AND datetime >= '%s' AND datetime <= '%s'
    GROUP BY datetime
) mo2 on mo1.datetime = mo2.datetime AND mo1.processed_time = mo2.processed_time and mo1.name = mo2.name ORDER BY datetime"

colors_MeteoSat_cot <- colorBin(c("#ffffff", "#000000"), domain=c(-1.5, 2.41), na.color="transparent", bins=15)
colors_MeteoSat_precip <- colorBin(c("#0000ff", "#ff0000"), domain=c(-2, 1), na.color="transparent", bins=20)
colors_MeteoSat <- colorBin(c("#0000ff00", "#ff0000ff"), domain=c(-200, 200), na.color="transparent", bins=20)

domain_MeteoSat_map <-list("sds"=c(0, 1000),
                           "sds_diff"=c(0,1000),
                           "cot"=c(0,300),
                           "caf"=c(0,3),
                           "precip"=c(0,20))
cpalet_MeteoSat_map <- list(
    "sds"=colorBin(c("#0000ffff", "#ffffff00", "#ff0000ff"),
                   domain=domain_MeteoSat_map[["sds"]],
                   na.color="transparent",
                   bins=20),
    "sds_diff"=colorBin(c("#0000ffff", "#ffffff00",  "#ff0000ff"),
                        domain=domain_MeteoSat_map[["sds_diff"]],
                        na.color="transparent",
                        bins=20),
    "cot"=colorBin(c("#0000ffff", "#ffffff00", "#ff0000ff"),
                   domain=domain_MeteoSat_map[["cot"]],
                   na.color="transparent",
                   bins=20),
    "caf"=colorBin(c("#0000ffff", "#ffffff00", "#ff0000ff"),
                   domain=domain_MeteoSat_map[["caf"]],
                   na.color="transparent",
                   bins=4),
    "precip"=colorBin(c("#ffffff00", "#0000ff00"),
                      domain=domain_MeteoSat_map[["precip"]],
                      na.color="transparent",
                      bins=20)
)

stmt_gfs_modelruns <- "SELECT
    model_date,
    model_run,
    datetime,
    lat,
    lon,
    2_metre_temperature_level_2 as gfs_temp,
    10_metre_wind_speed_level_10 as gfs_wind_speed,
    downward_short_wave_radiation_flux_level_0 as gfs_radiation,
    surface_pressure_level_0 as gfs_air_pressure
FROM
    gfs_data_source
WHERE
    partition_col >= floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 432
    AND partition_col < floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 432
    AND model_date >= '%s'
ORDER BY
    model_date,
    model_run"

stmt_hirlam_modelruns <- "SELECT
    model_date,
    model_run,
    datetime,
    lat,
    lon,
    2_metre_temperature - 273.15 as hirlam_temp,
    10_metre_wind_speed as hirlam_wind_speed,
    global_radiation_flux as hirlam_radiation,
    pressure as hirlam_air_pressure
FROM
    hirlam_data_source
WHERE
    partition_col >= floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72
    AND partition_col < floor((UNIX_TIMESTAMP('%s') - UNIX_TIMESTAMP('2017-11-29 00:00:00'))/3600) mod 72
    AND model_date >= '%s'
ORDER BY
    model_date,
    model_run"
