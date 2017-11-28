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


# Windparkfile
Windparks_filename <- file.path(base_path, 'Windparks/windparks_Eneco.csv')
Windparks <- Windparks_filename %>% read.csv %>% data.frame

external_windparks_filename <- file.path(base_path, "Windparks/windparks_External.csv")
external_windparks <- external_windparks_filename %>% read.csv %>% data.frame
#
# windparks_uk_filename <- file.path(base_path, "Windparks/windparks_External_UK.csv")
# windparks_uk <- windparks_uk_filename %>% read.csv %>% data.frame

# Windparks icons
windparkiconurl <- "../data/Windparks/wfarm.png"
windparkiconurl_grey <- "../data/Windparks/wfarm_grey.png"

