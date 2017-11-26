# To make sure the times that are obtain from the system are always in UTC, independent
# on which environment you are working
Sys.setenv(TZ='GMT')

data_path = "H:\\Sources\\Weather_Dashboard\\data\\data_hot_tub.csv"

conversion_list_HIRLAM <<- list("Windspeed"="windspeed",
                                "Temperature"="Temperature_height_above_ground",
                                "Air pressure"="Pressure_altitude_above_msl",
                                "Radiation"="Radiation")

conversion_list_GFS <<- list("Windspeed"="gfs_wind_speed",
                             "Temperature"="gfs_temp",
                             "Air pressure"="gfs_air_pressure",
                             "Radiation"="gfs_radiation")
