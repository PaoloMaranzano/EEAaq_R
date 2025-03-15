pkgname <- "EEAaq"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "EEAaq-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('EEAaq')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("EEAaq_export")
### * EEAaq_export

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_export
### Title: Export and save an 'EEAaq_df' class object
### Aliases: EEAaq_export

### ** Examples

## No test: 
### Download PM10 data for the province (NUTS-3) of Milano (Italy) from January 1st to January 31st, 2023
`%>%` <- dplyr::`%>%`
IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(NUTS3 %in% c("Milano")) %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2023-01-01", to = "2023-01-31", verbose = TRUE)

### Export data to csv file
temp <- tempdir()
filepath <- paste0(temp, "/data.csv")
EEAaq_export(data = data, filepath = filepath, format = "csv")
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_export", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_get_data")
### * EEAaq_get_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_get_data
### Title: Download air quality data at european level from the EEA
###   download service
### Aliases: EEAaq_get_data

### ** Examples

## No test: 
`%>%` <- dplyr::`%>%`
### Download PM10 data for the province (NUTS-3) of Milano (Italy) from January 1st to January 31st, 2023
IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(NUTS3 %in% c("Milano")) %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2023-01-01", to = "2023-01-31", verbose = TRUE)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_get_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_get_dataframe")
### * EEAaq_get_dataframe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_get_dataframe
### Title: EEAaq_get_dataframe
### Aliases: EEAaq_get_dataframe

### ** Examples

## No test: 
LAU <- EEAaq_get_dataframe(dataframe= "LAU")
pollutant <- EEAaq_get_dataframe(dataframe = "pollutant")
stations <- EEAaq_get_dataframe(dataframe = "stations")
NUTS <- EEAaq_get_dataframe(dataframe = "NUTS")
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_get_dataframe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_get_stations")
### * EEAaq_get_stations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_get_stations
### Title: Download EEA measurement station information dataset
### Aliases: EEAaq_get_stations

### ** Examples

## No test: 
EEAaq_get_stations(byStation = TRUE, complete = TRUE)
## End(No test)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_get_stations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_idw_map")
### * EEAaq_idw_map

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_idw_map
### Title: Build a spatial interpolation map based on the Inverse Distance
###   Weighting technique. The function 'EEAaq_idw_map' requires as input a
###   'EEAaq_taggr_df' or a 'EEAaq_taggr_df_sfc' class object and produces
###   a spatial interpolation map. Depending on the time frequency of the
###   aggregation, multiple maps are generated, one for each timestamp.
###   Interpolation maps may be exported as pdf, jpeg, png, gif and html.
### Aliases: EEAaq_idw_map

### ** Examples

## Not run: 
##D ### Filter all the stations installed in the city (LAU) of Milano (Italy)
##D IDstations <- EEAaq_get_stations(byStation = FALSE, complete = FALSE)
##D IDstations <- IDstations %>%
##D                 dplyr::filter(LAU_NAME == "Milano") %>%
##D                 dplyr::pull(AirQualityStationEoICode) %>%
##D                 unique()
##D ### Download NO2 measurement for the city of Milano from January 1st to December 31st, 2023
##D data <- EEAaq_get_data(IDstations = IDstations, pollutants = "NO2",
##D                        from = "2023-01-01", to = "2023-01-31", verbose = TRUE)
##D 
##D ### Monthly aggregation: compute station-specific monthly minimum, average, and maximum NO2 concentrations
##D t_aggr <- EEAaq_time_aggregate(data = data, frequency = "monthly",
##D                                aggr_fun = c("mean", "min", "max"))
##D 
##D ### Static IDW interpolation of the average NO2 concentrations for the whole Lombardy
##D ###    region (NUTS_extborder = "NUTS2"). Interpolated values are then aggregated at the provincial
##D ###    level (NUTS_filler = "NUTS3")
##D EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
##D               distinct = TRUE, gradient = FALSE,
##D               dynamic = FALSE,
##D               NUTS_filler = "NUTS3", NUTS_extborder = "NUTS2")
##D 
##D ### Dynamic IDW interpolation map (interactive leafleat) of the average NO2 concentrations for the whole Lombardy
##D ###    region (NUTS_extborder = "NUTS2"). Interpolated values are then aggregated at the municipal
##D ###    level (NUTS_filler = "LAU")
##D EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
##D               distinct = TRUE, gradient = FALSE,
##D               dynamic = TRUE,
##D               NUTS_filler = "LAU", NUTS_extborder = "NUTS2", NUTS_intborder = "LAU")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_idw_map", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_import")
### * EEAaq_import

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_import
### Title: Reverse function of 'EEAaq_export'. Reads an 'EEAaq_df' object
###   from a .txt or .csv file saved through 'EEAaq_export'.
### Aliases: EEAaq_import

### ** Examples

## No test: 
`%>%` <- dplyr::`%>%`
### Download PM10 data for the province (NUTS-3) of Milano (Italy) from January 1st to January 31st, 2023
IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(NUTS3 %in% c("Milano")) %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2023-01-01", to = "2023-01-31", verbose = TRUE)

### Export data to csv file
temp <- tempdir()
filepath <- paste0(temp, "/data.csv")
EEAaq_export(data = data, filepath = filepath, format = "csv")

### Import the EEAaq_df object saved in the previous code line
EEAaq_import(file_data = filepath)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_import", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_map_stations")
### * EEAaq_map_stations

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_map_stations
### Title: Create a static or dynamic (interactive leaflet) map
###   representing the geographical locations of the stations based on a
###   user-defined input dataset of class 'EEAaq_df' or 'EEAaq_df_sfc'.
### Aliases: EEAaq_map_stations

### ** Examples

## No test: 
`%>%` <- dplyr::`%>%`
### Retrieve all the stations measuring PM10 in Belgium
IDstations <- IDstations %>%
  dplyr::filter(ISO %in% c("BE"),
                AirPollutant %in% "PM10") %>%
  dplyr::pull(AirQualityStationEoICode) %>%
  unique()

### Download the corresponding data froom December 1st to December 31st, 2021
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2021-12-01", to = "2021-12-31", verbose = TRUE)

### Static map of available stations across the whole country. External borders are given by the
###     union of the available regions (NUTS-2), while municipalities (LAUs) are used as inner borders.
EEAaq_map_stations(data = data,
                  NUTS_extborder = "NUTS2", NUTS_intborder = "LAU",
                  color = TRUE, dynamic = FALSE)
### Dynamic (interactive leaflet) map of available stations across the whole country. External borders are given by the
###     union of the available regions (NUTS-2), while provinces (NUTS-3) are used as inner borders.
EEAaq_map_stations(data = data,
                  NUTS_extborder = "NUTS2", NUTS_intborder = "NUTS3",
                  color = TRUE, dynamic = TRUE)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_map_stations", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_summary")
### * EEAaq_summary

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_summary
### Title: Generate an 'EEAaq_df' data summary. This function must be
###   applied to an 'EEAaq_df' or 'EEAaq_df_sfc' class object and produces
###   a list of data frames, containing relevant information about the
###   data, such as descriptive statistics, missing values statistics, gap
###   length and linear correlation.
### Aliases: EEAaq_summary

### ** Examples

## No test: 
### Download PM10 data for the province (NUTS-3) of Milano (Italy) from January 1st to January 31st, 2023
IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(NUTS3 %in% c("Milano")) %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2023-01-01", to = "2023-01-31", verbose = TRUE)

### Compute summary statistics
EEAaq_summary(data)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_summary", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("EEAaq_time_aggregate")
### * EEAaq_time_aggregate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: EEAaq_time_aggregate
### Title: Time aggregation of an 'EEAaq_df' class object.
### Aliases: EEAaq_time_aggregate

### ** Examples

## No test: 
### Filter all the stations installed in the city (LAU) of Milano (Italy)
IDstations <- EEAaq_get_stations(byStation = FALSE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(LAU_NAME == "Milano") %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
### Download NO2 measurement for the city of Milano from January 1st to December 31st, 2023
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "NO2",
                       from = "2023-01-01", to = "2023-01-31", verbose = TRUE)

### Monthly aggregation: compute station-specific monthly minimum, average, and maximum NO2 concentrations
t_aggr <- EEAaq_time_aggregate(data = data, frequency = "monthly",
                               aggr_fun = c("mean", "min", "max"))

### Weekly aggregation: compute station-specific monthly average and standard deviation concentrations
t_aggr <- EEAaq_time_aggregate(data = data, frequency = "weekly",
                               aggr_fun = c("mean", "sd"))
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("EEAaq_time_aggregate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("is_EEAaq_df")
### * is_EEAaq_df

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: is_EEAaq_df
### Title: Check if a given object is an 'EEAaq_df' class object
### Aliases: is_EEAaq_df

### ** Examples

## No test: 
### Download PM10 data for the province (NUTS-3) of Milano (Italy) from January 1st to January 31st, 2023
`%>%` <- dplyr::`%>%`
IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(NUTS3 %in% c("Milano")) %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2023-01-01", to = "2023-01-31", verbose = TRUE)

### Check if the imported object belongs to the EEAaq_df class
is_EEAaq_df(data = data)
## End(No test)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("is_EEAaq_df", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
