library(lubridate)
library(tidyverse)
library(sf)

source("~/GitHub/EEAaq_R/Package/R/EEAaq_get_data.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_get_dataframe.R")
source("~/GitHub/EEAaq_R/Package/R/handle_dates.R")
source("~/GitHub/EEAaq_R/Package/R/code_extr.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_get_stations.R")
source("~/GitHub/EEAaq_R/Package/R/get_stations.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_time_aggregate.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_get_data.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_idw_map.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_map_stations.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_export.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_import.R")
source("~/GitHub/EEAaq_R/Package/R/EEAaq_summary.R")


### Debug EEAaq_get_data

# Prova 0
pollutants = c("PM10","NO2","O3")
from = "2022-01-01"
to = "2022-01-31"
verbose = TRUE
IDstations <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
IDstations <- IDstations %>%
  dplyr::filter(NUTS3 %in% c("Milano"),
                ISO %in% c("IT")) %>%
  dplyr::pull(AirQualityStationEoICode) %>%
  unique()
prova0 <- EEAaq_get_data(IDstations = IDstations, pollutants = pollutants, from = from, to = to, verbose = verbose)


### Prova 1
pollutants = c("PM10")
from = "2022-01-01"
to = "2022-12-31"
verbose = TRUE
Stats <- EEAaq_get_stations(byStation = FALSE, complete = TRUE)
ITC_PM10 <- Stats %>%
  dplyr::filter(NUTS1 %in% c("Nord-Ovest"),
                ISO %in% c("IT"),
                is.na(OperationalActivityEnd),
                OperationalActivityBegin < "2022-01-01") %>%
  dplyr::pull(AirQualityStationEoICode) %>%
  unique()
ITH_PM10 <- Stats %>%
  dplyr::filter(NUTS1 %in% c("Nord-Est"),
                ISO %in% c("IT"),
                is.na(OperationalActivityEnd),
                OperationalActivityBegin < "2022-01-01") %>%
  dplyr::pull(AirQualityStationEoICode) %>%
  unique()

ITC_dataPM10 <- EEAaq_get_data(IDstations = ITC_PM10, pollutants = pollutants, from = from, to = to, verbose = verbose)
ITH_dataPM10 <- EEAaq_get_data(IDstations = ITH_PM10, pollutants = pollutants, from = from, to = to, verbose = verbose)
length(unique(ITC_dataPM10$AirQualityStationName)) + length(unique(ITH_dataPM10$AirQualityStationName))
ITCH_dataPM10 <- EEAaq_get_data(IDstations = c(ITC_PM10,ITH_PM10), pollutants = pollutants, from = from, to = to, verbose = verbose)
length(unique(ITCH_dataPM10$AirQualityStationName))

table(ITCH_dataPM10$AveragingTime,ITCH_dataPM10$AveragingTime)
table(ITCH_dataPM10$AirQualityStationEoICode,ITCH_dataPM10$AveragingTime)

ITCH_dataPM10_agr <- ITCH_dataPM10 %>%
  mutate(y = year(DatetimeBegin),
         m = month(DatetimeBegin),
         d = day(DatetimeBegin)) %>%
  group_by(AirQualityStationEoICode,AirQualityStationName,y,m,d) %>%
  summarise(PM10 = mean(PM10,na.rm=T)) %>%
  ungroup()

ITCH_dataPM10_agr %>%
  group_by(AirQualityStationEoICode,AirQualityStationName) %>%
  summarise(NAs = sum(is.na(PM10))) %>%
  View()


### NUTS 0  sempre senza citta
prova1 <- EEAaq(zone_name =c("Italia"), NUTS_level =c("NUTS0"), pollutants = c("NO2"), from = "2022-01-01", to = "2024-03-02", verbose = TRUE )


### LAU  con citta. in LAU si ottengono solo stazioni all'interno confini comunali.
# es Milano LAU avrà meno stazioni di Milano NUTS 3 (prova3)
prova2 <- EEAaq(zone_name =c("Milano"), NUTS_level =c("LAU"), LAU_ISO = "IT", pollutants = c("NO2"), from = "2022-01-01", to = "2024-03-02", verbose = TRUE )

### NUTS 3 con citta
prova3 <- EEAaq(zone_name =c("Milano"), NUTS_level =c("NUTS3"), pollutants = c("NO2"), from = "2022-01-01", to = "2024-03-02", verbose = TRUE )

### LAU senza citta
prova4 <- EEAaq(zone_name =c("7023"), NUTS_level =c("LAU"), LAU_ISO = "IT", pollutants = c("NO2"), from = "2022-01-01", to = "2024-03-02", verbose = TRUE )

### NUTS 1 e NUTS 2 sono sempre senza citta
prova5 <- EEAaq(zone_name =c("Nord-Ovest"), NUTS_level =c("NUTS1"), pollutants = c("NO2"), from = "2022-01-01", to = "2024-03-02", verbose = TRUE )


# 270?
ITC_dataPM10 <- EEAaq(zone_name = "Nord-Ovest", NUTS_level = "NUTS1", LAU_ISO = "IT",
                      pollutant = "PM10", from = "2022-01-01", to = "2023-12-31", verbose = T)
ITH_dataPM10 <- EEAaq(zone_name = "Nord-Est", NUTS_level = "NUTS1", LAU_ISO = "IT",
                      pollutant = "PM10", from = "2022-01-01", to = "2023-12-31", verbose = T)
length(unique(ITC_dataPM10$AirQualityStationName))+length(unique(ITH_dataPM10$AirQualityStationName))


### parametro stazione
esempi <- EEAaq_get_stations(byStation = TRUE, complete = FALSE)
# esempi <- esempi %>% dplyr::filter(NUTS2 %in% "Prov. West-Vlaanderen") %>% dplyr::pull(AirQualityStationEoICode) %>% unique()
esempi <- esempi %>% dplyr::filter(NUTS1 %in% c("Nord-Ovest","Nord-Est"), ISO %in% c("IT")) %>% dplyr::pull(AirQualityStationEoICode) %>% unique()
prova6 <- EEAaq(station = esempi, pollutants = c("PM10"), from = "2022-01-01", to = "2022-01-31", verbose = TRUE )




###################################
########## EEAaq_idw_map ##########
###################################
# Download NO2 measurement for the city (LAU) of Milano (Italy) from January 1st to December 31st, 2023
IDstations <- EEAaq_get_stations(byStation = FALSE, complete = FALSE)
IDstations <- IDstations %>%
                dplyr::filter(NUTS3 == "Milano") %>%
                dplyr::pull(AirQualityStationEoICode) %>%
                unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "NO2",
                       from = "2023-01-01", to = "2023-03-31", verbose = TRUE)

# Weekly aggregation
t_aggr <- EEAaq_time_aggregate(data = data, frequency = "weekly",
                               aggr_fun = c("mean", "sd"))

# Monthly aggregation
t_aggr <- EEAaq_time_aggregate(data = data, frequency = "monthly",
                               aggr_fun = c("mean", "min", "max"))
data = t_aggr

NUTS_filler <- "NUTS3"
NUTS_extborder <- "NUTS2"
NUTS_intborder <- "LAU"

pollutant = "NO2"
aggr_fun = "mean"

distinct = TRUE
gradient = FALSE

idp = 2
nmax = NULL
maxdist = NULL
tile = "Esri.WorldGrayCanvas"
save = NULL
filepath = NULL
width = 1280
height = 720
res = 144
delay = 1
verbose = TRUE

EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
              distinct = TRUE, gradient = FALSE,
              dynamic = FALSE,
              NUTS_filler = "NUTS3", NUTS_extborder = "NUTS2", NUTS_intborder = "LAU"
              )

EEAaq_idw_map(data = t_aggr, pollutant = "NO2", aggr_fun = "mean",
              distinct = TRUE, gradient = FALSE,
              dynamic = TRUE,
              NUTS_filler = "LAU", NUTS_extborder = "NUTS2", NUTS_intborder = "LAU")



##### EEAaq_idw_map
# Download NO2 measurement for the city (LAU) of Milano (Italy) from January 1st to December 31st, 2023
IDstations <- EEAaq_get_stations(byStation = FALSE, complete = FALSE)
IDstations <- IDstations %>%
  dplyr::filter(ISO %in% c("BE"),
                AirPollutant %in% "PM10") %>%
  dplyr::pull(AirQualityStationEoICode) %>%
  unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2021-12-01", to = "2021-12-31", verbose = TRUE)
# Weekly aggregation
t_aggr <- EEAaq_time_aggregate(data = data, frequency = "weekly",
                               aggr_fun = c("mean", "min", "max"))

EEAaq_idw_map(data = t_aggr, pollutant = "PM10", aggr_fun = "mean",
              distinct = distinct, gradient = gradient,
              dynamic = FALSE,
              NUTS_filler = "NUTS3", NUTS_extborder = "NUTS2", NUTS_intborder = "LAU")

EEAaq_idw_map(data = t_aggr, pollutant = "PM10", aggr_fun = "max",
              distinct = distinct, gradient = gradient,
              dynamic = TRUE,
              NUTS_filler = NULL, NUTS_extborder = "NUTS2", NUTS_intborder = "LAU")



########################################
########## EEAaq_map_stations ##########
########################################

IDstations <- EEAaq_get_stations(byStation = FALSE, complete = FALSE)
IDstations <- IDstations %>%
  dplyr::filter(ISO %in% c("BE"),
                AirPollutant %in% "PM10") %>%
  dplyr::pull(AirQualityStationEoICode) %>%
  unique()
data <- EEAaq_get_data(IDstations = IDstations, pollutants = "PM10",
                       from = "2021-12-01", to = "2021-12-31", verbose = TRUE)

NUTS_extborder = "NUTS2"
NUTS_intborder = "LAU"

EEAaq_map_stations(data = data,
                   NUTS_extborder = "NUTS2",
                   NUTS_intborder = "LAU",
                   color = TRUE, dynamic = FALSE)

EEAaq_map_stations(data = data,
                   NUTS_extborder = "NUTS2",
                   NUTS_intborder = "NUTS3",
                   color = TRUE, dynamic = TRUE)



#Download a dataset with the function EEAaq_get_data, which generate an EEAaq_df object.
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
