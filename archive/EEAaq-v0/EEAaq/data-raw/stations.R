EEAaq_get_stations <- function(byStation = F, complete = T) {


  #Download dei dataset NUTS e LAU
  temp <- tempfile()
  utils::download.file("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/LAU.rds", temp, quiet = T)
  LAU <- readRDS(temp)
  temp <- tempfile()
  utils::download.file("https://github.com/AgostinoTassanMazzocco/EEAaq/raw/main/NUTS.rds", temp, quiet = T)
  NUTS <- readRDS(temp)


  #Download dei metadati disponibili a questo link: https://discomap.eea.europa.eu/App/AirQualityMeasurements/index.html
  #temp <- tempfile()
  #utils::download.file("https://drive.google.com/uc?export=download&id=1Y8wM64bWV5fReEgW4vJ9bNMeh2ji0zyk",temp)
  #stations <- readr::read_csv(unz(temp, "DataExtract.csv"), show_col_types = F)
  #unlink(temp)

  #stations <- read_csv("C:/Users/Agostino/Desktop/TESI/Datasets EEA/DataExtract.csv", show_col_types = F)
  #stations <- readr::read_csv("stations.csv", show_col_types = F)
  stations <- readr::read_csv(unz("stations.zip", "stations.csv"), show_col_types = F)

  colnames(stations) = gsub(pattern = " " , replacement = "", x = colnames(stations))
  "%>%" <- dplyr::`%>%`
  #Aggiungo il codice ISO degli stati alle centraline
  stations <- stations %>% dplyr::mutate(ISO = dplyr::case_when(Country == "Andorra" ~ "AD", Country == "Albania" ~ "AL", Country == "Austria" ~ "AT",
                                                                Country == "Bosnia and Herzegovina" ~ "BA", Country == "Belgium" ~ "BE", Country == "Bulgaria" ~ "BG", Country == "Switzerland" ~ "CH",
                                                                Country == "Cyprus" ~ "CY", Country == "Czechia" ~ "CZ", Country == "Germany" ~ "DE", Country == "Denmark" ~ "DK", Country == "Estonia" ~ "EE",
                                                                Country == "Spain" ~ "ES", Country == "Finland" ~ "FI", Country == "France" ~ "FR", Country == "United Kingdom" ~ "UK", Country == "Georgia" ~ "GE",
                                                                Country == "Greece" ~ "EL", Country == "Croatia" ~ "HR", Country == "Hungary" ~ "HU", Country == "Ireland" ~ "IE", Country == "Iceland" ~ "IS",
                                                                Country == "Italy" ~ "IT", Country == "Lithuania" ~ "LT", Country == "Luxembourg" ~ "LU", Country == "Latvia" ~ "LV", Country == "Montenegro" ~ "ME",
                                                                Country == "North Macedonia" ~ "MK", Country == "Malta" ~ "MT", Country == "Netherlands" ~ "NL", Country == "Norway" ~ "NO", Country == "Poland" ~ "PL",
                                                                Country == "Portugal" ~ "PT", Country == "Romania" ~ "RO", Country == "Serbia" ~ "RS", Country == "Sweden" ~ "SE", Country == "Slovenia" ~ "SI",
                                                                Country == "Slovakia" ~ "SK", Country == "Türkiye" ~ "TR", Country == "Ukraine" ~ "UA", Country == "Kosovo under UNSCR 1244/99" ~ "XK"))

  #Aggiungo la variabile SamplingPointStatus per differenziare tra i sensori  attivi e chiusi
  #Questa variabile è disponibile nel sistema di filtraggio del data viewer del precedente link commentato ma non nei dati scaricabili
  stations <- stations %>% dplyr::mutate(SamplingPointStatus = ifelse(is.na(OperationalActivityEnd), "active", "closed"))

  #Creo un dizionario in cui per ogni centralina (id: AirQualityStationEoICode) assegno longitudine e latitudine.
  #Per alcune centraline, i sensori presentavano valori leggermente diversi di lat e long, pur appartenendo alla stessa centralina, per cui ho
  #assegnato a queste centraline un unico valore di lat e long, quello più frequente tra i sensori
  dict <- stations %>% dplyr::filter(!is.na(Longitude)) %>% dplyr::group_by(AirQualityStationEoICode, AirQualityStationNatCode, AirQualityStationName, Longitude, Latitude) %>% dplyr::count() %>%
    dplyr::ungroup() %>% dplyr::group_by(AirQualityStationEoICode, AirQualityStationNatCode, AirQualityStationName) %>% dplyr::slice_max(order_by = n, with_ties = F) %>% dplyr::ungroup() %>% dplyr::select(-n)
  stations <- stations %>% dplyr::select(-Longitude, -Latitude)
  #stations <- dplyr::left_join(stations, dict, by = c("AirQualityStationEoICode", "AirQualityStationNatCode", "AirQualityStationName"))
  "%notin%" <- Negate("%in%")
  #Seleziono le centraline per cui non sono disponibili le coordinate (NA) e provo a ottenere questi valori
  #da altri metadati disponibili a questo link: https://discomap.eea.europa.eu/map/fme/AirQualityExport.htm

  #Seleziono i codici delle centraline di cui cercare le coordinate
  #cod <- stations %>% dplyr::filter(is.na(Longitude)) %>% dplyr::select(AirQualityStationEoICode, AirQualityStationNatCode, Longitude, Latitude) %>%
  #  dplyr::distinct(AirQualityStationEoICode, AirQualityStationNatCode) %>% dplyr::filter(AirQualityStationEoICode %notin% dict$AirQualityStationEoICode & AirQualityStationNatCode %notin% dict$AirQualityStationNatCode)
  #Alcune coordinate delle centraline italiane sono errate, per cui le seleziono e le correggo con quelle vere prese dai nuovi metadati
  #cod <- dplyr::bind_rows(cod, select(filter(dict, Longitude == 0.0001 & Latitude == 0.0004), AirQualityStationEoICode, AirQualityStationNatCode))
  #dict <- dict %>% filter(Longitude != 0.0001 & Latitude != 0.0004)

  #Download dei metadati
  #temp <- tempfile()
  #utils::download.file("https://drive.google.com/uc?export=download&id=1B1gm8qj4_sBnaJBtzh-Hl9GcA73CZCAV",temp)
  #Metadata <- readr::read_delim(unz(temp, "PanEuropean_metadata.csv"), delim = "\t", show_col_types = F)
  #unlink(temp)
  #dict2 <- Metadata %>% dplyr::filter(AirQualityStationEoICode %in% cod$AirQualityStationEoICode & AirQualityStationNatCode %in% cod$AirQualityStationNatCode) %>% dplyr::group_by(AirQualityStationEoICode, AirQualityStationNatCode, Longitude, Latitude) %>% dplyr::count() %>%
  #  dplyr::ungroup() %>% dplyr::group_by(AirQualityStationEoICode, AirQualityStationNatCode) %>% dplyr::slice_max(order_by = n, with_ties = F) %>% dplyr::ungroup() %>% dplyr::select(-n)

  #Aggiungo al dizionario le coordinate trovate
  #dict <- dict %>% dplyr::select(-AirQualityStationName)
  #dict <- dplyr::bind_rows(dict, dict2)

  #Completo i metadati aggiungendo tutte le coordinate disponibili
  #stations <- stations %>% dplyr::select(-Longitude, -Latitude)
  stations <- dplyr::left_join(stations, dict, by = c("AirQualityStationEoICode", "AirQualityStationNatCode", "AirQualityStationName"))

  #Aggiungo le geometrie al dizionario
  dict <- sf::st_as_sf(dict, coords = c("Longitude", "Latitude"), crs = 4326)

  #Aggiungo le variabili che andranno riempite con i valori dei NUTS e dei LAU
  dict <- dict %>% dplyr::mutate(NUTS1 = NA, NUTS1_ID = NA, NUTS2 = NA, NUTS2_ID = NA, NUTS3 = NA, NUTS3_ID = NA, LAU = NA, LAU_ID = NA)

  #Cerco in quale poligono si colloca ciascun punto e riempio le colonne in base al risultato
  ind <- sf::st_intersects(dplyr::pull(dict[,"geometry"]), dplyr::filter(NUTS, LEVL_CODE == 3), sparse = T)
  dict[as.logical(apply(as.matrix(ind), 1, sum)),c("NUTS3","NUTS3_ID")] <- dplyr::filter(dplyr::as_tibble(NUTS), LEVL_CODE == 3)[unlist(ind), c("NAME_LATN", "NUTS_ID")]
  ind <- sf::st_intersects(dplyr::pull(dict[,"geometry"]), dplyr::filter(NUTS, LEVL_CODE == 2), sparse = T)
  dict[as.logical(apply(as.matrix(ind), 1, sum)),c("NUTS2","NUTS2_ID")] <- dplyr::filter(dplyr::as_tibble(NUTS), LEVL_CODE == 2)[unlist(ind),  c("NAME_LATN", "NUTS_ID")]
  ind <- sf::st_intersects(dplyr::pull(dict[,"geometry"]), dplyr::filter(NUTS, LEVL_CODE == 1), sparse = T)
  dict[as.logical(apply(as.matrix(ind), 1, sum)),c("NUTS1","NUTS1_ID")] <- dplyr::filter(dplyr::as_tibble(NUTS), LEVL_CODE == 1)[unlist(ind), c("NAME_LATN", "NUTS_ID")]
  ind <- sf::st_intersects(dplyr::pull(dict[,"geometry"]), LAU, sparse = T)
  dict[as.logical(apply(as.matrix(ind), 1, sum)),c("LAU","LAU_ID")] <- dplyr::as_tibble(LAU)[unlist(ind), c("LAU_NAME", "LAU_ID")]

  #Infine aggrego le informazioni ottenute al dataset iniziale
  #dict <- dict %>% dplyr::select(-geometry)
  dict <- dict %>% sf::st_drop_geometry()
  stations <- dplyr::left_join(stations, dict, by = c("AirQualityStationEoICode", "AirQualityStationNatCode", "AirQualityStationName"))
  #stations <- stations[,-which(apply(stations, 2, VIM::countNA) >= 0.75*nrow(stations))] %>% select(Country, ISO, SamplingPointId, AirQualityStationEoICode,AirQualityStationNatCode,
  #                                                                                                  AirQualityStationName, AirPollutant, OperationalActivityBegin, OperationalActivityEnd,
  #                                                                                                  SamplingPointStatus, Longitude, Latitude, Altitude, starts_with("NUTS"), starts_with("LAU"), AirQualityStationArea, AirQualityStationType, everything())
  stations <- stations %>% dplyr::select(Country, ISO, SamplingPointId, AirQualityStationEoICode,AirQualityStationNatCode,
                                  AirQualityStationName, AirPollutant, OperationalActivityBegin, OperationalActivityEnd,
                                  SamplingPointStatus, Longitude, Latitude, Altitude, starts_with("NUTS"), starts_with("LAU"), AirQualityStationArea, AirQualityStationType, everything()) %>%
    dplyr::distinct()

  stations$Timezone <- stringr::str_to_upper(stations$Timezone)
  stations$AirQualityStationName <- stringr::str_to_title(stations$AirQualityStationName)
  stations$AirQualityStationName <- gsub(pattern = "\"", replacement = "", x = stations$AirQualityStationName)
  stations$AirQualityStationName <- gsub(pattern = "\n", replacement = "", x = stations$AirQualityStationName)

  if(byStation == T) {
    stations <- stations %>% dplyr::distinct(AirQualityStationEoICode, .keep_all = T) %>%
      dplyr::select(AirQualityStationEoICode, AirQualityStationNatCode, AirQualityStationName, AirQualityStationArea, AirQualityStationType, Longitude, Latitude, Altitude, ISO, NUTS1, NUTS1_ID,
                    NUTS2, NUTS2_ID, NUTS3, NUTS3_ID, LAU, LAU_ID, AirQualityNetwork, AirQualityNetworkName,
                    Timezone)
    if(complete == F) {
      stations <- stations %>% dplyr::select(AirQualityStationEoICode, AirQualityStationNatCode, AirQualityStationName, AirQualityStationArea, AirQualityStationType, Longitude, Latitude, Altitude, ISO, NUTS1,
                                             NUTS2, NUTS3, LAU)
    }
  } else if(byStation == F & complete == F) {
    stations <- stations %>% dplyr::select(SamplingPointId, AirQualityStationEoICode, AirQualityStationName, AirPollutant, OperationalActivityBegin, OperationalActivityEnd, ISO, NUTS1, NUTS2, NUTS3, LAU, AirQualityStationArea, AirQualityStationType, Longitude, Latitude, Altitude, Timezone)
  }

  return(stations)

}
stations <- EEAaq_get_stations()

usethis::use_data(stations, overwrite = TRUE, compress = "xz")

