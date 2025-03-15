#' Download air quality data at european level from the EEA download service
#'
#' This function imports air quality datasets at european level, based on the zone, time and pollutant specifications.
#' This function generates an \code{EEAaq_df} object, or an \code{EEAaq_df_sfc}.
#' @param zone_name character vector specifying the names of the zones to consider. The reference is the NUTS and LAU nomenclature by Eurostat.
#' See \emph{Details}.
#' @param NUTS_level character that specify the level of NUTS or LAU, to which \code{zone_name} belongs.
#' Allowed values are 'NUTS0', 'NUTS1', 'NUTS2', 'NUTS3', 'LAU'.
#' For further information see \emph{Details}.
#' @param pollutants the pollutants for which to download data. It may be:
#' \itemize{
#' \item{character vector representing the short names of the pollutants to analyse. The reference is the
#' variable \code{Notation} in the dataset \code{pollutants} provided by this package.}
#' \item{numeric vector representing the codes of the pollutants to analyse. The reference is the variable \code{Code}
#' in the dataset \code{pollutants} provided by this package.}
#' }
#' @param from the starting point of the time window to consider.
#' It may be:
#' \itemize{
#' \item{character containing a specific day of the year in the format \code{yyyy-mm-dd}}
#' }
#' @param to the ending point of the time window to consider.
#' It may be:
#' \itemize{
#' \item{character containing a specific day of the year in the format \code{yyyy-mm-dd}}
#' }
#'  ID logic value (T or F). If \code{TRUE} (the default), the character specified in the parameter \code{zone_name}
#'  is the unique identifier code provided by Eurostat. The reference is the \code{NUTS_ID}  column from the
#'  \code{NUTS} dataset or the \code{LAU_ID} from the \code{LAU} dataset.
#'  Also, in the case of using \code{LATN_NAME} from the \code{NUTS} dataset or \code{LAU_NAME} from the \code{LAU} dataset, TRUE must be specified.
#' If \code{FALSE} , it is used when \code{polygon} or \code{quadrant} are not null.
#' @param quadrant a list of bidimensional numeric vectors containing the coordinates in \bold{WGS84} format.
#'                  If the list has two elements, the function builds a square using the two coordinates as
#'                  opposite extremes. If the list contains three or more elements, every point is a vertex of a
#'                  polygon, in particular the convex hull of the specified points.
#' @param polygon A \code{sfc_POLYGON} or \code{sfc_MULTIPOLYGON} class object
#' The polygon can be imported via shapefile or other formats from the user.
#' @param verbose logic value (T or F). If \code{TRUE} (the default) information about
#' the function progress are printed. If \code{FALSE} no message is printed.
#' @param LAU_ISO a code to identify the corresponding ISO of the country since LAU_ID are not unique over Europe
#' @details
#' The NUTS classification (Nomenclature of territorial units for statistics) is a hierarchical system for dividing up the economic territory of the EU and the UK.
#' The levels are defined as follows:
#' \itemize{
#' \item{\strong{NUTS 0}: the whole country}
#' \item{\strong{NUTS 1}: major socio-economic regions}
#' \item{\strong{NUTS 2}: basic regions for the application of regional policies}
#' \item{\strong{NUTS 3}: small regions for specific diagnoses}
#' }

#'
#' @return A data frame of class \code{EEAaq_df}, if \code{zone_name} is specified, and of class \code{EEAaq_df_sfc}
#' if whether the parameter \code{quadrant} or \code{polygon} is specified.
#' @examples
#' \donttest{
#'data <- EEAaq_get_data(zone_name = "15146", NUTS_level = "LAU",LAU_ISO = "IT",
#'pollutants = "NO2", from = "2023-01-01", to = "2024-08-29",  verbose = TRUE)}
#' @export
EEAaq <- function(zone_name = NULL, NUTS_level = NULL, LAU_ISO= NULL, station = NULL, pollutants = NULL, from = NULL, to = NULL, quadrant = NULL, polygon= NULL, verbose = TRUE) {

  `%>%` <- dplyr::`%>%`
  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.") #se false, stop interrompe esecuzione
  }

  ########################### CHECK PARAMETRI INSERITI #################################################
  #  stopifnot("You need to specify both the zone_name and the NUTS_level" = (!is.null(zone_name) & !is.null(NUTS_level)) | (is.null(zone_name) & is.null(NUTS_level)))
  # se NUTS_level viene specificato allora deve esserci anche il parametro zone_name. non vale contrario
  # stopifnot("If you specify NUTS_level, you must also specify zone_name" = is.null(NUTS_level) | !is.null(zone_name))

  # Controllo: esattamente uno tra `zone_name`, `quadrant`, o `polygon` deve essere specificato
  # stopifnot("Specify either the zone_name, the station, the quadrant or a polygon (sfc_POLYGON object)" = as.numeric(!is.null(polygon))+as.numeric(!is.null(station))+as.numeric(!is.null(quadrant))+as.numeric(!is.null(zone_name)) == 1)


  # Verifica formato delle date
  if (is.null(from) && is.null(to)) {
    stop("You need to specify both from and to ")
  }
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", from) || !grepl("^\\d{4}-\\d{2}-\\d{2}$", to)) {
    stop("Both from and to must be in the format YYYY-MM-DD.")
  }
  if (is.null(pollutants) || length(pollutants) == 0) {
    stop("Nessun pollutant specificato.")
  }

  if(verbose ==  T) {
    cat(paste0("Download preparation started at ", Sys.time(), "\n"))
  }





  ####################################### scarichiamo dataset di interesse
  pollutant <- EEAaq_get_dataframe(dataframe = "pollutant")
  stations <- EEAaq_get_dataframe(dataframe = "stations")
  NUTS <- EEAaq_get_dataframe(dataframe = "NUTS")




  ################################### Pollutants
  # Converte tutto in character per gestire il matching come stringa ed estrae codice numerico in code
  pollutants <- as.character(pollutants)

  matched_codes <- list()
  # Verifica se i nomi degli inquinanti sono specificati nella colonna `Notation`
  if (any(pollutants %in% pollutant$Notation)) {
    matched_codes <- append(
      matched_codes,
      dplyr::pull(dplyr::filter(pollutant, Notation %in% pollutants),Notation)
    )
  }  # Verifica se i codici numerici degli inquinanti sono specificati
  if (any(pollutants %in% as.character(pollutant$Code))) {
    matched_codes <- append(
      matched_codes,
      dplyr::pull(dplyr::filter(pollutant, as.character(Code) %in% pollutants), Notation)
    )
  }
  # Estrae i codici validi e identifica quelli mancanti
  valid_pollutants <- unique(unlist(matched_codes))


  missing_pollutants <- setdiff(pollutants, c(pollutant$Notation, as.character(pollutant$Code)))

  if (length(missing_pollutants) > 0) {
    stop(paste("The following pollutants are not available:", paste(missing_pollutants, collapse = ", ")))
  }
  pollutants <- valid_pollutants


  ################################### GESTIONE ID, NUTS_LEVEL, ZONE_NAME

  # Funzione interna per estrarre il livello di codice basato sulla lunghezza di `NUTS_level`

  # # Gestione dei diversi casi in base a `ID` e `NUTS_level`
  # if (!is.null(NUTS_level)) {
  #   ## Caso: `NUTS_level` include il livello "LAU"
  #   if ( any(NUTS_level == "LAU")) {
  #     # Unifica la logica per `LAU_ID` e `LAU_NAME`
  #     if (is.null(LAU_ISO)  || is.null(NUTS_level) ) {
  #       stop("You must specify `LAU_ISO` when using `LAU_ID` to disambiguate.")
  #     }
  #     countries <-  LAU_ISO %>% unique()
  #
  #     ## nel caso in cui la città è presente
  #     matching_column_val <- stations %>%
  #       dplyr::select("ISO", "LAU_ID", "LAU_NAME", "CITY_NAME", "CITY_ID")%>%
  #       dplyr::filter(LAU_ID %in% zone_name |LAU_NAME %in% zone_name, !is.na(CITY_NAME)) %>%
  #       dplyr::filter(ISO %in%  countries) %>% unique()
  #
  #     #se la citta non è presente
  #     na_samplingpoint <- stations %>%
  #       dplyr::select("ISO", "LAU_ID", "LAU_NAME", "CITY_NAME", "CITY_ID", "SamplingPointId", "AirPollutant")%>%
  #       dplyr::filter(LAU_ID %in% zone_name |LAU_NAME %in% zone_name, is.na(CITY_NAME),ISO %in%  countries,AirPollutant %in% pollutants) %>% dplyr::mutate(SamplingPointId = stringr::str_replace_all(stringr::str_sub(SamplingPointId, 4), ":", "_")) %>% dplyr::pull(SamplingPointId)  %>% unique()
  #
  #
  #     if(length(na_samplingpoint) > 0) {
  #       cat("The following zone_name(s) or station(s) do not have an associated city: ", paste(zone_name, collapse = ", "))
  #
  #
  #     }   else if (nrow(matching_column_val) > 0) {
  #       zone_cities <- matching_column_val %>%  dplyr::pull(CITY_NAME) #citta
  #       #egestione combinazione zone name senza o con citta
  #       LAU_spoint <- stations %>%   dplyr::filter(LAU_NAME %in% matching_column_val$LAU_NAME & AirPollutant %in% pollutants) %>%  dplyr::mutate(SamplingPointId= stringr::str_replace_all(stringr::str_sub(SamplingPointId, 4), ":", "_")) %>% dplyr::pull(SamplingPointId)  %>% unique() #sampleid
  #     } else {
  #       stop("The following zone_name(s) or station(s) is(are) not available ", paste(zone_name, collapse = ", "))
  #     }} else if (all(NUTS_level != "NUTS0")) {
  #       countries <- NUTS %>%
  #         sf::st_drop_geometry() %>%
  #         dplyr::filter(
  #           LEVL_CODE %in% code_extr(NUTS_level),
  #           NUTS_ID %in% zone_name | NAME_LATN %in% zone_name) %>%
  #         dplyr::pull(CNTR_CODE) %>% unique()
  #
  #       #filtra dataframe con tutte le righe di riferimento
  #       stations_city <- stations %>%
  #         dplyr::filter(NUTS1_ID %in% zone_name | NUTS2_ID %in% zone_name | NUTS3_ID %in% zone_name | NUTS1 %in% zone_name | NUTS2 %in% zone_name | NUTS3 %in% zone_name) %>%
  #         dplyr::select("NUTS1_ID","NUTS1", "NUTS2_ID","NUTS2",  "NUTS3_ID","NUTS3", "CITY_ID", "CITY_NAME", "ISO","SamplingPointId")  %>%  dplyr::distinct()
  #
  #       na_samplingpoint <- stations_city %>%  dplyr::filter(is.na(CITY_NAME)) %>%   dplyr::mutate(SamplingPointId= stringr::str_replace_all(stringr::str_sub(SamplingPointId, 4), ":", "_")) %>% dplyr::pull(SamplingPointId)  %>% unique()
  #
  #       if(length(na_samplingpoint) > 0) {
  #         cat("The following zone_name(s) or station(s) do not have an associated city: ", paste(zone_name, collapse = ", "))
  #
  #       } else  if (nrow(stations_city) > 0) {
  #
  #         stations_city <- stations_city %>% dplyr::filter(!is.na(CITY_NAME))
  #
  #         zone_cities <- c()
  #
  #         # Gestione per ID (NUTS1_ID, NUTS2_ID, NUTS3_ID)
  #         if (all(zone_name %in% c(stations$NUTS1_ID, stations$NUTS2_ID, stations$NUTS3_ID))) {
  #           # Aggiungo citta valide
  #
  #           zone_cities <- append(zone_cities, stations_city %>%
  #                                   dplyr::filter(NUTS1_ID %in% zone_name | NUTS2_ID %in% zone_name | NUTS3_ID %in% zone_name) %>%
  #                                   dplyr::pull(CITY_NAME) %>%
  #                                   .[!is.na(.)])
  #         }   else if (all(zone_name %in% c(stations$NUTS1, stations$NUTS2, stations$NUTS3))) {
  #           # Aggiungo citta valide
  #           zone_cities <- append(zone_cities, stations_city %>%
  #                                   dplyr::filter(NUTS1 %in% zone_name | NUTS2 %in% zone_name | NUTS3 %in% zone_name) %>%
  #                                   dplyr::pull(CITY_NAME) %>%
  #                                   .[!is.na(.)])
  #         }
  #         # Rimuovo duplicati
  #         zone_cities <- unique(zone_cities)
  #
  #         # Blocco finale per il controllo
  #         if (length(zone_cities) == 0) {
  #           stop("The specified zone_name is invalid or does not correspond to the given NUTS_level.")
  #         }}
  #
  #     } else if (all(NUTS_level == "NUTS0")){ #COUNTRY
  #       paese <- stations %>%
  #         sf::st_drop_geometry() %>%
  #         dplyr::filter(
  #           NUTS0_ID %in% zone_name  | NUTS0 %in% zone_name | Country %in% zone_name
  #         ) %>%
  #         dplyr::filter(!is.na(ISO)) %>%  # Filtra prima di pull()
  #         dplyr::pull(ISO) %>%
  #         unique()
  #     }
  #   }

  if (!is.null(station)){
    missing_stations <- setdiff(station, stations$AirQualityStationEoICode)

    if (length(missing_stations) > 0) {
      warning("The following stations are not included in stations dataframe : ",
              paste(missing_stations, collapse = ", "))
    }

    filter_stations <- stations %>%
      filter(AirQualityStationEoICode %in% station,.data$AirPollutant %in% pollutants )

    if (all(!is.na(filter_stations$CITY_NAME))){
      zone_cities <- filter_stations %>% distinct(CITY_NAME) %>% dplyr::pull()
    } else {
      na_samplingpoint <- filter_stations %>%  dplyr::mutate(SamplingPointId = stringr::str_replace_all(stringr::str_sub(.data$SamplingPointId, 4), ":", "_")) %>% dplyr::pull(SamplingPointId)  %>% unique()
    }

    countries <- unique(filter_stations$ISO)

  }

  # Gestione di polygon o quadrant

  # if (as.numeric(!is.null(polygon)) + as.numeric(!is.null(quadrant)) == 1) {
  #
  #   if ( !is.null(quadrant) & length(quadrant) == 2) {
  #
  #     angles = data.frame(lon = c(quadrant[[1]][1], quadrant[[2]][1]), lat = c(quadrant[[1]][2], quadrant[[2]][2]))
  #     polygon <- sf::st_as_sfc(sf::st_bbox(sf::st_as_sf(angles, coords = c("lon", "lat"),
  #                                                       crs = 4326)))
  #
  #   } else if (!is.null(quadrant) & length(quadrant) > 2) {
  #
  #     lon <- vector()
  #     lat <- vector()
  #     for (i in 1:length(quadrant)) {
  #       lon[i] <- quadrant[[i]][1]
  #       lat[i] <- quadrant[[i]][2]
  #     }
  #     angles <- data.frame(lon = lon, lat = lat)
  #     # le coordiante in alge vengono convertite in oggetto spaziale e unite se formano geometrie separate
  #     polygon <- sf::st_convex_hull(sf::st_union(sf::st_as_sf(angles, coords = c("lon", "lat"), crs = 4326)))
  #
  #   } else if (!is.null(polygon)) {
  #
  #     polygon <- polygon
  #
  #   } else {
  #     stop("No valid quadrant or polygon provided.")
  #   }
  #
  #   stations <- sf::st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326)
  #   ind <- sf::st_intersects(dplyr::pull(stations[,"geometry"]), polygon, sparse = T)
  #   #definisco rows globalmente per filtrare successivamente i dati
  #   rows <- stations[as.logical(apply(as.matrix(ind), 1, sum)),] #seleziono solo le stazioni che intersecano il rettangolo.
  #   #  Controllo se sono state trovate stazioni
  #   if (nrow(rows) == 0) {
  #     stop("No stations found in the specified area.")
  #   }
  #   # Estrazione di `countries` e `zone_cities`
  #   countries <- rows %>% dplyr::pull(ISO) %>%  .[!is.na(.)]  %>% unique()
  #   if  (all(!is.na(rows$CITY_NAME))){
  #     zone_cities <- rows %>% dplyr::pull(CITY_NAME) %>% .[!is.na(.)] %>% unique()
  #
  #   } else {
  #     zone_cities <- rows %>%
  #       dplyr::filter(ISO %in% countries) %>%
  #       dplyr::pull(CITY_NAME) %>%
  #       .[!is.na(.)] %>%
  #       unique()
  #
  #   }
  #
  # }

  # Gestione date. Separazione in request per dataset (1, 2, 3)
  date_intervals <- handle_dates(from, to)
  # dataset nuovo formato parquet
  requests_apiUrl1 <- date_intervals[base::sapply(date_intervals, function(x) x$dataset %in% c(1, 2))]
  requests_apiUrl2 <- date_intervals[base::sapply(date_intervals, function(x) x$dataset %in% c(3))]

  combined_df1 <- NULL
  combined_df2 <- NULL
  #gestione apiurl1
  if (exists("requests_apiUrl1") && length(requests_apiUrl1) > 0) {

    queries <- list()
    for (idx in base::seq_along(requests_apiUrl1)) {
      dataset <- requests_apiUrl1[[idx]]$dataset
      dateStart <- requests_apiUrl1[[idx]]$dateStart
      dateEnd <- requests_apiUrl1[[idx]]$dateEnd

      if ((exists("na_samplingpoint") && length(na_samplingpoint) > 0) ||
          (exists("paese") && length(paese) > 0)) {
        #cat("Query senza parametro citta ")
        if (exists("paese") && length(paese) > 0) {
          countries <- paese
        }
        json_body <- base::paste0(
          '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
          '"cities": [', "", '],',
          '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
          '"dataset": "', dataset, '",',
          '"dateTimeStart": "', dateStart, '",',
          '"dateTimeEnd": "', dateEnd, '"',
          '}'
        )
      } else {

        json_body <- base::paste0(
          '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
          '"cities": [', base::paste0('"', zone_cities, '"', collapse = ", "), '],',
          '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
          '"dataset": "', dataset, '",',
          '"dateTimeStart": "', dateStart, '",',
          '"dateTimeEnd": "', dateEnd, '"',
          '}'
        )}
      queries[[idx]] <- json_body
    }
    if(verbose ==  T) {
      base::print(queries)
    }

    #richiesta singola (o filtrato o tutto dataset) parquet file non ha bisogno di filtro date
    apiUrl1 <- "https://eeadmz1-downloads-api-appservice.azurewebsites.net/ParquetFile"
    all_data <- list()

    for (idx in base::seq_along(queries)) {
      request_body <- queries[[idx]]

      res <- httr::POST(
        url = apiUrl1,
        body = request_body,
        encode = "raw",
        httr::add_headers("Content-Type" = "application/json")
      )

      if (res$status_code != 200) {
        base::warning("Request failed for idx ", idx, " with status code ", res$status_code)
        next
      }

      # Forza il nome della directory temporanea
      #temp_dir <- base::file.path("C:/Temp/ParquetProcessing", base::paste0("temp_requests_", idx))
      temp_dir <- base::file.path(tempdir(), base::paste0("temp_requests_", idx))
      if (base::dir.exists(temp_dir)) {
        base::unlink(temp_dir, recursive = TRUE)
      }
      base::dir.create(temp_dir, recursive = TRUE)

      temp_zip_file <- base::file.path(temp_dir, "temp_file.zip")
      base::on.exit({ unlink(temp_dir, recursive = TRUE) }, add = TRUE)

      content_raw1 <- httr::content(res, "raw")
      base::writeBin(content_raw1, temp_zip_file)

      # Verifica se il file .zip  valido
      if (!base::file.exists(temp_zip_file)) {
        warning("The zip file was not created correctly for idx =", idx)
        next
      }
      file_info <- base::file.info(temp_zip_file)
      if (file_info$size == 0) {
        warning("The zip file is empty for idx ", idx)
        next
      }

      # Estrai il contenuto del file .zip
      utils::unzip(temp_zip_file, exdir = temp_dir)
      unzipped_files <- base::list.files(temp_dir, recursive = TRUE, full.names = TRUE)


      # Verifica la presenza di file .parquet
      parquet_files <- base::list.files(temp_dir, pattern = "\\.parquet$", full.names = TRUE, recursive = TRUE)
      if (length(parquet_files) == 0) {
        warning("The zip file is empty for idx ", idx)
        next
      }

      ##########################################################################################################filtro se na_sampling > 0
      if (exists("na_samplingpoint") && !is.null(na_samplingpoint) && length(na_samplingpoint) > 0) {
        parquet_files <- parquet_files[stringr::str_detect(
          basename(parquet_files),
          paste(na_samplingpoint, collapse = "|") #converte na_samplingpoint in una sola stringa separando con operatore or
        )]}

      if (exists("LAU_spoint") && !is.null(LAU_spoint) && length(LAU_spoint) > 0) {
        parquet_files <- parquet_files[stringr::str_detect(
          basename(parquet_files),
          paste(LAU_spoint, collapse = "|") #converte na_samplingpoint in una sola stringa separando con operatore or
        )]}

      ##########################################################################################################f

      if(verbose ==  T) {
        #base::print(parquet_files)
      }

      data_for_request <- base::do.call(base::rbind, base::lapply(parquet_files, function(file) {
        arrow::read_parquet(file)
      }))

      all_data[[idx]] <- data_for_request
    }

    combined_df1 <- dplyr::bind_rows(all_data)
    if ("FkObservationLog" %in% colnames(combined_df1)) {
      combined_df1 <- combined_df1 %>% dplyr::select(-FkObservationLog)
    }
    #print(paste("Dati combinati: ", nrow(combined_df), " righe."))
  }

  # Gestione apiUrl2
  if (exists("requests_apiUrl2") && length(requests_apiUrl2) > 0) {
    apiUrl2 <- "https://eeadmz1-downloads-api-appservice.azurewebsites.net/ParquetFile/urls"

    dateStart <- requests_apiUrl2[[1]]$dateStart
    dateEnd <- requests_apiUrl2[[1]]$dateEnd



    if ((exists("na_samplingpoint") && length(na_samplingpoint) > 0) ||
        (exists("paese") && length(paese) > 0)) {
      # cat("Query senza parametro citta ")
      if (exists("paese") && length(paese) > 0) {
        countries <- paese
      }
      json_body <- base::paste0(
        '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
        '"cities": [', "", '],',
        '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
        '"dataset": "', 3, '",',
        '"dateTimeStart": "', dateStart, '",',
        '"dateTimeEnd": "', dateEnd, '"',
        '}'
      ) }  else {
        json_body <- base::paste0(
          '{"countries": [', base::paste0('"', countries, '"', collapse = ", "), '],',
          '"cities": [', base::paste0('"', zone_cities, '"', collapse = ", "), '],',
          '"pollutants": [', base::paste0('"', pollutants , '"', collapse = ", "), '],',
          '"dataset": "', 3, '",',
          '"dateTimeStart": "', dateStart, '",',
          '"dateTimeEnd": "', dateEnd, '"',
          '}'
        )}



    print(json_body)
    # Invia la richiesta POST
    res <- httr::POST(
      url = apiUrl2,
      body =  json_body ,
      encode = "raw",
      httr::add_headers("Content-Type" = "application/json")
    )

    # Estrai gli URL dei file .parquet dalla risposta
    content_raw <- httr::content(res, as = "text", encoding = "UTF-8")
    #estrae primo elemento dopo strip
    lines <- base::strsplit(content_raw, "\r\n")[[1]]
    #Controllo degli URL validi
    parquet_urls <- base::grep("\\.parquet$", lines, value = TRUE)
    if (base::length(parquet_urls) == 0) stop("No valid URL found in the answer.")

    # Usa una directory temporanea fissa per scaricare i file
    base_temp_dir <- base::file.path(tempdir(), "ParquetFiles")
    #base_temp_dir <- "C:/Temp/ParquetFiles"
    # Svuota la directory temporanea se esiste
    if (base::dir.exists(base_temp_dir)) {
      base::unlink(base_temp_dir, recursive = TRUE)
    }
    base::dir.create(base_temp_dir, recursive = TRUE)
    downloaded_files <- base::character()
    # Itera sugli URL validi e scarica ciascun file nella directory temporanea
    for (i in base::seq_along(parquet_urls)) {
      dest_file <- base::file.path(base_temp_dir, base::basename(parquet_urls[i]))
      tryCatch({
        utils::download.file(parquet_urls[i], dest_file, mode = "wb", quiet = TRUE)
        downloaded_files <- base::c(downloaded_files, dest_file)  # Aggiungi il file scaricato

      }, error = function(e) {
        base::warning("Error while downloading ", parquet_urls[i], ": ", e$message)
      })
    }
    print( downloaded_files)
    ##################################################################################
    if (exists("na_samplingpoint") && !is.null(na_samplingpoint) && length(na_samplingpoint) > 0) {
      patterns <- stringr::str_sub(na_samplingpoint, 1, 30)
      downloaded_files <- downloaded_files[stringr::str_detect(
        basename(downloaded_files),
        paste(patterns, collapse = "|") #converte na_samplingpoint in una sola stringa separando con operatore or
      )]}
    cat( "filtrati", downloaded_files)
    #########################################################################################

    if (base::length(downloaded_files) == 0) base::stop("No files successfully downloaded.")
    if(verbose ==  T) {
      #base::print(downloaded_files)
    }
    # Converti `dateStart` e `dateEnd` in formato POSIXct
    dateStart <- base::as.POSIXct(dateStart,format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    dateEnd <- base::as.POSIXct(dateEnd, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    combined_df2 <- arrow::open_dataset(downloaded_files) %>%
      dplyr::filter(Start >= dateStart & End <= dateEnd) %>%
      dplyr::collect()

  }
  combined_df <- dplyr::bind_rows(combined_df1, combined_df2)
  if (nrow(combined_df) == 0) {
    base::warning("Combined data frame is empty.")
  }

  # # Filtraggio e join dei dati in base alle condizioni
  # #dopo aver filtrato stations per tenere solo le righe in cui cityname presente in zone_cities, mantengo solo le righe in combined_df che corrispondo alla citta di interesse, poi aggiungo qualche colonna a titolo informativo
  # if ( any(NUTS_level == "LAU")) {
  #   combined_df <- combined_df %>%
  #     dplyr::left_join(
  #       stations %>%
  #         dplyr::select(
  #           SamplingPointId,
  #           AirQualityStationEoICode,
  #           AirQualityStationName
  #         ),
  #       by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
  #     )
  #   if (nrow(combined_df) == 0) {
  #     warning(paste("No data found for the following zone names:",
  #                   paste(zone_name, collapse = ", "), "\n"))}
  # }else if (all(NUTS_level != "NUTS0") & (is.null(quadrant) & is.null(polygon))) {
  #   combined_df <- combined_df %>%
  #     dplyr::left_join(
  #       stations %>%
  #         dplyr::select(
  #           SamplingPointId,
  #           AirQualityStationEoICode,
  #           AirQualityStationName
  #         ),
  #       by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
  #     )
  #   if (nrow(combined_df) == 0) {
  #     warning(paste("No data found for the following zone names:",
  #                   paste(zone_name, collapse = ", "), "\n"))}
  # }    else if (all(NUTS_level == "NUTS0") & (is.null(quadrant) & is.null(polygon))) {
  #   combined_df <- combined_df %>%
  #     dplyr::left_join(
  #       stations %>%
  #         dplyr::select(
  #           SamplingPointId,
  #           AirQualityStationEoICode,
  #           AirQualityStationName
  #         ),
  #       by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
  #     )
  #   if (nrow(combined_df) == 0) {
  #     warning(paste("No data found for the following zone names:",
  #                   paste(zone_name, collapse = ", "), "\n"))}
  # }  else if (!is.null(station)) {
  #   combined_df <- combined_df %>%
  #     dplyr::left_join(
  #       stations %>%
  #         dplyr::select(
  #           SamplingPointId,
  #           AirQualityStationEoICode,
  #           AirQualityStationName
  #         ),
  #       by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
  #     )
  #   if (nrow(combined_df) == 0) {
  #     warning(paste("No data found for the following station:",
  #                   paste(zone_name, collapse = ", "), "\n"))}
  # } else if  (!is.null(quadrant) | !is.null(polygon) ){
  #   combined_df <- combined_df %>%
  #     dplyr::left_join(
  #       stations %>%
  #         dplyr::select(
  #           SamplingPointId,
  #           AirQualityStationEoICode,
  #           AirQualityStationName
  #         ),
  #       by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
  #     )
  #   if (nrow(combined_df) == 0) {
  #     warning(paste("No data found for the following quadrant/polygon:",
  #                   paste(zone_name, collapse = ", "), "\n"))}
  # }
  #
  #
  # combined_df <- combined_df %>%
  #   dplyr::left_join(
  #     stations %>%
  #       dplyr::select(
  #         SamplingPointId,
  #         AirQualityStationEoICode,
  #         AirQualityStationName
  #       ),
  #     by = c("Samplingpoint" = "SamplingPointId")  # Mappatura corretta delle colonne
  #   )




  #filtro per validity
  combined_df <- combined_df %>%
    dplyr::filter(!Validity %in% c(-1, -99)) %>%
    dplyr::select(-any_of(c("DataCapture", "ResultTime"))) %>%
    dplyr::rename(
      DatetimeBegin = Start,
      DatetimeEnd = End,
      AveragingTime = AggType,
      Concentration= Value
    )


  ##Da spostare
  process_combined_df <- function(combined_df, pollutant) {
    #check overwriting tra day e hour
    if (any(duplicated(combined_df[, c("AirQualityStationEoICode", "Pollutant", "DatetimeBegin")]))) {
      combined_df <- combined_df %>%
        dplyr::group_by(AirQualityStationEoICode, Pollutant, DatetimeBegin) %>%
        dplyr::mutate(n_righe = dplyr::n()) %>%
        dplyr::filter(
          n_righe == 1 |
            (n_righe > 1 & AveragingTime == "hour")
        ) %>%
        dplyr::select(-n_righe) %>%
        dplyr::ungroup()
      }

    #gestione dataset to wide
    pollutant_map <- pollutant %>%
      dplyr::mutate(Code = base::as.integer(Code)) %>%
      dplyr::filter(Code %in% base::unique(combined_df$Pollutant)) %>%
      dplyr::select("Code", "Notation", "URI")
    # cat("pollutant_map:",paste0(pollutant_map$Notation), "\n" )

    #stampa pollutants non trovati
    if (all(pollutants %in% c(pollutant$Notation, pollutant$URI))){
      if (all(pollutants %in% pollutant$Notation)){
        diff_notation <- setdiff(pollutants, pollutant_map$Notation)
        if (length(diff_notation) > 0) {
          warning("pollutants not available: ", paste0(diff_notation, collapse = ", "), "\n")
        }
      }
      else  {diff_code <- setdiff(pollutants, pollutant_map$Code)
      if (length(diff_code) > 0) {
        cat("pollutants not available: ", paste0(diff_code, collapse = ", "), "\n")
      }
      }}
    # Separazione dei dati per granularità temporale: "day"
    dayss <- combined_df %>%
      dplyr::select("AirQualityStationEoICode", "Pollutant", "Concentration", "AveragingTime", "DatetimeBegin", "DatetimeEnd","AirQualityStationName") %>%
      dplyr::filter(AveragingTime == "day") %>%
      dplyr::mutate(PollutantName = pollutant_map$Notation[base::match(Pollutant, pollutant_map$Code)]) %>%
      tidyr::pivot_wider(
        names_from = PollutantName,
        values_from = Concentration
      )
    #print(dayss)
    # Separazione dei dati per granularità temporale: "hour"
    hourss <- combined_df %>%
      dplyr::select("AirQualityStationEoICode", "Pollutant", "Concentration", "AveragingTime", "DatetimeBegin", "DatetimeEnd", "AirQualityStationName") %>%
      dplyr::filter(AveragingTime == "hour") %>%
      dplyr::mutate(PollutantName = pollutant_map$Notation[base::match(Pollutant, pollutant_map$Code)]) %>%
      tidyr::pivot_wider(
        names_from = PollutantName,
        values_from = Concentration,
        values_fn=mean
      )
    #print(hourss)
    # Concatenare i risultati dayss e hourss
    combined_df <- dplyr::bind_rows(dayss, hourss) %>%
      dplyr::select(- Pollutant) %>%
      dplyr::relocate(dplyr::where(is.character)) %>%
      dplyr::relocate(c("AirQualityStationEoICode",  "AirQualityStationName"), .before  = "AveragingTime") %>%
      dplyr::relocate(
        dplyr::where(~ typeof(.) == "double" && !inherits(., "POSIXct")), # Solo colonne double NON POSIXct
        .after = "AirQualityStationName"
      )
    return(combined_df)
  }
  combined_df <- process_combined_df(combined_df, pollutant)

  if (all(pollutants %in% pollutant$Notation)) {
    # Se pollutants in forma  Notation
    valid_pollutants <- pollutants[pollutants %in% names(combined_df)]
  } else {
    # Se pollutants in forma  Code, convertilo in Notation
    pollutants <- pollutant %>%
      dplyr::filter(Code %in% pollutants) %>%
      dplyr::pull(Notation)

    # Filtra pollutants disponibili in combined_df
    valid_pollutants <- pollutants[pollutants %in% names(combined_df)]
  }
  # Aggiunge attributi al dataset finale in base ai parametri della query
  if(!is.null(zone_name)) {
    attr(combined_df, "class") <- c("EEAaq_df", "tbl_df", "tbl", "data.frame")
    # attr(combined_df, "NUTS_level") <- NUTS_level
    # attr(combined_df,"zone_name") <- zone_name
    attr(combined_df, "pollutants") <- valid_pollutants
    attr(combined_df, "countries") <- countries
  } else if(!is.null(quadrant) | !is.null(polygon)) {
    attr(combined_df, "class") <- c("EEAaq_df_sfc", "tbl_df", "tbl", "data.frame")
    # attr(combined_df,"zone_geometry") <- polygon
    attr(combined_df, "pollutants") <- valid_pollutants
    attr(combined_df, "countries") <- countries
  }



  #con all non permetto all'utente di inserire NUTS0 e NUTS1 nella stessa query
  return(combined_df)

}
