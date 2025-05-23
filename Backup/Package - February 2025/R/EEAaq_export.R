#' Export and save an \code{EEAaq_df} class object
#'
#' \code{EEAaq_export} saves an \code{EEAaq_df} class object as a \emph{.csv} or a \emph{.txt} file,
#' and exports the associated shapefile as well.
#' @param data an \code{EEAaq_df} class object.
#' @param filepath character string giving the file path
#' @param format character string giving the format of the file. It must be one of 'csv' and 'txt'.
#' @param shape logical value (T or F). If \code{TRUE} the shapefile associated to the \code{EEAaq_df} object
#' given in input is saved in the same directory specified in \code{filepath}. If \code{FALSE} (the default),
#' only the data frame containing the data is saved.
#'
#' @return No return value, called for side effects.
#' @examples
#' \donttest{
#' #Download a dataset with the function EEAaq_get_data, which generate an EEAaq_df object.
#' data <- EEAaq_get_data(zone_name = "15146", NUTS_level = "LAU",
#'                        LAU_ISO = "IT", pollutants = "PM10",
#'                        from = "2023-01-01", to = "2023-05-31")
#' temp <- tempdir()
#' filepath <- paste0(temp, "/data.csv")
#' EEAaq_export(data = data, filepath = filepath, format = "csv", shape = TRUE)
#' }
#' @export

EEAaq_export <- function(data, filepath, format, shape = FALSE) {

  `%>%` <- dplyr::`%>%`
  "%notin%" <- Negate("%in%")

  #Verifica connessione a internet
  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.")
  }


  #Download dei dataset NUTS e LAU

  LAU <- EEAaq_get_dataframe(dataframe = "LAU")

  NUTS <- EEAaq_get_dataframe(dataframe = "NUTS")

  #Se l'oggetto non e' di classe EEAaq_df errore
  stopifnot("The given object for the parameter data is not an 'EEAaq_df' class object" =
              "EEAaq_df" %in% class(data) | "EEAaq_df_sfc" %in% class(data))

  #Se non deve essere salvato lo shape, viene salvato solo il data.frame
  if(format == "csv") {
    utils::write.csv(x = data, file = filepath, row.names = F)
  } else if(format == "txt") {
    utils::write.table(x = data, file = filepath, row.names = F)
  }



  #Salvataggio dello shape nel caso in cui il dataset sia di classe EEAaq_df_sfc
  if(shape == T & "EEAaq_df_sfc" %in% class(data)) {
    if("data.frame" %notin% class(attributes(data)$zone_geometry)) {
      sf::st_write(obj = attributes(data)$zone_geometry, dsn = paste0(substr(filepath, 1, nchar(filepath)-4), ".shp"), quiet = T)
    } else {
      sf::st_write(obj = attributes(data)$zone_geometry[,!nchar(colnames(attributes(data)$zone_geometry))>10], dsn = paste0(substr(filepath, 1, nchar(filepath)-4), ".shp"), quiet = T)
    }
  } else if(shape == T & "EEAaq_df_sfc" %notin% class(data)) {
    if(attributes(data)$NUTS_level == "LAU") {

      sh <-  LAU   %>% as.data.frame() %>% dplyr::filter((.data$LAU_NAME %in% attributes(data)$zone_name |
                                                            .data$LAU_ID %in% attributes(data)$zone_name) &
                                                           .data$ISO %in% attributes(data)$countries)  %>% sf::st_as_sf()
    } else {
      lev <- code_extr(level = attributes(data)$NUTS_level)
      sh <- NUTS  %>% as.data.frame() %>% dplyr::filter(.data$LEVL_CODE == lev & (
        .data$NAME_LATN %in% attributes(data)$zone_name  | .data$NUTS_ID %in% attributes(data)$zone_name )) %>% sf::st_as_sf()

    }
    sf::st_write(obj = sh[,!nchar(colnames(sh))>10], dsn = paste0(substr(filepath, 1, nchar(filepath)-4), ".shp"), quiet = T, append = FALSE)
  }

}


