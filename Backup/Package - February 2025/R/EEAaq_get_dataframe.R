#'@title EEAaq_get dataframe
#'
#'@description Retrieve one of the datasets (LAU, NUTS, stations, or pollutant)
#'This function downloads and loads one dataset at a time from a predefined list of available datasets.
#'Ensure that the dataset name is written correctly.
#' @param dataframe name of the dataframe to retireve
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' \donttest{
#' LAU <- EEAaq_get_dataframe(dataframe= "LAU")}
#'
#'
EEAaq_get_dataframe <- function(dataframe = NULL){

  if(!curl::has_internet()) {
    stop("Please check your internet connection. If the problem persists, please
         contact the package maintainer.") #se false, stop interrompe esecuzione
  }

  file_urls <- list(
    LAU = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/LAU.rds",
    NUTS = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/NUTS.rds",
    stations = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/stations.rds",
    pollutant = "https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/pollutant.rds"
  )
  # Controlla se il dataframe richiesto e valido
  if (!dataframe %in% names(file_urls)) {
    stop("The requested dataframe is not available. Please choose one of the following: ",
         paste(names(file_urls), collapse = ", "), ".")
  }

  # Scarica il file corrispondente
  temp <- tempfile()
  res <- curl::curl_fetch_disk(file_urls[[dataframe]], temp)

  if (res$status_code == 200) {
    result <- readRDS(temp)
    return(result)
  } else {
    stop("The internet resource is not available at the moment. Try again later. If the problem persists, contact the package mainteners.")
  }
}
