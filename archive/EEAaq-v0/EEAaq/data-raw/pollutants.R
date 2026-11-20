EEAaq_get_pollutants <- function() {
  #download dei dati 
  pollutants <- readr::read_csv(unz("pollutant.zip", "pollutant.csv"), 
                                col_select = c("URI", "Label", "Definition", "Notation", "Status"), 
                                show_col_types = F, name_repair = "minimal")
  #Funzione che estrae il codice dell'inquinante dalla riga 'URI'
  code <- function(x) as.integer(strsplit(substr(x, nchar(x)-5, nchar(x)), "/")[[1]][2])
  pollutants <- pollutants %>% dplyr::mutate(URI = apply(dplyr::select(pollutants,URI), 1, code)) %>% dplyr::rename(Code = URI)
  pollutants <- pollutants %>% dplyr::select(Code, Notation, Label, Definition, Status)
  pollutants <- pollutants %>% tidyr::separate(col = "Definition", into = c("Definition", "RecommendedUnit"), sep = "- recommended unit: ") %>% suppressWarnings() %>% dplyr::select(-Definition, -Status)
  
  return(pollutants) 
}

pollutants <- EEAaq_get_pollutants()


usethis::use_data(pollutants, overwrite = TRUE)
