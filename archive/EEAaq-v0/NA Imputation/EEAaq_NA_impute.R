load("C:/Users/Agostino/Desktop/TESI/applicazione/CASO STUDIO/bc_pm_data.RData")
load("C:/Users/Agostino/Desktop/TESI/applicazione/CASO STUDIO/bc_data.RData")
attributes(bc_pm_data)
data <- bc_pm_data



EEAaq_NA_impute <- function(data, method = "SDEM") {
  
  `%>%` <- dplyr::`%>%`
  "%notin%" <- Negate("%in%")
  
  #if("EEA_AQ_df" %notin% class(data)) {
  #  stop("data should be an EEAaq_df")a
  #}
  
  #Inquinanti presenti all'interno del dataset 
  polls <- attributes(data)$pollutants
  
  #Seleziono i dati orari
  h_data <- data %>% dplyr::filter(AveragingTime == "hour")
  #Aggiungo le variabili week, day e hour
  h_data <- h_data %>% 
    dplyr::mutate(week = lubridate::week(DatetimeBegin),
                  day = lubridate::wday(DatetimeBegin),
                  hour = lubridate::hour(DatetimeBegin) + 1)
  
  
  sel_st <- function(st, data, pollutant) {
    
    data_st <- data %>% filter(AirQualityStationEoICode == st)
    
    if(sum(is.na(data_st[,pollutant])) == nrow(data_st)) {
      return(st)
    }
    
  }
  
  #Funzione SDEM che verrà applicata ad ogni inquinante
  SDEM <- function(pollutant, data) {
    
    #data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
    sel_st <- function(st, data, pollutant) {
      
      data_st <- data %>% filter(AirQualityStationEoICode == st)
      
      if(sum(is.na(data_st[,pollutant])) == nrow(data_st)) {
        return(st)
      }
      
    }
    
    ex_st <- unlist(lapply(unique(data$AirQualityStationEoICode), sel_st, data, pollutant))
    
    data <- data %>% filter(AirQualityStationEoICode %notin% ex_st)
    
    w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(get(pollutant), na.rm = T)) %>% 
      pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
    w_mean[apply(w_mean, 2, is.nan)] <- 0
    
    d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(get(pollutant), na.rm = T)) %>% 
      pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
    
    h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(get(pollutant), na.rm = T)) %>% 
      pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
    
    na_data <- data %>% filter(is.na(get(pollutant))) 
    
    
    w_eff_gen <- function(w, s) {
      w_eff <- w_mean[which(w_mean$week == w),s, drop = T] - rowMeans(w_mean[which(w_mean$week == w),-1])
      return(w_eff)
    }
    d_eff_gen <- function(d,s) {
      d_eff <- d_mean[d,s, drop = T] - rowMeans(d_mean[d,-1])
      return(d_eff)
    }
    h_eff_gen <- function(h, s) {
      h_eff <- h_mean[h,s, drop = T] - rowMeans(h_mean[h,-1], na.rm = T)
    }
    x_mean_gen <- function(w, d, h, s) {
      x_mean <- data %>% 
        filter(week == w & day == d & hour == h & AirQualityStationEoICode != s) %>% 
        pull(get(pollutant)) %>% 
        mean(na.rm = T)
      return(x_mean)
    }
    
    
    
    imps <- na_data %>% 
      rowwise() %>% 
      mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
             d_eff = d_eff_gen(day, AirQualityStationEoICode), 
             h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
             x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
      mutate(imps = x_mean + 0.5*w_eff + 0.5*d_eff + 0.5*h_eff) %>% 
      pull(imps)
    
    
    
    
    imps <- ifelse(imps <= 0, 0.01, imps)
    
    #data[is.na(data[,pollutant]), pollutant] <- imps
    
    
    return(imps)
    
    
  }
  
  LOCF <- function(pollutant, data) {
    ex_st <- unlist(lapply(unique(data$AirQualityStationEoICode), sel_st, data, pollutant))
    data <- data %>% filter(AirQualityStationEoICode %notin% ex_st)
    na_data <- data %>% filter(is.na(get(pollutant))) 
  }
  
  
  
  imps <- lapply(polls, SDEM, data = h_data)
  
  
  for (i in length(polls)) {
    
    ex_st <- unlist(lapply(unique(data$AirQualityStationEoICode), sel_st, data, polls[i]))
    data[data$AirQualityStationEoICode %notin% ex_st & is.na(data[,polls[i]]), polls[i]] <- imps[[i]]
    
  }
  
  return(data)
  
}

sum(is.na(data$`Black Carbon`))

library(tidyverse)

may_data <- data %>% dplyr::filter(DatetimeBegin >= lubridate::ymd_hms("2019-05-01 00:00:00")) %>% filter(DatetimeBegin <= lubridate::ymd_hms("2019-05-31 23:00:00"))

may_data <- data %>% dplyr::filter(DatetimeBegin >= as.Date("2019-01-05 00:00:00") & DatetimeBegin <= as.Date("2019-31-05 23:00:00"))

library(tictoc)
tic()
imp_data <- EEAaq_NA_impute(data = may_data, method = "SDEM")
toc()



library(imputeTS)
?imputeTS
es <- may_data %>% filter(AirQualityStationEoICode == "BELSZ02")

unique(may_data$AirQualityStationEoICode)


na_locf(es$PM2.5)
?EEAaq_idw_map
