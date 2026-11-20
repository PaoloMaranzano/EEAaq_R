

########################################
#Algoritmo
########################################
library(mgcv)
library(tidyverse)
library(dplyr)
library(tictoc)
library(lubridate)

load("C:/Users/Agostino/Desktop/Funzione Simulazioni/dati_sim.RData")


#SDEM
SDEM_bc <- function(data) {
  
  #Aggiungo ai dati le informazioni relative alla settimana dell'anno, al giorno della settimana e all'ora del giorno
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  #Creazione delle matrici contenenti le medie settimanli per stazione
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(`Black Carbon`, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  w_mean[apply(w_mean,2,is.nan)] <- 0
  
  #Medie giornaliere per stazione
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(`Black Carbon`, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  d_mean[apply(d_mean,2,is.nan)] <- 0
  
  #Medie orarie per stazione
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(`Black Carbon`, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  h_mean[apply(h_mean,2,is.nan)] <- 0
  
  #Filtro i dati mancanti
  na_data <- data %>% filter(is.na(`Black Carbon`)) 
  
  #Funzioni che per ogni centraline e per ogni timestamp, generano l'effetto settimanale, giornaliero e orario
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
      pull(`Black Carbon`) %>% 
      mean(na.rm = T)
    return(x_mean)
  }
  
  
  #Calcolo i valori che vanno imputati
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    mutate(imps = x_mean + 0.5*w_eff + 0.5*d_eff + 0.5*h_eff) %>% pull(imps)
  
  
  #Correzione per valori ottenuti pari a 0
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  #Aggiungo i dati imputati 
  data[is.na(data$`Black Carbon`), "Black Carbon"] <- imps
  
  
  return(data)
  
  
}

SDEM_pm10 <- function(data) {
  
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  w_mean[apply(w_mean,2,is.nan)] <- 0
  
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  d_mean[apply(d_mean,2,is.nan)] <- 0
  
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  h_mean[apply(h_mean,2,is.nan)] <- 0
  
  na_data <- data %>% filter(is.na(PM10)) 
  
  
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
      pull(PM10) %>% 
      mean(na.rm = T)
    return(x_mean)
  }
  
  
  
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    mutate(imps = x_mean + 0.5*w_eff + 0.5*d_eff + 0.5*h_eff) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$PM10), "PM10"] <- imps
  
  
  return(data)
  
  
}

SDEM_pm2.5 <- function(data) {
  
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(PM2.5, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  w_mean[apply(w_mean,2,is.nan)] <- 0
  
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(PM2.5, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  d_mean[apply(d_mean,2,is.nan)] <- 0
  
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(PM2.5, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  h_mean[apply(h_mean,2,is.nan)] <- 0
  
  na_data <- data %>% filter(is.na(PM2.5)) 
  
  
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
      pull(PM2.5) %>% 
      mean(na.rm = T)
    return(x_mean)
  }
  
  
  
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    mutate(imps = x_mean + 0.5*w_eff + 0.5*d_eff + 0.5*h_eff) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$PM2.5), "PM2.5"] <- imps
  
  
  return(data)
  
  
}

#SDEM_w_gl
SDEM_w_gl <- function(data) {
  
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  w_mean[apply(w_mean,2,is.nan)] <- 0
  
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  d_mean[apply(d_mean,2,is.nan)] <- 0
  
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  h_mean[apply(h_mean,2,is.nan)] <- 0
  
  na_data <- data %>% filter(is.na(PM10)) 
  
  
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
      pull(PM10) %>% 
      mean(na.rm = T)
    return(x_mean)
  }
  
  
  tr <- data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>%
    select(PM10, x_mean, w_eff, d_eff, h_eff) 
  
  #Calcolo dei pesi che verranno usati in fase di imputazione
  pesi <- lm(PM10 ~ 0+., data = tr)$coeff
  
  
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    mutate(imps = pesi[1]*x_mean + pesi[2]*w_eff + pesi[3]*d_eff + pesi[4]*h_eff) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$PM10), "PM10"] <- imps
  
  
  return(data)
  
  
}

#SDEM_w_st
SDEM_w_st <- function(data) {
  
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(PM10, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  na_data <- data %>% filter(is.na(PM10)) 
  
  
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
      pull(PM10) %>% 
      mean(na.rm = T)
    return(x_mean)
  }
  
  
  tr <- data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>%
    select(AirQualityStationEoICode,PM10, x_mean, w_eff, d_eff, h_eff) 
  
  
  #Calcolo dei pesi che verranno usati in fase di imputazione, calcolari centralina per centralina
  names <- rep(c("x_mean", "w_eff", "d_eff", "h_eff"), 31)
  pesi <- tr %>% group_by(AirQualityStationEoICode) %>% 
    reframe(pesi = coefficients(lm(PM10 ~ 0+ .data$x_mean + .data$w_eff + .data$d_eff + .data$h_eff))) %>%
    bind_cols(names = names) %>% 
    pivot_wider(names_from = names, values_from = pesi)
  
  
  #Funzione per calcolare i dati imputati
  imp_pesi_st <- function(st, x_mean, w_eff, d_eff, h_eff) {
    vec_pesi <- filter(pesi, AirQualityStationEoICode == st)
    res <- as.numeric(vec_pesi[2]*x_mean + vec_pesi[3]*w_eff + vec_pesi[4]*d_eff + vec_pesi[5]*h_eff)
    return(res)
  }
  
  
  #Imputazione
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    rowwise() %>%
    mutate(imps = imp_pesi_st(AirQualityStationEoICode, x_mean, w_eff, d_eff, h_eff)) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$PM10), "PM10"] <- imps
  
  
  return(data)
  
  
}

#Timestamps del mese 
h_times <- unique(may_data$DatetimeBegin)
#Giorni del mese
d_times <- unique(lubridate::as_date(may_data$DatetimeBegin))




#Imputazione iniziale <- completo il dataset per poi poter fare le simulazioni 

may_data <- SDEM_bc(may_data)

may_data <- SDEM_pm10(may_data)

may_data <- SDEM_pm2.5(may_data)



r_sq <- function(true, yhat) {
  rss <- sum((true - yhat)^2)
  tss <- sum((true - mean(true))^2)
  return(1 - (rss/tss))
}
rmse <- function(true, yhat) {
  return(sqrt(mean((true-yhat)^2)) )
}
mae <- function(true, yhat) {
  return(mean(abs(true-yhat)))
}


#FUNZIONE

valutazione_sim <- function(x) {
  
  
  make_ts <- function(data) {
    newdata <- data %>%
      group_by(AirQualityStationEoICode) %>%
      rename(BC_t = `Black Carbon`, PM10_t = PM10, PM2.5_t = PM2.5) %>%
      #BC ritardi
      mutate(BC_t_1 = dplyr::lag(BC_t, n = 1, default = NA)) %>% 
      mutate(BC_t_2 = dplyr::lag(BC_t, n = 2, default = NA)) %>% 
      mutate(BC_t_3 = dplyr::lag(BC_t, n = 3, default = NA)) %>%
      mutate(BC_t_4 = dplyr::lag(BC_t, n = 4, default = NA)) %>% 
      mutate(BC_t_5 = dplyr::lag(BC_t, n = 5, default = NA)) %>%
      mutate(BC_t1 = dplyr::lead(BC_t, n = 1, default = NA)) %>%
      mutate(BC_t2 = dplyr::lead(BC_t, n = 2, default = NA)) %>%
      mutate(BC_t3 = dplyr::lead(BC_t, n = 3, default = NA)) %>%
      #mutate(BC_t_24 = dplyr::lag(BC_t, n = 24, default = NA)) %>%
      #mutate(BC_t_168 = dplyr::lag(BC_t, n = 168, default = NA)) %>%
      mutate(PM10_t_1 = dplyr::lag(PM10_t, n = 1, default = NA)) %>%
      mutate(PM10_t_2 = dplyr::lag(PM10_t, n = 2, default = NA)) %>%
      mutate(PM10_t_3 = dplyr::lag(PM10_t, n = 3, default = NA)) %>%
      mutate(PM10_t_4 = dplyr::lag(PM10_t, n = 4, default = NA)) %>%
      mutate(PM10_t_5 = dplyr::lag(PM10_t, n = 5, default = NA)) %>%
      mutate(PM10_t1 = dplyr::lead(PM10_t, n = 1, default = NA)) %>%
      mutate(PM10_t2 = dplyr::lead(PM10_t, n = 2, default = NA)) %>%
      mutate(PM10_t3 = dplyr::lead(PM10_t, n = 3, default = NA)) %>%
      mutate(PM2.5_t_1 = dplyr::lag(PM2.5_t, n = 1, default = NA)) %>%
      mutate(PM2.5_t_2 = dplyr::lag(PM2.5_t, n = 2, default = NA)) %>%
      mutate(PM2.5_t_3 = dplyr::lag(PM2.5_t, n = 3, default = NA)) %>%
      mutate(PM2.5_t_4 = dplyr::lag(PM2.5_t, n = 4, default = NA)) %>%
      mutate(PM2.5_t_5 = dplyr::lag(PM2.5_t, n = 5, default = NA)) %>%
      mutate(PM2.5_t1 = dplyr::lead(PM2.5_t, n = 1, default = NA)) %>%
      mutate(PM2.5_t2 = dplyr::lead(PM2.5_t, n = 2, default = NA)) %>%
      mutate(PM2.5_t3 = dplyr::lead(PM2.5_t, n = 3, default = NA)) %>%
      ungroup() %>%
      relocate(DatetimeBegin, .after = AirQualityStationEoICode) %>%
      mutate(AirQualityStationArea = as.factor(AirQualityStationArea),
             AirQualityStationType = as.factor(AirQualityStationType))
    
    return(newdata)
    
  }
  
  
  
  true <- may_data[x,]
  
  may_data_na <- may_data
  #Aggiungo i dati mancanti 
  may_data_na[x, "PM10"] <- NA
  
  
  #Imputazione iniziale delle prime 6 ore tramite SDEM, per avere i dati completi 
  #relativi ai 5 ritardi, per dare inizio all'imputazione
  imps <- SDEM_pm10(may_data_na)
  imps <- imps %>% filter(DatetimeBegin < (h_times[1] + hours(6))) %>% pull(PM10)
  may_data_na[may_data_na$DatetimeBegin < (h_times[1] + hours(6)) ,"PM10"] <- imps
  
  #SDEM
  may_data_na_SDEM <- may_data_na
  #SDEM_w_gl
  may_data_na_SDEM_w_gl <- may_data_na
  #SDEM_w_st
  may_data_na_SDEM_w_st <- may_data_na
  #Mod_m_st_fut
  may_data_na_mod_st_fut <- may_data_na
  #Mod_m_gwr_fut
  may_data_na_mod_gwr_fut <- may_data_na
  
  
  
  
  ##########################################################################################
  #IMPUTAZIONE 
  #########################################################################################
  
  
  
  
  ########################
  #SDEM
  #cat(paste0("SDEM - simulazione ", j, "\n"))
  tic()
  may_data_na_SDEM <- SDEM_pm10(may_data_na_SDEM)
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  yhat <- may_data_na_SDEM[x,"PM10"] %>% pull()
  y <- true[,"PM10"] %>% pull()
  
  
  SDEM_times <- tot_time
  SDEM_r_sq <- r_sq(y, yhat)
  SDEM_RMSE <- rmse(y, yhat)
  SDEM_MAE <- mae(y, yhat)
  
  res <- data.frame(SDEM_times = SDEM_times, SDEM_r_sq = SDEM_r_sq, SDEM_RMSE = SDEM_RMSE, SDEM_MAE = SDEM_MAE)
  
  
  
  
  ########################
  #SDEM_w_gl
  #cat(paste0("SDEM_w_gl - simulazione ", j, "\n"))
  tic()
  may_data_na_SDEM_w_gl <- SDEM_w_gl(may_data_na_SDEM_w_gl)
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  yhat <- may_data_na_SDEM_w_gl[x,"PM10"] %>% pull()
  y <- true[,"PM10"] %>% pull()
  
  res$SDEM_w_gl_times <- tot_time
  res$SDEM_w_gl_r_sq <- r_sq(y, yhat)
  res$SDEM_w_gl_RMSE <- rmse(y, yhat)
  res$SDEM_w_gl_MAE <- mae(y, yhat)
  
  
  
  
  
  ############################
  #SDEM_w_st
  #cat(paste0("SDEM_w_st - simulazione ", j, "\n"))
  tic()
  may_data_na_SDEM_w_st <- SDEM_w_st(may_data_na_SDEM_w_st)
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  yhat <- may_data_na_SDEM_w_st[x,"PM10"] %>% pull()
  y <- true[,"PM10"] %>% pull()
  
  res$SDEM_w_st_times <- tot_time
  res$SDEM_w_st_r_sq <- r_sq(y, yhat)
  res$SDEM_w_st_RMSE <- rmse(y, yhat)
  res$SDEM_w_st_MAE <- mae(y, yhat)
  
  
  
  
  ###########################
  #Mod_m_st_fut
  #cat(paste0("Mod_m_st_fut - simulazione ", j, "\n"))
  tic()
  
  inizio <- which(d_times == d_times[1])
  
  ts_data <- may_data_na_mod_st_fut  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
  train <- ts_data 
  
  mods <- list()
  mods_fut <- list()
  st <- unique(train$AirQualityStationEoICode)
  
  for (s in 1:length(st)) {
    
    train_st <- filter(train, AirQualityStationEoICode == st[s])
    
    
    fit_st <- mgcv::gam(PM10_t ~ PM10_t_1 +
                          PM10_t_2 +
                          PM10_t_3 +
                          PM10_t_4 +
                          PM10_t_5 +
                          PM2.5_t +
                          PM2.5_t_1 +
                          PM2.5_t_2 +
                          PM2.5_t_3 +
                          PM2.5_t_4 +
                          PM2.5_t_5,
                        data = train_st, family = Gamma(link = "identity"))
    
    
    mods[[st[s]]] <- fit_st
    
    train_st_fut <- train_st %>% filter(!is.na(PM10_t1) & !is.na(PM10_t2) & !is.na(PM10_t3) & !is.na(PM2.5_t1) & !is.na(PM2.5_t2) & !is.na(PM2.5_t2))
    
    
    fit_st_fut <- mgcv::gam(PM10_t ~ PM10_t_1 +
                              PM10_t_2 +
                              PM10_t_3 +
                              PM10_t_4 +
                              PM10_t_5 +
                              PM10_t1 +
                              PM10_t2 +
                              PM10_t3 +
                              PM2.5_t +
                              PM2.5_t_1 +
                              PM2.5_t_2 +
                              PM2.5_t_3 +
                              PM2.5_t_4 +
                              PM2.5_t_5 +
                              PM2.5_t1 +
                              PM2.5_t2 +
                              PM2.5_t3,
                            data = train_st, family = Gamma(link = "identity"))
    
    mods_fut[[st[s]]] <- fit_st_fut
    
  }
  
  
  for (i in (inizio):length(d_times)) {
    
    na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(PM10_t))
    
    
    if(nrow(na_data) > 0) {
      
      h_day <- na_data %>% 
        arrange(DatetimeBegin, AirQualityStationEoICode) %>% 
        distinct(DatetimeBegin) %>% 
        pull(DatetimeBegin)
      
      
      #Previsione
      for (k in 1:length(h_day)) {
        na_h <- na_data %>%
          filter(DatetimeBegin == h_day[k])
        prev <- vector()
        
        for (h in 1:nrow(na_h)) {
          
          if(!is.na(na_h[h,]$PM2.5_t1) & !is.na(na_h[h,]$PM2.5_t2) & !is.na(na_h[h,]$PM2.5_t3) & !is.na(na_h[h,]$PM10_t1) & !is.na(na_h[h,]$PM10_t2) & !is.na(na_h[h,]$PM10_t3)) {
            
            mod <- mods_fut[[na_h[h,]$AirQualityStationEoICode]]
            prev[h] <- predict(mod, na_h[h,])
            
          } else {
            
            mod <- mods[[na_h[h,]$AirQualityStationEoICode]]
            prev[h] <- predict(mod, na_h[h,])
            
          }
          
          
          
        }
        
        prev <- ifelse(prev <= 0, 0.01, prev)
        
        may_data_na_mod_st_fut[which(is.na(may_data_na_mod_st_fut$PM10) & may_data_na_mod_st_fut$DatetimeBegin == h_day[k]), "PM10"] <- prev
        ts_data <- may_data_na_mod_st_fut  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
        na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(PM10_t))
        
        
      }
    }
    
    
  }
  
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  
  yhat <- may_data_na_mod_st_fut[x,"PM10"] %>% pull()
  y <- true[,"PM10"] %>% pull()
  
  res$mod_m_st_fut_times <- tot_time
  res$mod_m_st_fut_r_sq <- r_sq(y,yhat)
  res$mod_m_st_fut_RMSE <- rmse(y,yhat)
  res$mod_m_st_fut_MAE <- mae(y, yhat)
  
  
  
  
  
  
  
  ############################à
  #mod_gwr_fut
  #cat(paste0("mod_gwr_fut - simulazione ", j, "\n"))
  
  tic()
  
  ts_data <- may_data_na_mod_gwr_fut  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
  train <- ts_data %>% filter(DatetimeBegin >= d_times[inizio] + hours(6)) %>% 
    filter(!is.na(PM10_t) & !is.na(PM10_t_1) & !is.na(PM10_t_2) & !is.na(PM10_t_3) &!is.na(PM10_t_4) & !is.na(PM10_t_5) & !is.na(PM2.5_t) & !is.na(PM2.5_t_1) & !is.na(PM2.5_t_2) & !is.na(PM2.5_t_3) & !is.na(PM2.5_t_4) & !is.na(PM2.5_t_5))
  
  train_fut <- train %>% filter(!is.na(PM10_t1) & !is.na(PM10_t2) & !is.na(PM10_t3) & !is.na(PM2.5_t1) & !is.na(PM2.5_t2) & !is.na(PM2.5_t3))
  
  sp::coordinates(train) <- c("Longitude", "Latitude")
  sp::coordinates(train_fut) <- c("Longitude", "Latitude")
  
  
  
  mod <- spgwr::gwr(PM10_t ~ PM10_t_1 +
                      PM10_t_2 +
                      PM10_t_3 +
                      PM10_t_4 +
                      PM10_t_5 +
                      PM2.5_t +
                      PM2.5_t_1 +
                      PM2.5_t_2 +
                      PM2.5_t_3 +
                      PM2.5_t_4 +
                      PM2.5_t_5,
                    data = train, bandwidth = 0.2)
  
  mod_fut <- spgwr::gwr(PM10_t ~ PM10_t_1 +
                          PM10_t_2 +
                          PM10_t_3 +
                          PM10_t_4 +
                          PM10_t_5 +
                          PM10_t1 +
                          PM10_t2 +
                          PM10_t3 +
                          PM2.5_t +
                          PM2.5_t_1 +
                          PM2.5_t_2 +
                          PM2.5_t_3 +
                          PM2.5_t_4 +
                          PM2.5_t_5 +
                          PM2.5_t1 +
                          PM2.5_t2 +
                          PM2.5_t3,
                        data = train_fut, 
                        bandwidth = 0.2)
  
  
  
  for (i in (inizio):length(d_times)) {
    
    na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(PM10_t))
    
    
    if(nrow(na_data) > 0) {
      
      
      h_day <- na_data %>% 
        arrange(DatetimeBegin, AirQualityStationEoICode) %>% 
        distinct(DatetimeBegin) %>% 
        pull(DatetimeBegin)
      
      
      
      #Previsione
      for (k in 1:length(h_day)) {
        na_h <- na_data %>%
          filter(DatetimeBegin == h_day[k])
        sp::coordinates(na_h) <- c("Longitude", "Latitude")
        prev <- vector()
        
        for (h in 1:nrow(na_h)) {
          
          if(!is.na(na_h[h,]$PM2.5_t1) & !is.na(na_h[h,]$PM2.5_t2) & !is.na(na_h[h,]$PM2.5_t3) & !is.na(na_h[h,]$PM10_t1) & !is.na(na_h[h,]$PM10_t2) & !is.na(na_h[h,]$PM10_t3)) {
            
            pred <- spgwr::gwr(PM10_t ~ PM10_t_1 +
                                 PM10_t_2 +
                                 PM10_t_3 +
                                 PM10_t_4 +
                                 PM10_t_5 +
                                 PM10_t1 +
                                 PM10_t2 +
                                 PM10_t3 +
                                 PM2.5_t +
                                 PM2.5_t_1 +
                                 PM2.5_t_2 +
                                 PM2.5_t_3 +
                                 PM2.5_t_4 +
                                 PM2.5_t_5 +
                                 PM2.5_t1 +
                                 PM2.5_t2 +
                                 PM2.5_t3,
                               data = train_fut, 
                               bandwidth = 0.3, fit.points = na_h[h,], predictions = TRUE, fittedGWRobject = mod_fut)
            prev[h] <- pred$SDF$pred
            
          } else {
            
            pred <- spgwr::gwr(PM10_t ~ PM10_t_1 +
                                 PM10_t_2 +
                                 PM10_t_3 +
                                 PM10_t_4 +
                                 PM10_t_5 +
                                 PM2.5_t +
                                 PM2.5_t_1 +
                                 PM2.5_t_2 +
                                 PM2.5_t_3 +
                                 PM2.5_t_4 +
                                 PM2.5_t_5,
                               data = train, 
                               bandwidth = 0.3, fit.points = na_h[h,], predictions = TRUE, fittedGWRobject = mod)
            prev[h] <- pred$SDF$pred
            
          }
          
        }
        
        prev <- ifelse(prev <= 0, 0.01, prev)
        
        
        may_data_na_mod_gwr_fut[which(is.na(may_data_na_mod_gwr_fut$PM10) & may_data_na_mod_gwr_fut$DatetimeBegin == h_day[k]), "PM10"] <- prev
        ts_data <- may_data_na_mod_gwr_fut  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
        na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(PM10_t))
        
        
      }
      
    }
  }
  
  
  
  
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  
  yhat <- may_data_na_mod_gwr_fut[x,"PM10"] %>% pull()
  y <- true[,"PM10"] %>% pull()
  
  res$mod_m_gwr_fut_times <- tot_time
  res$mod_m_gwr_fut_r_sq <- r_sq(y,yhat)
  res$mod_m_gwr_fut_RMSE <- rmse(y,yhat)
  res$mod_m_gwr_fut_MAE <- mae(y, yhat)
  
  
  res$na_count <- length(x)
  
  
  
  #save.image(file = paste0("C:/Users/Agostino/Desktop/provaaaa/risultati_short_gaps", j,".RData"))
  
  return(res)
  
  
}



library(future)
library(future.apply)

set.seed(1)
sims <- list()


for (i in 1:500) {
  
  sims[[i]] <- gen_na_short()
  
}




future::plan(future::multisession, workers = 4)
res_pm10 <- future.apply::future_lapply(X = sims, FUN = valutazione_sim)
plan(sequential)


#filepath = 
save.image(file = filepath)


