#IMPUTAZIONE PM2.5

#SDEM PESI STAZIONE PER STAZIONE (lm)

#Dati full
#50 simulazioni
#5% missing rate

library(mgcv)
library(tidyverse)
library(dplyr)
library(tictoc)
library(imputeTS)
library(lubridate)



load("C:/Users/Agostino/Desktop/TESI/applicazione/CASO STUDIO/bc_data.RData")
load("C:/Users/Agostino/Desktop/Algoritmo Imputazione/simulazioni/dati_simulazione_algoritmo.RData")
load("C:/Users/Agostino/Desktop/NA_impute/data_full_prova.RData")


#Aggiungo pm10 e pm2.5 al bc
data <- left_join(bc_data, select(data_full, AirQualityStationEoICode, PM10, PM2.5, DatetimeBegin), by = c("AirQualityStationEoICode", "DatetimeBegin"))

#Elimino le centraline che non hanno dati per il pm
"%notin%" <- Negate("%in%")
data <- data %>% filter(AirQualityStationEoICode %notin% c("BELOB01", "BETN043", "BETN047", "BETR001", "BETR002", "BETR012", "BETWOL1",
                                                           "NL00003", "NL00020", "NL00237", "NL00492", "NL00493", "NL00633"))



tab3 <- tibble(int = c("long", "medium", "one", "short", "three", "two"), 
               prob = c(0.00314, 0.0148, 0.818, 0.0373, 0.0368, 0.0898))

gen_na_short <- function(){
  sam <- sample(tab3$int, size = 544, replace = T, prob = tab3$prob)
  prova <- vector()
  for (i in 1:length(sam)) {
    
    if(sam[i] == "one") {
      prova <- c(prova,1)
    } else if(sam[i] == "two") {
      prova <- c(prova, 2)
    } else if(sam[i] == "three") {
      prova <- c(prova, 3)
    } else if(sam[i] == "short") {
      prova <- c(prova, sample(4:24,1))
    } else if(sam[i] == "medium") {
      prova <- c(prova, sample(25:72,1))
    } else {
      prova <- c(prova, sample(73:120,1))
    }
    
  }
  out <- vector()
  prova <- sort(prova, decreasing = T)
  "%notin%" <- Negate("%in%")
  fun <- function(x) {((x*744)+1):((x*744)+744-gap+1)}
  for (i in 1:length(prova)) {
    gap <- prova[i]
    poss <- vector()
    poss <- sort(unlist(lapply(0:41, fun)))
    poss <- poss[poss %notin% out]
    res <- sample(poss,1)
    out <- c(out, res:(res+gap-1))
    
  }
  out <- unique(out)
  return(out)
}

gen_na_short_2 <- function(){
  sam <- sample(tab3$int, size = 435, replace = T, prob = tab3$prob)
  prova <- vector()
  for (i in 1:length(sam)) {
    
    if(sam[i] == "one") {
      prova <- c(prova,1)
    } else if(sam[i] == "two") {
      prova <- c(prova, 2)
    } else if(sam[i] == "three") {
      prova <- c(prova, 3)
    } else if(sam[i] == "short") {
      prova <- c(prova, sample(4:24,1))
    } else if(sam[i] == "medium") {
      prova <- c(prova, sample(25:72,1))
    } else {
      prova <- c(prova, sample(73:120,1))
    }
    
  }
  out <- vector()
  prova <- sort(prova, decreasing = T)
  "%notin%" <- Negate("%in%")
  fun <- function(x) {((x*744)+1):((x*744)+744-gap+1)}
  for (i in 1:length(prova)) {
    gap <- prova[i]
    poss <- vector()
    poss <- sort(unlist(lapply(0:30, fun)))
    poss <- poss[poss %notin% out]
    res <- sample(poss,1)
    out <- c(out, res:(res+gap-1))
    
  }
  out <- unique(out)
  return(out)
}



#summ <- EEA_AQ_summary(bc_data)
stations_info <- summ$Stations_info %>% 
  select(AirQualityStationEoICode, Longitude, Latitude, AirQualityStationType, AirQualityStationArea)

may_data <- data %>% filter(AirQualityStationEoICode  %notin% c("BELOB01", "BETR002")) %>%
  filter(DatetimeBegin >= ymd_hms("2019-05-01 00:00:00") & DatetimeBegin <= ymd_hms("2019-05-31 23:00:00")) %>%
  left_join(stations_info, by = "AirQualityStationEoICode")

may_data[which(may_data$PM2.5 < 0 & !is.na(may_data$PM2.5)),"PM2.5"] <- NA
#Trasformo i valori pari a 0 in valori vicini allo zero
may_data[which(may_data$PM2.5 == 0 & !is.na(may_data$PM2.5)),"PM2.5"] <- NA



nrow(may_data)#23064
#1153.2 missing 





SDEM_w_st <- function(data) {
  
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(PM2.5, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(PM2.5, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(PM2.5, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
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
  
  
  tr <- data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>%
    select(AirQualityStationEoICode,PM2.5, x_mean, w_eff, d_eff, h_eff) 
  
  
  
  names <- rep(c("x_mean", "w_eff", "d_eff", "h_eff"), 31)
  pesi <- tr %>% group_by(AirQualityStationEoICode) %>% 
    reframe(pesi = coefficients(lm(PM2.5 ~ 0+ .data$x_mean + .data$w_eff + .data$d_eff + .data$h_eff))) %>%
    bind_cols(names = names) %>% 
    pivot_wider(names_from = names, values_from = pesi)
  
  
  imp_pesi_st <- function(st, x_mean, w_eff, d_eff, h_eff) {
    vec_pesi <- filter(pesi, AirQualityStationEoICode == st)
    res <- as.numeric(vec_pesi[2]*x_mean + vec_pesi[3]*w_eff + vec_pesi[4]*d_eff + vec_pesi[5]*h_eff)
    return(res)
  }
  
  
  
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    rowwise() %>%
    mutate(imps = imp_pesi_st(AirQualityStationEoICode, x_mean, w_eff, d_eff, h_eff)) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$PM2.5), "PM2.5"] <- imps
  
  
  return(data)
  
  
}


#Creazione dataset completo
may_data <- SDEM_w_st(may_data)



#Simulazione * 100

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

SDEM_r_sq <- vector()
SDEM_RMSE <- vector()
SDEM_MAE <- vector()
SDEM_times <- vector()
SDEM_na_count <- vector()


set.seed(77)

for (i in 1:50) {
  
  tic()
  cat(paste(i))
  mynas <- gen_na_short_2()
  true <- may_data[mynas,]
  may_data_na <- may_data
  may_data_na[mynas, "PM2.5"] <- NA
  may_data_na <- SDEM_w_st(may_data_na)
  yhat <- may_data_na[mynas,"PM2.5"] %>% pull()
  y <- true[,"PM2.5"] %>% pull()
  
  
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  
  SDEM_r_sq[i] <- r_sq(y, yhat)
  SDEM_RMSE[i] <- rmse(y, yhat)
  SDEM_MAE[i] <- mae(y, yhat)
  SDEM_times[i] <- tot_time
  SDEM_na_count[i] <- length(mynas)
  
  
}



mean(SDEM_na_count)
mean(SDEM_times)
mean(SDEM_r_sq)
mean(SDEM_RMSE)
sd(SDEM_RMSE)
mean(SDEM_MAE)
sd(SDEM_MAE)



