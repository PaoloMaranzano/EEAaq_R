#ALG

#50 sim
#5% missing rate

#Tutto lineare




#######################################################
#imputazione Bc usando pm10 e pm2.5
#######################################################
#Finestra settimanale, orizzonte previsivo 1 giorno





########################################
#Algoritmo
########################################
library(mgcv)
library(tidyverse)
library(dplyr)
library(tictoc)
library(imputeTS)
library(lubridate)

#Prova con una finestra temporale di una giornata intera 



load("C:/Users/Agostino/Desktop/TESI/applicazione/CASO STUDIO/bc_data.RData")
load("C:/Users/Agostino/Desktop/Algoritmo Imputazione/simulazioni/dati_simulazione_algoritmo.RData")
load("C:/Users/Agostino/Desktop/NA_impute/data_full_prova.RData")


#Aggiungo pm10 e pm2.5 al bc
data <- left_join(bc_data, select(data_full, AirQualityStationEoICode, PM10, PM2.5, DatetimeBegin), by = c("AirQualityStationEoICode", "DatetimeBegin"))

#Elimino le centraline che non hanno dati per il pm
"%notin%" <- Negate("%in%")
data <- data %>% filter(AirQualityStationEoICode %notin% c("BELOB01", "BETN043", "BETN047", "BETR001", "BETR002", "BETR012", "BETWOL1",
                                                           "NL00003", "NL00020", "NL00237", "NL00492", "NL00493", "NL00633"))


#1-sum(tab3$prob[-1])
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


#summ <- EEA_AQ_summary(bc_data, verbose = F)
stations_info <- summ$Stations_info %>% 
  select(AirQualityStationEoICode, Longitude, Latitude, AirQualityStationType, AirQualityStationArea)

"%notin%" <- Negate("%in%")
may_data <- data %>% filter(AirQualityStationEoICode %notin% c("BELOB01", "BETR002")) %>%
  filter(DatetimeBegin >= ymd_hms("2019-05-01 00:00:00") & DatetimeBegin <= ymd_hms("2019-05-31 23:00:00")) %>%
  left_join(stations_info, by = "AirQualityStationEoICode")


may_data[which(may_data$`Black Carbon` < 0 & !is.na(may_data$`Black Carbon`)),"Black Carbon"] <- NA
may_data[which(may_data$`Black Carbon` == 0 & !is.na(may_data$`Black Carbon`)),"Black Carbon"] <- NA
may_data[which(may_data$PM10 < 0 & !is.na(may_data$PM10)),"PM10"] <- NA
may_data[which(may_data$PM10 == 0 & !is.na(may_data$PM10)),"PM10"] <- NA
may_data[which(may_data$PM2.5 < 0 & !is.na(may_data$PM2.5)),"PM2.5"] <- NA
may_data[which(may_data$PM2.5 == 0 & !is.na(may_data$PM2.5)),"PM2.5"] <- NA


make_ts <- function(data) {
  newdata <- data %>%
    group_by(AirQualityStationEoICode) %>%
    rename(BC_t = `Black Carbon`, PM10_t = PM10, PM2.5_t = PM2.5) %>%
    #BC ritardi
    mutate(BC_t_1 = dplyr::lag(BC_t, n = 1, default = NA)) %>% 
    mutate(BC_t_2 = dplyr::lag(BC_t, n = 2, default = NA)) %>% 
    mutate(BC_t_3 = dplyr::lag(BC_t, n = 3, default = NA)) %>%
    mutate(BC_t_24 = dplyr::lag(BC_t, n = 24, default = NA)) %>%
    mutate(BC_t_168 = dplyr::lag(BC_t, n = 168, default = NA)) %>%
    ungroup() %>%
    relocate(DatetimeBegin, .after = AirQualityStationEoICode) %>%
    mutate(AirQualityStationArea = as.factor(AirQualityStationArea),
           AirQualityStationType = as.factor(AirQualityStationType))
  
  return(newdata)
  
}






######################################################################
#ALGORITMO
#####################################################################
#Imputazione iniziale:

SDEM_bc <- function(data) {
  
  data <- data %>% mutate(week = week(DatetimeBegin), day = wday(DatetimeBegin), hour = hour(DatetimeBegin) + 1)
  
  w_mean <- data %>% group_by(AirQualityStationEoICode, week) %>% summarise(mean = mean(`Black Carbon`, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  d_mean <- data %>% group_by(AirQualityStationEoICode, day) %>% summarise(mean = mean(`Black Carbon`, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  h_mean <- data %>% group_by(AirQualityStationEoICode, hour) %>% summarise(mean = mean(`Black Carbon`, na.rm = T)) %>% 
    pivot_wider(names_from = AirQualityStationEoICode, values_from = mean)
  
  na_data <- data %>% filter(is.na(`Black Carbon`)) 
  
  
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
  
  
  
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    mutate(imps = x_mean + 0.5*w_eff + 0.5*d_eff + 0.5*h_eff) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$`Black Carbon`), "Black Carbon"] <- imps
  
  
  return(data)
  
  
}

SDEM_pm10 <- function(data) {
  
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


h_times <- unique(may_data$DatetimeBegin)
d_times <- unique(lubridate::as_date(may_data$DatetimeBegin))



#imps_bc <- SDEM_bc(may_data)
#imps_bc <- imps_bc %>% pull(`Black Carbon`)
#may_data[,"Black Carbon"] <- imps_bc
may_data <- SDEM_bc(may_data)

#imps_pm10 <- SDEM_pm10(may_data)
#imps_pm10 <- imps_pm10 %>% pull(PM10)
#may_data[,"PM10"] <- imps_pm10
may_data <- SDEM_pm10(may_data)

#imps_pm2.5 <- SDEM_pm2.5(may_data)
#imps_pm2.5 <- imps_pm2.5 %>% pull(PM2.5)
#may_data[,"PM2.5"] <- imps_pm2.5
may_data <- SDEM_pm2.5(may_data)










ts_data <- may_data  %>% make_ts()



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

alg_r_sq <- vector()
alg_RMSE <- vector()
alg_MAE <- vector()
alg_times <- vector()
alg_na_count <- vector()


set.seed(77)


for (j in 1:3) {
  
  
  tic()
  cat(paste0(j, "\n"))
  mynas <- gen_na_short_2()
  true <- may_data[mynas,]
  may_data_na <- may_data
  may_data_na[mynas, "Black Carbon"] <- NA
  
  
  #Imputazione iniziale:
  imps <- SDEM_bc(may_data_na)
  imps <- imps %>% filter(DatetimeBegin < (d_times[1] + days(14))) %>% pull(`Black Carbon`)
  may_data_na[may_data_na$DatetimeBegin < (d_times[1] + days(14)) ,"Black Carbon"] <- imps
  
  
  ##########################################################################################
  #Algoritmo
  #########################################################################################
  #inizio in ore
  #inizio <- which(h_times == (h_times[1] + hours(168)))
  #inizio in giorni
  inizio <- which(d_times == d_times[1] + days(14))
  #Indicizzazione del tempo come ora della settimana
  
  
  
  for (i in (inizio):length(d_times)) {
    
    #cat(paste0("Imputing data for ", h_times[i], "\n"))
    ts_data <- may_data_na  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
    train <- ts_data %>% filter(DatetimeBegin >= d_times[i] - hours(168) & DatetimeBegin <= d_times[i] - hours(1))
    na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(BC_t))
    
    
    
    # BC
    if(nrow(na_data) > 0) {
      
      h_day <- na_data %>% 
        arrange(DatetimeBegin, AirQualityStationEoICode) %>% 
        distinct(DatetimeBegin) %>% 
        pull(DatetimeBegin)
      
      
      #Modello Black Carbon
      tic()
      fit_BC <- mgcv::gam(BC_t ~ s(time) + 
                            BC_t_1 + 
                            BC_t_24 + 
                            BC_t_168 +
                            PM10_t + 
                            s(PM2.5_t, by = AirQualityStationEoICode), #+ 
                            #AirQualityStationType, 
                          data = train, family = Gamma(link = "identity"))
      toc()
      
      #s(Tourists_stays_pc,bs = "bs",m=c(3,2))
      
      #Previsione
      for (k in 1:length(h_day)) {
        na_h <- na_data %>%
          filter(DatetimeBegin == h_day[k])
        
        #prev_BC <- exp(predict(fit_BC, na_h))
        prev_BC <- predict(fit_BC, na_h)
        prev_BC <- ifelse(prev_BC <= 0, 0.01, prev_BC)
        #prev_BC <- ifelse(prev_BC > 27.89, 27.89, prev_BC)
        
        may_data_na[which(is.na(may_data_na$`Black Carbon`) & may_data_na$DatetimeBegin == h_day[k]), "Black Carbon"] <- prev_BC
        #if( (k+hours(1)) %in% h_day & as.logical(sum(na_data[na_data$DatetimeBegin == (k+hours(1)),]$AirQualityStationEoICode %in% na_data[na_data$DatetimeBegin == k,]$AirQualityStationEoICode))) {
        #  
        #  ts_data <- may_data_na  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
        #  na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(BC_t))
        #}
        ts_data <- may_data_na  %>% make_ts() %>% mutate(time = (wday(DatetimeBegin) - 1) * 24 + (hour(DatetimeBegin) + 1))
        na_data <- ts_data %>% filter(lubridate::as_date(DatetimeBegin) == d_times[i]) %>% filter(is.na(BC_t))
        
        
      }
    }
    
    
  }

    
    
    
    tot_time <- tictoc::toc(quiet = T)
    tot_time <- tot_time$toc - tot_time$tic
    
    
    yhat <- may_data_na[mynas,"Black Carbon"] %>% pull()
    y <- true[,"Black Carbon"] %>% pull()
    
    alg_r_sq[j] <- r_sq(y,yhat)
    alg_RMSE[j] <- rmse(y,yhat)
    alg_MAE[j] <- mae(y, yhat)
    alg_times[j] <- tot_time
    alg_na_count[j] <- length(mynas)
    
    #save.image(file = paste0("C:/Users/Agostino/Desktop/provaaaa/risultati_short_gaps", j,".RData"))
    
  
  
}



save.image(file = "C:/Users/Agostino/Desktop/risultati_short_gaps.RData")
mean(alg_r_sq)
mean(alg_times)
mean(alg_RMSE)
sd(alg_RMSE)
mean(alg_MAE)
sd(alg_MAE)

#cbind(may_data_na[mynas,"Black Carbon"], may_data[mynas, "Black Carbon"]) 


may_data_na[which(may_data_na$`Black Carbon` == Inf),]
train[c(650:658,which(train$BC_t == Inf)), ] %>% select(AirQualityStationEoICode, DatetimeBegin, BC_t, BC_t_1, BC_t_24, BC_t_168, PM10_t, PM2.5_t)
