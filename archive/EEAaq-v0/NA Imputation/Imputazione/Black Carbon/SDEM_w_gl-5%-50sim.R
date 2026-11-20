#SDEM PESI GLOBALI (lm)

#Dati full
#50 simulazioni
#5% missing rate





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

may_data[which(may_data$`Black Carbon` < 0 & !is.na(may_data$`Black Carbon`)),"Black Carbon"] <- NA
#Trasformo i valori pari a 0 in valori vicini allo zero
may_data[which(may_data$`Black Carbon` == 0 & !is.na(may_data$`Black Carbon`)),"Black Carbon"] <- NA



nrow(may_data)#23064
#1153.2 missing 





SDEM_w_gl <- function(data) {
  
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
  
  
  tr <- data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>%
    select(`Black Carbon`, x_mean, w_eff, d_eff, h_eff) %>% 
    rename("bc" = `Black Carbon`) 
  
  pesi <- lm(bc ~ 0+., data = tr)$coeff
  
  #weights <- function(x, data) {
  #  
  #  s <- x[,"AirQualityStationEoICode"] %>% pull()
  #  w <- x[,"week"]%>% pull()
  #  d <- x[,"day"]%>% pull()
  #  h <- x[,"hour"]%>% pull()
  #  x_mean <- data %>% filter(week == w & day == d & hour == h) %>% pull(`Black Carbon`) %>% mean(na.rm = T)
  #  w_eff <- w_mean[which(w_mean$week == w),s] - rowMeans(w_mean[which(w_mean$week == w),-1])
  #  d_eff <- d_mean[d,s] - rowMeans(d_mean[d,-1])
  #  h_eff <- h_mean[h,s] - rowMeans(h_mean[h,-1], na.rm = T)
  #  
  #  #return(x_mean + 0.5*w_eff + 0.5*d_eff + 0.5*h_eff)
  #  return(c(x_mean, w_eff, d_eff, h_eff))
  #  
  #}
  
  #tr <- data.frame(x_mean = NA, w_eff = NA, d_eff = NA, h_eff = NA, bc = NA)
  
  
  
  #for (i in 1:nrow(data)) {
  #  #cat(paste0("imputing data ", i, " out of ", nrow(na_data)))
  #  hat <- weights(data[i,], data)
  #  tr[i,1] <- hat[1]
  #  tr[i,2] <- hat[2]
  #  tr[i,3] <- hat[3]
  #  tr[i,4] <- hat[4]
  #  tr[i,5] <- data[i,"Black Carbon"]
  #  cat(paste0(round((i/nrow(data))*100, 1)))
  #  
  #}
  
  
  
  #pesi <- lm(bc ~ 0+., data = tr)$coeff
  #summary(lm(bc ~ 0+., data = tr))
  
  
  imps <- na_data %>% 
    rowwise() %>% 
    mutate(w_eff = w_eff_gen(week, AirQualityStationEoICode), 
           d_eff = d_eff_gen(day, AirQualityStationEoICode), 
           h_eff = h_eff_gen(hour, AirQualityStationEoICode), 
           x_mean = x_mean_gen(week, day, hour, AirQualityStationEoICode)) %>% 
    mutate(imps = pesi[1]*x_mean + pesi[2]*w_eff + pesi[3]*d_eff + pesi[4]*h_eff) %>% pull(imps)
  
  
  
  
  imps <- ifelse(imps <= 0, 0.01, imps)
  
  data[is.na(data$`Black Carbon`), "Black Carbon"] <- imps
  
  
  return(data)
  
  
}


#Creazione dataset completo
may_data <- SDEM_w_gl(may_data)



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
  may_data_na[mynas, "Black Carbon"] <- NA
  may_data_na <- SDEM_w_gl(may_data_na)
  yhat <- may_data_na[mynas,"Black Carbon"] %>% pull()
  y <- true[,"Black Carbon"] %>% pull()
  
  
  tot_time <- tictoc::toc(quiet = T)
  tot_time <- tot_time$toc - tot_time$tic
  
  
  SDEM_r_sq[i] <- r_sq(y, yhat)
  SDEM_RMSE[i] <- rmse(y, yhat)
  SDEM_MAE[i] <- mae(y, yhat)
  SDEM_times[i] <- tot_time
  SDEM_na_count[i] <- length(mynas)
  
  
}


mean(SDEM_r_sq)
mean(SDEM_RMSE)
sd(SDEM_RMSE)
mean(SDEM_MAE)
sd(SDEM_MAE)
mean(SDEM_na_count)
mean(SDEM_times)







set.seed(456)



for (j in 1:1000) {
  
  len <- vector()
  set.seed(j)
  
  for (i in 1:50) {
    df <- length(gen_na_short_2())
    len[i] <- df
  }
  
  if(round(mean(len)) == 1153) {
    break
  }
  
}


