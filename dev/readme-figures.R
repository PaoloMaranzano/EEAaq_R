## ---------------------------------------------------------------------------
## Renders the two figures of README.md into man/figures/:
##
##   readme-network.png  the EEA sampling points of NO2, PM10, PM2.5 and O3,
##                       from Support_files/stations.rds over the NUTS-0
##                       borders of Support_files/NUTS.rds;
##   readme-milan.png    hourly NO2 and daily PM10 at the monitoring stations
##                       of the province of Milano, 1-15 January 2024: the
##                       EEAaq_df object shipped in inst/extdata and used by
##                       the vignette, i.e. the output of EEAaq_get_data().
##
##     Rscript dev/readme-figures.R
##
## No network access is needed. The pollutant colours are the ones of logo
## proposal B (dev/make-logo.R) and are the same in both figures.
## ---------------------------------------------------------------------------

if (!file.exists("DESCRIPTION")) stop("Run this script from the repository root.")
suppressPackageStartupMessages({
  library(sf)
  library(ggplot2)
  library(dplyr)
})
sf::sf_use_s2(FALSE)
out_dir <- file.path("man", "figures")

pal <- c(NO2 = "#d1495b", PM10 = "#edae49", PM2.5 = "#00798c", O3 = "#30638e")
labels_poll <- c(NO2 = "NO[2]", PM10 = "PM[10]", PM2.5 = "PM[2.5]", O3 = "O[3]")

## ---- Figure 1: the network ------------------------------------------------------
bbox_ll <- sf::st_as_sfc(sf::st_bbox(c(xmin = -11, ymin = 34.5, xmax = 34, ymax = 71.5),
                                     crs = 4326))
nuts0 <- readRDS(file.path("Support_files", "NUTS.rds"))
nuts0 <- nuts0[nuts0$LEVL_CODE == 0, ]
nuts0 <- suppressWarnings(sf::st_intersection(sf::st_make_valid(nuts0), bbox_ll))
nuts0 <- sf::st_simplify(sf::st_transform(nuts0, 3035), dTolerance = 2000)
## The United Kingdom has left the NUTS classification, but its historical
## stations are still in the EEA metadata: its outline comes from spData::world
## (Natural Earth), which only this script uses.
uk <- sf::st_geometry(spData::world[spData::world$iso_a2 %in% "GB", ])
nuts0 <- c(sf::st_geometry(nuts0), sf::st_transform(uk, 3035))

stations <- readRDS(file.path("Support_files", "stations.rds")) %>%
  filter(AirPollutant %in% names(pal), !is.na(Longitude), !is.na(Latitude),
         Longitude > -11, Longitude < 34, Latitude > 34.5, Latitude < 71.5) %>%
  distinct(AirQualityStationEoICode, AirPollutant, .keep_all = TRUE)
n_by <- table(factor(stations$AirPollutant, levels = names(pal)))
strip <- sprintf("%s~~\"(%s stations)\"", labels_poll,
                 formatC(as.integer(n_by), big.mark = ",", format = "d"))
names(strip) <- names(pal)
stations <- stations %>%
  mutate(panel = factor(strip[AirPollutant], levels = strip)) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  sf::st_transform(3035)
bb <- sf::st_bbox(nuts0)

p1 <- ggplot() +
  geom_sf(data = nuts0, fill = "grey93", colour = "grey60", linewidth = 0.15) +
  geom_sf(data = stations, aes(colour = AirPollutant), size = 0.25, alpha = 0.6,
          show.legend = FALSE) +
  scale_colour_manual(values = pal) +
  facet_wrap(~panel, ncol = 2, labeller = label_parsed) +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE,
           crs = 3035, default_crs = 3035) +
  scale_x_continuous(breaks = c(0, 20)) +
  scale_y_continuous(breaks = c(40, 50, 60, 70)) +
  labs(x = "Longitude (degrees)", y = "Latitude (degrees)") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "grey95"))

ggsave(file.path(out_dir, "readme-network.png"), p1, width = 8, height = 8.6,
       dpi = 150, bg = "white")

## ---- Figure 2: an EEAaq_df object -----------------------------------------------
invisible(Sys.setlocale("LC_TIME", "C"))            # English month names
milan <- readRDS(file.path("inst", "extdata", "vignette_data.rds"))
## missing hours/days are made explicit, so that the lines break at the gaps
## instead of joining the two sides of a gap with a straight segment
no2 <- milan %>% filter(AveragingTime == "hour", !is.na(NO2)) %>%
  transmute(station = AirQualityStationName, time = DatetimeBegin, value = NO2) %>%
  tidyr::complete(station, time)
pm10 <- milan %>% filter(AveragingTime == "day", !is.na(PM10)) %>%
  transmute(station = AirQualityStationName, time = DatetimeBegin, value = PM10) %>%
  tidyr::complete(station, time)

base_theme <- theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "none")
x_scale <- scale_x_datetime(date_breaks = "2 days", date_labels = "%d %b",
                            limits = range(milan$DatetimeBegin), expand = c(0.01, 0))
no2_median <- no2 %>% group_by(time) %>% summarise(value = median(value, na.rm = TRUE), .groups = "drop")
pm10_median <- pm10 %>% group_by(time) %>% summarise(value = median(value, na.rm = TRUE), .groups = "drop")

pa <- ggplot(no2, aes(time, value)) +
  geom_line(aes(group = station), colour = "grey70", linewidth = 0.25) +
  geom_line(data = no2_median, colour = pal[["NO2"]], linewidth = 0.8) +
  x_scale + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Date (2024)", y = expression(NO[2]~"hourly concentration ("*mu*g/m^3*")")) +
  base_theme
pb <- ggplot(pm10, aes(time, value)) +
  geom_line(aes(group = station), colour = "grey70", linewidth = 0.3) +
  geom_point(aes(group = station), colour = "grey60", size = 0.6, shape = 16) +
  geom_line(data = pm10_median, colour = pal[["PM10"]], linewidth = 0.8) +
  geom_point(data = pm10_median, colour = pal[["PM10"]], size = 1.6, shape = 17) +
  geom_hline(yintercept = 50, linetype = 2, linewidth = 0.4, colour = "grey30") +
  x_scale + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Date (2024)", y = expression(PM[10]~"daily concentration ("*mu*g/m^3*")")) +
  base_theme

p2 <- ggpubr::ggarrange(pa, pb, ncol = 2, labels = c("A", "B"),
                        font.label = list(size = 14, face = "bold"),
                        hjust = -0.3, vjust = 1.2)
ggsave(file.path(out_dir, "readme-milan.png"), p2, width = 9, height = 3.6,
       dpi = 150, bg = "white")
message("README figures written to ", out_dir)
