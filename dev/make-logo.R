## ---------------------------------------------------------------------------
## Renders the hex-sticker proposals (A-C) for EEAaq into man/figures/ (PNG,
## 480 x 552 px at 144 dpi), and copies the selected proposal to
## man/figures/logo.png, the file the README shows.
##
##     Rscript dev/make-logo.R          # all proposals, A as logo.png
##     Rscript dev/make-logo.R B        # choose proposal B as logo.png
##
## The map is drawn from the package's own metadata: the country borders are
## the NUTS-0 polygons of Support_files/NUTS.rds and the dots are the
## monitoring stations of Support_files/stations.rds, so the sticker shows the
## real EEA network. Every composition lives in a 100 x 115 user space whose
## hexagon has its vertices at (50,0) (100,28.75) (100,86.25) (50,115)
## (0,86.25) (0,28.75); the map is clipped to an inner hexagon.
## ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
chosen <- if (length(args)) toupper(args[1]) else "A"
if (!file.exists("DESCRIPTION")) stop("Run this script from the repository root.")
suppressPackageStartupMessages(library(sf))
out_dir <- file.path("man", "figures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

hex_x <- c(50, 100, 100, 50, 0, 0)
hex_y <- c(0, 28.75, 86.25, 115, 86.25, 28.75)

## Georgia where it exists, a generic serif otherwise.
serif <- "serif"
if (.Platform$OS.type == "windows") {
  ok <- tryCatch({
    grDevices::windowsFonts(eealogo = grDevices::windowsFont("Georgia"))
    TRUE
  }, error = function(e) FALSE)
  if (ok) serif <- "eealogo"
}

## ---- Map data in logo coordinates ------------------------------------------
## Mainland Europe in the ETRS89 Lambert azimuthal equal-area projection
## (EPSG:3035), the projection of the EEA maps, rescaled into a box of the
## logo space and clipped to a hexagon inset from the sticker border.
sf::sf_use_s2(FALSE)
bbox_ll <- sf::st_as_sfc(sf::st_bbox(c(xmin = -11, ymin = 34.5, xmax = 34, ymax = 71.5),
                                     crs = 4326))
nuts0 <- readRDS(file.path("Support_files", "NUTS.rds"))
nuts0 <- nuts0[nuts0$LEVL_CODE == 0, ]
nuts0 <- suppressWarnings(sf::st_intersection(sf::st_make_valid(nuts0), bbox_ll))
## a 2.5 km tolerance is invisible at sticker size and keeps the drawing light
nuts0 <- sf::st_simplify(sf::st_transform(nuts0, 3035), dTolerance = 2500)
## The United Kingdom has left the NUTS classification, but its historical
## stations are still in the EEA metadata: its outline comes from spData::world
## (Natural Earth), which only this script uses.
uk <- sf::st_geometry(spData::world[spData::world$iso_a2 %in% "GB", ])
nuts0 <- c(sf::st_geometry(nuts0), sf::st_transform(uk, 3035))

stations <- readRDS(file.path("Support_files", "stations.rds"))
stations <- stations[!is.na(stations$Longitude) & !is.na(stations$Latitude), ]
stations <- stations[stations$Longitude > -11 & stations$Longitude < 34 &
                       stations$Latitude > 34.5 & stations$Latitude < 71.5, ]
## one dot per sampling point, coloured later by the pollutant it measures
pts <- sf::st_as_sf(stations[, c("Longitude", "Latitude", "AirPollutant",
                                 "AirQualityStationEoICode")],
                    coords = c("Longitude", "Latitude"), crs = 4326)
pts <- sf::st_transform(pts, 3035)

## affine map from EPSG:3035 metres to the logo box [x0, x1] x [y0, y1]
## (y grows downwards in the logo space)
to_logo <- function(geom, box) {
  bb <- sf::st_bbox(nuts0)
  s <- min((box[2] - box[1]) / (bb$xmax - bb$xmin),
           (box[4] - box[3]) / (bb$ymax - bb$ymin))
  cx <- (box[1] + box[2]) / 2; cy <- (box[3] + box[4]) / 2
  mx <- (bb$xmin + bb$xmax) / 2; my <- (bb$ymin + bb$ymax) / 2
  g <- sf::st_geometry(geom)
  g <- (g - c(mx, my)) * matrix(c(s, 0, 0, -s), 2, 2) + c(cx, cy)
  sf::st_set_crs(g, NA)
}

inner_hex <- function(inset) {
  k <- (50 - inset) / 50
  sf::st_polygon(list(cbind(50 + (c(hex_x, hex_x[1]) - 50) * k,
                            57.5 + (c(hex_y, hex_y[1]) - 57.5) * k)))
}

draw_sf <- function(g, col, border, lwd = 0.4) {
  g <- sf::st_cast(sf::st_collection_extract(g, "POLYGON"), "POLYGON")
  for (i in seq_along(g)) {
    rings <- Filter(function(r) nrow(r) >= 4, unclass(g[[i]]))
    if (!length(rings)) next
    xs <- unlist(lapply(rings, function(r) c(NA, r[, 1])))[-1]
    ys <- unlist(lapply(rings, function(r) c(NA, r[, 2])))[-1]
    graphics::polypath(xs, ys, col = col, border = border, lwd = lwd, rule = "evenodd")
  }
}

map_layer <- function(box, inset) {
  clip <- sf::st_sfc(inner_hex(inset))
  land <- suppressWarnings(sf::st_intersection(sf::st_make_valid(to_logo(nuts0, box)), clip))
  xy <- to_logo(pts, box)
  inside <- lengths(sf::st_intersects(xy, clip)) > 0
  xy <- sf::st_coordinates(xy)[inside, , drop = FALSE]
  pollutant <- pts$AirPollutant[inside]
  ## sampling points closer than 0.3 logo units overlap on the sticker: keep
  ## one per cell and pollutant
  cell <- paste(round(xy[, 1] / 0.3), round(xy[, 2] / 0.3), pollutant)
  keep <- !duplicated(cell)
  list(land = land, xy = xy[keep, , drop = FALSE], pollutant = pollutant[keep])
}

## ---- Devices and text -------------------------------------------------------
open_device <- function(path, width = 480L) {
  height <- as.integer(round(width * 115 / 100))
  if (grepl("\\.svg$", path)) {
    grDevices::svg(path, width = width / 96, height = height / 96, bg = "transparent")
  } else {
    grDevices::png(path, width = width, height = height, res = 144L,
                   bg = "transparent",
                   type = if (.Platform$OS.type == "windows") "windows" else "cairo",
                   antialias = "cleartype")
  }
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", family = serif)
  graphics::plot.new()
  graphics::plot.window(xlim = c(-3, 103), ylim = c(118.5, -3.5))
}

## "EEA" and "aq" in two colours, centred as one word
wordmark <- function(y, size, col1, col2, target = 60) {
  cex <- size / 12
  w1 <- graphics::strwidth("EEA", cex = cex, font = 2)
  w2 <- graphics::strwidth("aq", cex = cex, font = 2)
  if (w1 + w2 > target) {
    cex <- cex * target / (w1 + w2)
    w1 <- graphics::strwidth("EEA", cex = cex, font = 2)
    w2 <- graphics::strwidth("aq", cex = cex, font = 2)
  }
  x0 <- 50 - (w1 + w2) / 2
  graphics::text(x0, y, "EEA", adj = c(0, 0), cex = cex, col = col1, font = 2)
  graphics::text(x0 + w1, y, "aq", adj = c(0, 0), cex = cex, col = col2, font = 2)
}

caption <- function(word, y, col, cex = 0.62) {
  graphics::text(50, y, word, cex = cex, col = col, font = 1)
}

## ---- Proposal A: "The network" ----------------------------------------------
## Dark sticker: the countries of Europe in blue and every EEA sampling point
## as a small cyan dot, the wordmark below in white and amber.
draw_A <- function(path) {
  bg <- "#0b2540"; land <- "#174a73"; edge <- "#2f6a96"
  dot <- grDevices::adjustcolor("#7fe3f0", alpha.f = 0.55)
  amber <- "#f2b134"; word <- "#f4f7fa"
  m <- map_layer(box = c(2, 98, 9, 89), inset = 3.5)
  open_device(path)
  graphics::polygon(hex_x, hex_y, col = bg, border = NA)
  draw_sf(m$land, col = land, border = edge, lwd = 0.35)
  graphics::points(m$xy[, 1], m$xy[, 2], pch = 16, cex = 0.16, col = dot)
  wordmark(101.5, 27, word, amber, target = 46)
  graphics::polygon(hex_x, hex_y, border = amber, lwd = 5, ljoin = "round")
  invisible(grDevices::dev.off())
}

## ---- Proposal B: "Pollutants" -----------------------------------------------
## Light sticker: grey-blue Europe, sampling points coloured by the pollutant
## they measure (NO2, PM10, PM2.5, O3, others), navy wordmark.
draw_B <- function(path) {
  bg <- "#f5f8fb"; land <- "#dfe7ef"; edge <- "#b9c8d6"; navy <- "#13314f"
  pal <- c(NO2 = "#d1495b", PM10 = "#edae49", PM2.5 = "#00798c", O3 = "#30638e")
  m <- map_layer(box = c(2, 98, 9, 89), inset = 3.5)
  key <- ifelse(m$pollutant %in% names(pal), m$pollutant, "other")
  col <- c(pal, other = "#8aa1b5")[key]
  open_device(path)
  graphics::polygon(hex_x, hex_y, col = bg, border = NA)
  draw_sf(m$land, col = land, border = edge, lwd = 0.35)
  ord <- order(key != "other")                       # coloured dots on top
  graphics::points(m$xy[ord, 1], m$xy[ord, 2], pch = 16, cex = 0.16,
                   col = grDevices::adjustcolor(col[ord], alpha.f = 0.75))
  wordmark(101.5, 27, navy, pal[["PM2.5"]], target = 46)
  graphics::polygon(hex_x, hex_y, border = navy, lwd = 5, ljoin = "round")
  invisible(grDevices::dev.off())
}

## ---- Proposal C: "Map and series" -------------------------------------------
## Dark teal sticker: a smaller map of the network above an hourly
## concentration trace that crosses a dashed limit value, the two sides of the
## package (where the stations are, what they measure).
draw_C <- function(path) {
  bg <- "#0f2a3a"; land <- "#1d4a5e"; edge <- "#2d6a80"
  dot <- grDevices::adjustcolor("#9be7d8", alpha.f = 0.55)
  amber <- "#f2b134"; teal <- "#5cc4c9"; word <- "#f4f7f8"
  m <- map_layer(box = c(8, 92, 8, 66), inset = 3.5)
  open_device(path)
  graphics::polygon(hex_x, hex_y, col = bg, border = NA)
  draw_sf(m$land, col = land, border = edge, lwd = 0.3)
  graphics::points(m$xy[, 1], m$xy[, 2], pch = 16, cex = 0.13, col = dot)
  ## a decorative concentration trace (a slow cycle with daily peaks) and a
  ## dashed limit value; the stretches above the limit are drawn in amber
  t <- seq(0, 1, length.out = 400)
  conc <- 0.45 + 0.25 * sin(2 * pi * 2.2 * t - 1.2) + 0.18 * sin(2 * pi * 9 * t)^2
  x <- 10 + 80 * t
  y <- 86 - 16 * conc
  limit <- 86 - 16 * 0.78
  graphics::segments(10, limit, 90, limit, col = grDevices::adjustcolor(word, 0.5),
                     lwd = 1.2, lty = 2)
  graphics::lines(x, y, col = teal, lwd = 2.6, lend = "round", ljoin = "round")
  above <- ifelse(y < limit, y, NA)
  graphics::lines(x, above, col = amber, lwd = 2.6, lend = "round", ljoin = "round")
  wordmark(102, 24, word, amber, target = 40)
  graphics::polygon(hex_x, hex_y, border = teal, lwd = 5, ljoin = "round")
  invisible(grDevices::dev.off())
}

## ---- Render -------------------------------------------------------------------
drawers <- list(A = draw_A, B = draw_B, C = draw_C)
if (!chosen %in% names(drawers)) stop("Unknown proposal: ", chosen)
## PNG only: with some 20,000 station dots an SVG weighs about 10 MB.
for (id in names(drawers)) {
  drawers[[id]](file.path(out_dir, sprintf("logo-%s.png", id)))
}
invisible(file.copy(file.path(out_dir, sprintf("logo-%s.png", chosen)),
                    file.path(out_dir, "logo.png"), overwrite = TRUE))
message("Proposals written to ", out_dir, "; logo.png = proposal ", chosen)
