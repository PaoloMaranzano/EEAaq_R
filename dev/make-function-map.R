## ---------------------------------------------------------------------------
## Generates the map of the EEAaq package, in three files:
##
##   inst/extdata/EEAaq_function_map.svg   for the README and the vignette
##   inst/extdata/EEAaq_function_map.pdf   for print and slides
##   inst/extdata/EEAaq_function_map.md    the layers, the complete call list
##                                         (static scan of R/ at generation
##                                         time), the objects and the network
##                                         access of every function
##
## and copies the SVG to man/figures/function-map.svg (README) and
## vignettes/function-map.svg (vignette "Map of the package").
##
## Same approach as the maps of HierarchicalETI and Stem. The layout is
## explicit rather than computed: the reading imposed on the package is five
## horizontal bands, from the sources of the data down to the maps, and three
## columns: the measurements on the left, the metadata in the middle, the
## maintainers' tools on the right. The SVG coordinate system is used
## throughout (960 by 800 user units, y growing downwards); the PDF device is
## opened at one point per unit, so text and lines have the same size in both.
##
## Run it from the root of the repository:
##
##   Rscript dev/make-function-map.R
##
## After adding or renaming a function: put it in the `layers` table (the
## script stops if a function of R/ is missing from it), give it a box and
## its edges if it deserves them, then rerun. The script also stops if a grey
## arrow does not correspond to a call found in the sources, or if a box
## overlaps another box or an edge runs through a box.
## ---------------------------------------------------------------------------
if (!file.exists("DESCRIPTION") || !dir.exists("R")) {
  stop("Run this script from the root of the EEAaq_R repository.", call. = FALSE)
}
W <- 960
H <- 800
out <- file.path("inst", "extdata")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

pal <- list(
  paper = "#ffffff",
  band_fill = "#f4f6f8", band_line = "#dfe4ea", band_lab = "#8a939f",
  exp_fill = "#e8f2f6", exp_line = "#1f6f8b",
  int_fill = "#ffffff", int_line = "#b9c1cb",
  ext_fill = "#f5efe4", ext_line = "#b08d47",
  ink = "#1b2430", ink_soft = "#3d4753", note = "#6b7480",
  edge = "#5b6472", accent = "#1f6f8b", badge = "#b08d47"
)

## ---------------------------------------------------------------------------
## Static scan of the sources with the R parser: the top-level functions and
## the package functions that each of them calls. Functions defined inside
## another one (the nested helpers) count as part of their parent.
## ---------------------------------------------------------------------------
ns <- readLines("NAMESPACE", warn = FALSE)
exported <- gsub("\"", "", sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE)))
defs <- list()
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) {
  ex <- parse(f, keep.source = TRUE)
  sr <- attr(ex, "srcref")
  for (k in seq_along(ex)) {
    e <- ex[[k]]
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") &&
        is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function"))) {
      defs[[as.character(e[[2]])]] <- list(file = basename(f), line = sr[[k]][1], fun = e[[3]])
    }
  }
}
fun_names <- names(defs)

## every call in an expression: the name of the function and, for
## EEAaq_get_dataframe(), the table it asks for
scan_calls <- function(x) {
  calls <- character(0); tables <- character(0); nested <- character(0)
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- x[[1]]
    if (is.name(h)) {
      nm <- as.character(h)
      calls <<- c(calls, nm)
      if (nm == "EEAaq_get_dataframe" && length(x) > 1) {
        a <- as.list(x)[-1]
        v <- if (!is.null(a$dataframe)) a$dataframe else a[[1]]
        if (is.character(v)) tables <<- c(tables, v)
      }
      if (nm %in% c("<-", "=") && length(x) == 3 && is.name(x[[2]]) &&
          is.call(x[[3]]) && identical(x[[3]][[1]], as.name("function"))) {
        nested <<- c(nested, as.character(x[[2]]))
      }
    } else if (is.call(h) && as.character(h[[1]]) %in% c("::", ":::") &&
               as.character(h[[2]]) == "EEAaq") {
      calls <<- c(calls, as.character(h[[3]]))
    }
    for (a in as.list(x)[-1]) if (!missing(a)) walk(a)
  }
  walk(x)
  list(calls = unique(calls), tables = unique(tables), nested = unique(nested))
}
scan <- lapply(fun_names, function(fn) scan_calls(defs[[fn]]$fun[[3]]))
names(scan) <- fun_names
calls <- lapply(fun_names, function(fn) setdiff(intersect(scan[[fn]]$calls, fun_names), fn))
names(calls) <- fun_names

## who reads Support_files/ on GitHub: EEAaq_get_dataframe() and every
## function that reaches it through the calls above
reads_metadata <- "EEAaq_get_dataframe"
repeat {
  more <- fun_names[vapply(fun_names, function(fn) any(calls[[fn]] %in% reads_metadata), logical(1))]
  if (all(more %in% reads_metadata)) break
  reads_metadata <- union(reads_metadata, more)
}

## ---------------------------------------------------------------------------
## Layers: the reading of the package (drives the Markdown). Every top-level
## function of R/ must appear here.
## ---------------------------------------------------------------------------
layers <- list(
  "Metadata, read at run time from GitHub" = c("EEAaq_get_dataframe", "EEAaq_get_stations"),
  "Builders of Support_files/, used by the maintainers only" =
    c("get_stations", "get_pollutants", "get_NUTS", "get_LAU"),
  "Download and storage" = c("EEAaq_get_data", "handle_dates", "is_EEAaq_df",
                             "EEAaq_export", "EEAaq_import"),
  "Description and time aggregation" = c("EEAaq_summary", "EEAaq_time_aggregate", "my_summarise"),
  "Maps" = c("EEAaq_map_stations", "EEAaq_idw_map", "code_extr")
)
missing_fn <- setdiff(fun_names, unlist(layers))
if (length(missing_fn)) {
  stop("Functions of R/ missing from the `layers` table: ",
       paste(missing_fn, collapse = ", "), call. = FALSE)
}

## ---------------------------------------------------------------------------
## Geometry: five bands, three columns (x = 40-310, 345-615, 650-920)
## ---------------------------------------------------------------------------
B1 <- 88; B2 <- 196; B3 <- 344; B4 <- 484; B5 <- 610
bands <- data.frame(
  x = 24, y = c(B1, B2, B3, B4, B5), w = 912, h = c(92, 132, 124, 110, 110),
  label = c("SOURCES, OUTSIDE THE PACKAGE", "METADATA", "DOWNLOAD AND STORAGE",
            "DESCRIPTION AND AGGREGATION", "MAPS"),
  stringsAsFactors = FALSE
)
## kind: "exported", "internal", "ext" (data outside the package or an object
## a function returns). fn: the function in the box, for the badge and the
## checks (NA for a box that holds several functions or none).
box <- function(x, y, w, h, label, kind, fn = NA, size = NA) {
  data.frame(x = x, y = y, w = w, h = h, label = label, kind = kind, fn = fn,
             size = size, stringsAsFactors = FALSE)
}
boxes <- rbind(
  ## band 1: the sources
  box( 40, B1 + 30, 270, 50, "EEA Air Quality Download Service\nparquet files: Airbase, E1a, E2a", "ext", size = 11),
  box(345, B1 + 30, 270, 50, "Support_files/ on GitHub\nstations, pollutant, NUTS, LAU", "ext", size = 11),
  box(650, B1 + 30, 270, 50, "EEA, EIONET and Eurostat GISCO\nthe raw metadata", "ext", size = 11),
  ## band 2: the metadata
  box(345, B2 + 32, 270, 32, "EEAaq_get_dataframe", "exported", "EEAaq_get_dataframe"),
  box(345, B2 + 88, 270, 32, "EEAaq_get_stations", "exported", "EEAaq_get_stations"),
  box(650, B2 + 32, 270, 50, "get_stations, get_pollutants\nget_NUTS, get_LAU", "internal", size = 11.5),
  box(650, B2 + 90, 270, 30, "run by dev/update-support-files.R", "ext", size = 10.5),
  ## band 3: download and storage
  box( 40, B3 + 32,  85, 34, "handle_dates", "internal", "handle_dates", 10.5),
  box(135, B3 + 32, 175, 34, "EEAaq_get_data", "exported", "EEAaq_get_data"),
  box( 40, B3 + 76, 270, 36, "EEAaq_df: one row per station and time", "ext", size = 10.5),
  box(345, B3 + 32, 130, 34, "EEAaq_export", "exported", "EEAaq_export", 12),
  box(485, B3 + 32, 130, 34, "EEAaq_import", "exported", "EEAaq_import", 12),
  box(345, B3 + 76, 270, 36, "CSV file, read back as an EEAaq_df_sfc", "ext", size = 10.5),
  box(650, B3 + 32, 270, 34, "is_EEAaq_df", "exported", "is_EEAaq_df"),
  box(650, B3 + 76, 270, 36, "TRUE for an EEAaq_df or an EEAaq_df_sfc", "ext", size = 10.5),
  ## band 4: description and time aggregation
  box( 40, B4 + 32, 270, 34, "EEAaq_summary", "exported", "EEAaq_summary"),
  box( 40, B4 + 74, 270, 28, "statistics, gaps and correlations", "ext", size = 10.5),
  box(345, B4 + 32, 270, 34, "EEAaq_time_aggregate", "exported", "EEAaq_time_aggregate"),
  box(345, B4 + 74, 270, 28, "my_summarise", "internal", "my_summarise"),
  box(650, B4 + 32, 270, 34, "EEAaq_taggr_df or EEAaq_taggr_list", "ext", size = 11),
  ## band 5: maps
  box( 40, B5 + 32, 270, 34, "EEAaq_map_stations", "exported", "EEAaq_map_stations"),
  box( 40, B5 + 74, 270, 28, "ggplot2 or leaflet map of the stations", "ext", size = 10.5),
  box(410, B5 + 36, 140, 26, "code_extr", "internal", "code_extr"),
  box(650, B5 + 32, 270, 34, "EEAaq_idw_map", "exported", "EEAaq_idw_map"),
  box(650, B5 + 74, 270, 28, "IDW maps: PDF, PNG, JPEG, GIF or HTML", "ext", size = 10.5)
)

## Edges. A straight edge is given by its two endpoints, a curved one by the
## four control points of a cubic Bezier (start, c1, c2, end). accent = TRUE
## for the blue arrows, which follow the data and the objects; grey arrows are
## calls from caller to callee, and name both functions in `from` and `to`,
## which the script checks against the scan of the sources. head = FALSE for
## the segments that feed a bus, which carry no arrowhead.
seg <- function(x1, y1, x2, y2, accent = FALSE, dashed = FALSE, from = NA, to = NA, head = TRUE) {
  list(type = "L", pts = c(x1, y1, x2, y2), accent = accent, dashed = dashed,
       from = from, to = to, head = head)
}
cur <- function(x1, y1, cx1, cy1, cx2, cy2, x2, y2, accent = FALSE, dashed = FALSE,
                from = NA, to = NA, head = TRUE) {
  list(type = "C", pts = c(x1, y1, cx1, cy1, cx2, cy2, x2, y2), accent = accent,
       dashed = dashed, from = from, to = to, head = head)
}
BUS <- 16   # the EEAaq_df runs down the left margin to EEAaq_summary and EEAaq_map_stations
edges <- list(
  ## the data: sources -> functions -> objects -> functions
  seg(250, B1 + 80, 250, B3 + 32, accent = TRUE),                            # parquet files -> EEAaq_get_data
  seg(480, B1 + 80, 480, B2 + 32, accent = TRUE),                            # Support_files -> EEAaq_get_dataframe
  seg(785, B1 + 80, 785, B2 + 32, accent = TRUE, dashed = TRUE),             # raw metadata -> builders
  cur(668, B2 + 32, 668, B2, 625, B2 - 6, 600, B1 + 80, accent = TRUE, dashed = TRUE),  # builders -> Support_files
  cur(345, B2 + 104, 300, B2 + 104, 280, B2 + 124, 280, B3 + 32, accent = TRUE),  # station codes -> EEAaq_get_data
  seg(222, B3 + 66, 222, B3 + 76, accent = TRUE),                            # EEAaq_get_data -> EEAaq_df
  seg(310, B3 + 86, 345, B3 + 56, accent = TRUE),                            # EEAaq_df -> EEAaq_export
  seg(410, B3 + 66, 410, B3 + 76, accent = TRUE),                            # EEAaq_export -> CSV
  seg(550, B3 + 76, 550, B3 + 66, accent = TRUE),                            # CSV -> EEAaq_import
  seg(785, B3 + 66, 785, B3 + 76, accent = TRUE),                            # is_EEAaq_df -> TRUE/FALSE
  seg(40, B3 + 94, BUS, B3 + 94, accent = TRUE, head = FALSE),               # EEAaq_df -> the bus
  seg(BUS, B3 + 94, BUS, B5 + 49, accent = TRUE, head = FALSE),              # the bus
  seg(BUS, B4 + 49, 40, B4 + 49, accent = TRUE),                             # the bus -> EEAaq_summary
  seg(BUS, B5 + 49, 40, B5 + 49, accent = TRUE),                             # the bus -> EEAaq_map_stations
  cur(260, B3 + 112, 260, B4 + 6, 420, B4 + 2, 420, B4 + 32, accent = TRUE), # EEAaq_df -> EEAaq_time_aggregate
  seg(175, B4 + 66, 175, B4 + 74, accent = TRUE),                            # EEAaq_summary -> its tables
  seg(615, B4 + 49, 650, B4 + 49, accent = TRUE),                            # EEAaq_time_aggregate -> taggr
  seg(785, B4 + 66, 785, B5 + 32, accent = TRUE),                            # taggr -> EEAaq_idw_map
  seg(175, B5 + 66, 175, B5 + 74, accent = TRUE),                            # EEAaq_map_stations -> map
  seg(785, B5 + 66, 785, B5 + 74, accent = TRUE),                            # EEAaq_idw_map -> maps
  ## the calls
  seg(480, B2 + 88, 480, B2 + 64, from = "EEAaq_get_stations", to = "EEAaq_get_dataframe"),
  seg(650, B2 + 54, 615, B2 + 48, from = "get_stations", to = "EEAaq_get_dataframe"),
  seg(135, B3 + 49, 125, B3 + 49, from = "EEAaq_get_data", to = "handle_dates"),
  cur(310, B4 + 49, 328, B4 + 49, 328, B2 + 134, 360, B2 + 120,
      from = "EEAaq_summary", to = "EEAaq_get_stations"),
  seg(480, B4 + 66, 480, B4 + 74, from = "EEAaq_time_aggregate", to = "my_summarise"),
  seg(310, B5 + 49, 410, B5 + 49, from = "EEAaq_map_stations", to = "code_extr"),
  seg(650, B5 + 49, 550, B5 + 49, from = "EEAaq_idw_map", to = "code_extr")
)

## labels set along an edge, in italics, ending at x (right-aligned)
edge_labels <- list(
  list(x = 240, y = B2 + 66, text = "parquet files, through the API"),
  list(x = 335, y = B2 + 96, text = "IDstations")
)
dots <- data.frame(x = BUS, y = c(B3 + 94, B4 + 49))

titles <- list(
  list(x = 24, y = 30, size = 17, bold = TRUE, col = pal$ink, text = "EEAaq: how the functions fit together"),
  list(x = 24, y = 50, size = 11, bold = FALSE, col = pal$note,
       text = "Blue boxes are exported functions, white boxes internal ones, sand boxes data outside the package and the objects the functions return."),
  list(x = 24, y = 64, size = 11, bold = FALSE, col = pal$note,
       text = "Blue arrows follow the data and the objects, grey arrows are function calls from caller to callee; dashed arrows are the maintainers' tools."),
  list(x = 38, y = 78, size = 11, bold = FALSE, col = pal$note,
       text = "reads the metadata in Support_files/ through EEAaq_get_dataframe(), so it needs an internet connection")
)
legend_dot <- c(x = 29, y = 74.5)
footnotes <- list(
  list(x = 24, y = 748, text = "EEAaq_get_data() splits the period with handle_dates() across Airbase (until 2012), E1a (2013-2024) and E2a (from 2025),"),
  list(x = 24, y = 762, text = "asks the API for the parquet files of the chosen stations and reads them with arrow. The builders are not called by the exported functions:"),
  list(x = 24, y = 776, text = "the maintainers run them to rebuild Support_files/. Generated by dev/make-function-map.R from the sources; the complete call list is in"),
  list(x = 24, y = 790, text = "inst/extdata/EEAaq_function_map.md.")
)

style_of <- function(kind) switch(kind,
  exported = list(fill = pal$exp_fill, line = pal$exp_line, lwd = 1.6, col = pal$ink, size = 12.5, bold = TRUE),
  ext = list(fill = pal$ext_fill, line = pal$ext_line, lwd = 1.0, col = pal$ink_soft, size = 10.5, bold = FALSE),
  list(fill = pal$int_fill, line = pal$int_line, lwd = 1.1, col = pal$ink_soft, size = 11.5, bold = FALSE))

## a badge on the boxes of the functions that read Support_files/
badges <- boxes[!is.na(boxes$fn) & boxes$fn %in% reads_metadata, ]
badges <- data.frame(x = badges$x + badges$w - 9, y = badges$y + 9)

bezier_pts <- function(p, n = 60) {
  tt <- seq(0, 1, length.out = n)
  b0 <- (1 - tt)^3; b1 <- 3 * (1 - tt)^2 * tt; b2 <- 3 * (1 - tt) * tt^2; b3 <- tt^3
  cbind(x = b0 * p[1] + b1 * p[3] + b2 * p[5] + b3 * p[7],
        y = b0 * p[2] + b1 * p[4] + b2 * p[6] + b3 * p[8])
}
edge_path <- function(e, n = 60) {
  if (e$type == "L") cbind(x = e$pts[c(1, 3)], y = e$pts[c(2, 4)]) else bezier_pts(e$pts, n)
}

## ---------------------------------------------------------------------------
## Checks: the grey arrows are real calls; no box overlaps another one or
## leaves its band; no edge runs through a box other than its two ends; no
## edge label covers a box.
## ---------------------------------------------------------------------------
check_layout <- function() {
  bad <- character(0)
  for (e in edges) {
    if (!is.na(e$from) && !(e$to %in% calls[[e$from]])) {
      bad <- c(bad, sprintf("the arrow %s -> %s is not a call in the sources", e$from, e$to))
    }
  }
  pad <- 2
  for (k in seq_along(edges)) {
    p <- edge_path(edges[[k]], n = 200)
    if (edges[[k]]$type == "L") {
      tt <- seq(0, 1, length.out = 200)
      p <- cbind(x = p[1, 1] + tt * (p[2, 1] - p[1, 1]), y = p[1, 2] + tt * (p[2, 2] - p[1, 2]))
    }
    for (i in seq_len(nrow(boxes))) {
      b <- boxes[i, ]
      inside <- p[, 1] > b$x + pad & p[, 1] < b$x + b$w - pad &
                p[, 2] > b$y + pad & p[, 2] < b$y + b$h - pad
      j <- which(inside)
      if (any(j > 0.10 * nrow(p) & j < 0.90 * nrow(p))) {
        bad <- c(bad, sprintf("edge %d crosses the box '%s'", k, gsub("\n", " ", b$label)))
      }
    }
  }
  for (i in seq_len(nrow(boxes))) for (j in seq_len(nrow(boxes))) if (i < j) {
    a <- boxes[i, ]; b <- boxes[j, ]
    if (a$x < b$x + b$w && b$x < a$x + a$w && a$y < b$y + b$h && b$y < a$y + a$h) {
      bad <- c(bad, sprintf("the box '%s' overlaps the box '%s'", a$label, b$label))
    }
  }
  for (i in seq_len(nrow(boxes))) {
    b <- boxes[i, ]
    if (!any(bands$x <= b$x & b$x + b$w <= bands$x + bands$w &
             bands$y <= b$y & b$y + b$h <= bands$y + bands$h)) {
      bad <- c(bad, sprintf("the box '%s' is outside every band", b$label))
    }
  }
  ## text: the width is measured on a null PDF device, plus the letter spacing
  ## of the band labels in the SVG and a margin of 10% for the browser fonts
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  width_of <- function(text, size, font) graphics::strwidth(text, units = "inches", cex = size / 12, font = font) * 72
  texts <- c(
    lapply(edge_labels, function(l) {
      w <- 1.1 * width_of(l$text, 10.5, 3)
      list(text = l$text, x0 = l$x - w, x1 = l$x, y0 = l$y - 9, y1 = l$y + 3)
    }),
    lapply(seq_len(nrow(bands)), function(i) {
      b <- bands[i, ]
      w <- 1.1 * (width_of(b$label, 12, 2) + 0.96 * nchar(b$label))
      list(text = b$label, x0 = b$x + 12, x1 = b$x + 12 + w, y0 = b$y + 9, y1 = b$y + 23)
    })
  )
  for (t in texts) {
    hit <- boxes$x < t$x1 & t$x0 < boxes$x + boxes$w & boxes$y < t$y1 & t$y0 < boxes$y + boxes$h
    if (any(hit)) bad <- c(bad, sprintf("the text '%s' covers a box", t$text))
    for (k in seq_along(edges)) {
      p <- edge_path(edges[[k]], n = 200)
      if (edges[[k]]$type == "L") {
        tt <- seq(0, 1, length.out = 200)
        p <- cbind(x = p[1, 1] + tt * (p[2, 1] - p[1, 1]), y = p[1, 2] + tt * (p[2, 2] - p[1, 2]))
      }
      if (any(p[, 1] > t$x0 - 3 & p[, 1] < t$x1 + 3 & p[, 2] > t$y0 - 3 & p[, 2] < t$y1 + 3)) {
        bad <- c(bad, sprintf("edge %d runs over the text '%s'", k, t$text))
      }
    }
  }
  if (length(bad)) stop("the layout is not clean:\n  ", paste(bad, collapse = "\n  "), call. = FALSE)
  invisible(TRUE)
}

## ---------------------------------------------------------------------------
## SVG
## ---------------------------------------------------------------------------
emit_svg <- function(path) {
  esc <- function(s) {
    s <- gsub("&", "&amp;", s, fixed = TRUE); s <- gsub("<", "&lt;", s, fixed = TRUE)
    gsub(">", "&gt;", s, fixed = TRUE)
  }
  L <- character(0); add <- function(...) L <<- c(L, paste0(...))
  add('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', W, ' ', H,
      '" font-family="ui-sans-serif, system-ui, -apple-system, \'Segoe UI\', Helvetica, Arial, sans-serif">')
  add('  <!-- Generated by dev/make-function-map.R. Do not edit by hand. -->')
  add('  <defs>')
  for (m in c("arrow", "arrowAccent")) {
    fill <- if (m == "arrow") pal$edge else pal$accent
    add('    <marker id="', m, '" viewBox="0 0 10 10" refX="9.5" refY="5" markerWidth="7" ',
        'markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="', fill, '"/></marker>')
  }
  add('    <style>')
  add('      .band { fill:', pal$band_fill, '; stroke:', pal$band_line, '; stroke-width:1; rx:10; }')
  add('      .bandlab { font-size:12px; font-weight:600; fill:', pal$band_lab, '; letter-spacing:.08em; }')
  add('      .exported { fill:', pal$exp_fill, '; stroke:', pal$exp_line, '; stroke-width:1.6; rx:7; }')
  add('      .internal { fill:', pal$int_fill, '; stroke:', pal$int_line, '; stroke-width:1.1; rx:6; }')
  add('      .ext { fill:', pal$ext_fill, '; stroke:', pal$ext_line, '; stroke-width:1; rx:6; }')
  add('      .edge { stroke:', pal$edge, '; stroke-width:1.3; fill:none; }')
  add('      .edgeA { stroke:', pal$accent, '; stroke-width:1.8; fill:none; }')
  add('      .foot { font-size:11px; fill:', pal$note, '; }')
  add('      .elab { font-size:10.5px; font-style:italic; fill:', pal$note, '; }')
  add('    </style>')
  add('  </defs>')
  add('  <rect width="', W, '" height="', H, '" fill="', pal$paper, '"/>')
  for (t in titles) {
    add('  <text x="', t$x, '" y="', t$y, '" font-size="', t$size, '"',
        if (t$bold) ' font-weight="700"' else '', ' fill="', t$col, '">', esc(t$text), '</text>')
  }
  add('  <circle cx="', legend_dot["x"], '" cy="', legend_dot["y"], '" r="4" fill="', pal$badge, '"/>')
  for (i in seq_len(nrow(bands))) {
    b <- bands[i, ]
    add('  <rect class="band" x="', b$x, '" y="', b$y, '" width="', b$w, '" height="', b$h, '"/>')
    add('  <text x="', b$x + 12, '" y="', b$y + 20, '" class="bandlab">', esc(b$label), '</text>')
  }
  for (e in edges) {
    d <- if (e$type == "L") paste("M", e$pts[1], e$pts[2], "L", e$pts[3], e$pts[4]) else
      paste("M", e$pts[1], e$pts[2], "C", e$pts[3], e$pts[4], e$pts[5], e$pts[6], e$pts[7], e$pts[8])
    add('  <path class="', if (e$accent) "edgeA" else "edge", '" d="', d, '"',
        if (e$dashed) ' stroke-dasharray="4 3"' else '',
        if (e$head) paste0(' marker-end="url(#', if (e$accent) "arrowAccent" else "arrow", ')"') else '', '/>')
  }
  for (i in seq_len(nrow(dots))) {
    add('  <circle cx="', dots$x[i], '" cy="', dots$y[i], '" r="3.2" fill="', pal$accent, '"/>')
  }
  for (i in seq_len(nrow(boxes))) {
    b <- boxes[i, ]; st <- style_of(b$kind); sz <- if (is.na(b$size)) st$size else b$size
    add('  <rect class="', b$kind, '" x="', b$x, '" y="', b$y, '" width="', b$w, '" height="', b$h, '"/>')
    parts <- strsplit(b$label, "\n", fixed = TRUE)[[1]]
    y0 <- b$y + b$h / 2 - (length(parts) - 1) * sz * 0.65 + sz / 3
    for (k in seq_along(parts)) {
      add('  <text x="', round(b$x + b$w / 2, 2), '" y="', round(y0 + (k - 1) * sz * 1.3, 2),
          '" text-anchor="middle" font-size="', sz, '"',
          if (st$bold && k == 1) ' font-weight="600"' else '', ' fill="', st$col, '">', esc(parts[k]), '</text>')
    }
  }
  for (i in seq_len(nrow(badges))) {
    add('  <circle cx="', badges$x[i], '" cy="', badges$y[i], '" r="4" fill="', pal$badge, '"/>')
  }
  for (l in edge_labels) {
    add('  <text x="', l$x, '" y="', l$y, '" text-anchor="end" class="elab">', esc(l$text), '</text>')
  }
  for (f in footnotes) add('  <text x="', f$x, '" y="', f$y, '" class="foot">', esc(f$text), '</text>')
  add('</svg>')
  writeLines(L, path, useBytes = TRUE)
  path
}

## ---------------------------------------------------------------------------
## PDF on a base-graphics device (one point per SVG unit, y flipped)
## ---------------------------------------------------------------------------
draw_base <- function() {
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot(NA, xlim = c(0, W), ylim = c(H, 0), axes = FALSE, xlab = "", ylab = "")
  graphics::rect(0, 0, W, H, col = pal$paper, border = NA)
  cex_of <- function(size) size / 12
  for (t in titles) {
    graphics::text(t$x, t$y, t$text, adj = c(0, 0.3), cex = cex_of(t$size),
                   font = if (t$bold) 2 else 1, col = t$col)
  }
  graphics::symbols(legend_dot["x"], legend_dot["y"], circles = 4, inches = FALSE, add = TRUE,
                    bg = pal$badge, fg = pal$badge)
  for (i in seq_len(nrow(bands))) {
    b <- bands[i, ]
    graphics::rect(b$x, b$y, b$x + b$w, b$y + b$h, col = pal$band_fill, border = pal$band_line)
    graphics::text(b$x + 12, b$y + 16, b$label, adj = c(0, 0.5), cex = cex_of(12), font = 2, col = pal$band_lab)
  }
  arrow_head <- function(p, col) {
    n <- nrow(p); d <- p[n, ] - p[n - 1, ]; d <- d / sqrt(sum(d^2)); nrm <- c(-d[2], d[1])
    tip <- p[n, ]; base <- tip - 7 * d
    graphics::polygon(c(tip[1], base[1] + 3.5 * nrm[1], base[1] - 3.5 * nrm[1]),
                      c(tip[2], base[2] + 3.5 * nrm[2], base[2] - 3.5 * nrm[2]), col = col, border = NA)
  }
  for (e in edges) {
    p <- edge_path(e); col <- if (e$accent) pal$accent else pal$edge
    graphics::lines(p[, 1], p[, 2], col = col, lwd = if (e$accent) 1.8 else 1.3, lty = if (e$dashed) 2 else 1)
    if (e$head) arrow_head(p, col)
  }
  graphics::points(dots$x, dots$y, pch = 16, col = pal$accent, cex = 0.7)
  for (i in seq_len(nrow(boxes))) {
    b <- boxes[i, ]; st <- style_of(b$kind); sz <- if (is.na(b$size)) st$size else b$size
    graphics::rect(b$x, b$y, b$x + b$w, b$y + b$h, col = st$fill, border = st$line, lwd = st$lwd)
    parts <- strsplit(b$label, "\n", fixed = TRUE)[[1]]
    y0 <- b$y + b$h / 2 - (length(parts) - 1) * sz * 0.65
    for (k in seq_along(parts)) {
      graphics::text(b$x + b$w / 2, y0 + (k - 1) * sz * 1.3, parts[k], cex = cex_of(sz),
                     font = if (st$bold && k == 1) 2 else 1, col = st$col)
    }
  }
  if (nrow(badges)) {
    graphics::symbols(badges$x, badges$y, circles = rep(4, nrow(badges)), inches = FALSE, add = TRUE,
                      bg = pal$badge, fg = pal$badge)
  }
  for (l in edge_labels) graphics::text(l$x, l$y, l$text, adj = c(1, 0.3), cex = cex_of(10.5), font = 3, col = pal$note)
  for (f in footnotes) graphics::text(f$x, f$y, f$text, adj = c(0, 0.3), cex = cex_of(11), col = pal$note)
}
emit_pdf <- function(path) {
  if (capabilities("cairo")) {
    grDevices::cairo_pdf(path, width = W / 72, height = H / 72)
  } else {
    grDevices::pdf(path, width = W / 72, height = H / 72, family = "Helvetica")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_base()
  path
}

## ---------------------------------------------------------------------------
## Markdown: the layers, the calls, the objects and the network access
## ---------------------------------------------------------------------------
emit_md <- function(path) {
  mark <- function(f) if (f %in% exported) paste0("`", f, "`*") else paste0("`", f, "`")
  L <- c(
    "# Map of the EEAaq package",
    "",
    "How the functions of the package call one another. The diagram is in",
    "`EEAaq_function_map.svg` (and `.pdf` for print); the narrative version is",
    "the vignette `function-map`. The three files are generated by",
    "`dev/make-function-map.R` in the GitHub repository: the layers are declared",
    "in the script, the calls are found by a static scan of the sources with",
    "the R parser when the map is generated, so they cannot drift from the code.",
    "Exported functions are marked with `*`. A function defined inside another",
    "one (a nested helper) is counted as part of its parent.",
    "",
    "## Layers", ""
  )
  for (nm in names(layers)) {
    L <- c(L, paste0("### ", nm), "", paste(vapply(layers[[nm]], mark, character(1)), collapse = ", "), "")
  }
  L <- c(L, "## Calls, function by function", "",
         "Only the calls to other functions of the package are listed, with the",
         "metadata tables asked to `EEAaq_get_dataframe()`.", "")
  for (fn in unlist(layers)) {
    d <- defs[[fn]]; s <- scan[[fn]]
    items <- if (length(calls[[fn]])) paste(vapply(calls[[fn]], mark, character(1)), collapse = ", ") else "no package function"
    extra <- c(if (length(s$tables)) paste0("reads ", paste0("`", s$tables, "`", collapse = ", ")),
               if (length(s$nested)) paste0("nested helpers ", paste0("`", s$nested, "`", collapse = ", ")))
    L <- c(L, paste0("* ", mark(fn), " (`R/", d$file, "`, line ", d$line, ") -> ", items,
                     if (length(extra)) paste0("; ", paste(extra, collapse = "; ")) else ""))
  }
  L <- c(L, "",
    "## Objects", "",
    "| Object | Returned by | Accepted by |",
    "|---|---|---|",
    "| `EEAaq_df` | `EEAaq_get_data()` | `EEAaq_summary()`, `EEAaq_time_aggregate()`, `EEAaq_map_stations()`, `EEAaq_export()`, `is_EEAaq_df()` |",
    "| `EEAaq_df_sfc` | `EEAaq_import()` | `EEAaq_time_aggregate()`, `EEAaq_export()`, `is_EEAaq_df()` |",
    "| `EEAaq_taggr_df`, `EEAaq_taggr_df_sfc` | `EEAaq_time_aggregate()`, with one pollutant | `EEAaq_idw_map()` |",
    "| `EEAaq_taggr_list` | `EEAaq_time_aggregate()`, with more than one pollutant: all of them in `TimeAggr`, one `EEAaq_taggr_df` per pollutant in `TimeAggr_byPollutant` | `EEAaq_idw_map()` |",
    "",
    "## Network access", "",
    "* The EEA Air Quality Download Service: `EEAaq_get_data()`, through the",
    "  API (`ParquetFile/urls`), which returns the URLs of the parquet files.",
    paste0("* `Support_files/` on GitHub, through `EEAaq_get_dataframe()`: ",
           paste(vapply(sort(reads_metadata), mark, character(1)), collapse = ", "), "."),
    "* The builders of `Support_files/` read the EEA station metadata, the",
    "  EIONET vocabulary of pollutants and the Eurostat GISCO boundaries.",
    "",
    "Nothing of this may run while CRAN checks the package: the examples of",
    "these functions are in `\\dontrun{}` and the main vignette is precomputed.",
    "")
  writeLines(L, path, useBytes = TRUE)
  path
}

check_layout()
svg_path <- emit_svg(file.path(out, "EEAaq_function_map.svg"))
pdf_path <- emit_pdf(file.path(out, "EEAaq_function_map.pdf"))
md_path <- emit_md(file.path(out, "EEAaq_function_map.md"))
invisible(file.copy(svg_path, file.path("man", "figures", "function-map.svg"), overwrite = TRUE))
invisible(file.copy(svg_path, file.path("vignettes", "function-map.svg"), overwrite = TRUE))
message("wrote ", svg_path, ", ", pdf_path, " and ", md_path,
        "\ncopied the SVG to man/figures/ and vignettes/\n",
        nrow(boxes), " boxes, ", length(edges), " edges, ", length(fun_names), " functions scanned, ",
        length(reads_metadata), " read Support_files/")
