# EEAaq 1.0.4

* Resubmission after the archiving of 1.0.3 by CRAN (2026-08-26): the
  vignette no longer downloads data; it reads a small `EEAaq_df` object
  shipped in `inst/extdata/vignette_data.rds` (NO2 and PM10 at the stations
  of the province of Milano, 1-15 January 2024).
* Examples that need the EEA service or the metadata on GitHub are wrapped
  in `\dontrun{}`.
* The maintainer e-mail in `DESCRIPTION` is again
  pmaranzano.ricercastatistica@gmail.com.
* The GitHub repository has been reorganised: the package is at the root of
  <https://github.com/PaoloMaranzano/EEAaq_R>, with a README, a logo and the
  maintenance scripts in `dev/`.

# EEAaq 1.0.3 (CRAN, 2026-02-10)

* `EEAaq_time_aggregate()` and `EEAaq_summary()` revised.
* R-hub workflow for the CRAN checks.

# EEAaq 1.0.2 (CRAN, 2026-01-21)

* Extended vignette (`vignettes/vignettes.Rmd`), which downloaded the Milano
  example from the EEA service when it was built: the cause of the CRAN
  check error that led to the archiving of 1.0.3.
* Station metadata in `Support_files/` updated (`stations.rds`), with the
  city names and codes completed from the EEA Air Quality Statistics viewer
  (`missing_cities.rds`).

# EEAaq 1.0.1 (CRAN, 2025-03-20)

* Maintenance release: revision of the R functions, including the version of
  `EEAaq_get_data()` developed by Samir Doghmi in February 2025.

# EEAaq 1.0.0 (CRAN, 2025-02-06)

* Rewritten for the new EEA Air Quality Download Service, which since the
  end of 2024 distributes the data as parquet files through an API: the
  measurements are read with arrow and split automatically across the
  Airbase, E1a and E2a datasets.
* Stations are selected by their EEA codes (`IDstations`), found through
  `EEAaq_get_stations()`.
* The metadata (stations, pollutants, NUTS and LAU boundaries) are no longer
  stored in the package: they are pre-processed by the maintainers and read
  from `Support_files/` in the GitHub repository
  (`EEAaq_get_dataframe()`).
* Samir Doghmi joins the authors.

# EEAaq 0.0.3 (CRAN, 2023-08-23)

# EEAaq 0.0.2 (CRAN, 2023-07-17)

# EEAaq 0.0.1 (CRAN, 2023-05-29)

* First release, developed by Agostino Tassan Mazzocco with Paolo Maranzano
  and Riccardo Borgoni (sources and history in `archive/EEAaq-v0/`): download
  of EEA data by NUTS/LAU zone, bounding box or polygon, station maps,
  summaries, time aggregation and IDW interpolation maps.
