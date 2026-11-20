# Support files

Metadata read by EEAaq at run time, by Maranzano, Borgoni, Tassan Mazzocco
and Doghmi.

> **Do not move, rename or delete these files.** Every installed copy of
> EEAaq (1.0.0 onwards) downloads them from
> `https://github.com/PaoloMaranzano/EEAaq_R/raw/refs/heads/main/Support_files/<name>.rds`:
> a change of path or of columns on `main` breaks the package for all its
> users at once.

| File | Read by | Content |
|---|---|---|
| `stations.rds` | `EEAaq_get_dataframe("stations")`, `EEAaq_get_stations()`, `EEAaq_get_data()`, `EEAaq_summary()`, the maps | one row per sampling point of the EEA network, with station codes, coordinates, type, pollutant and its LAU/NUTS codes and names |
| `pollutant.rds` | `EEAaq_get_dataframe("pollutant")`, `EEAaq_get_data()`, the maps | the EEA vocabulary of pollutants: notation, code, unit |
| `NUTS.rds` | `EEAaq_get_dataframe("NUTS")`, `EEAaq_get_data()`, `EEAaq_export()`, the maps | NUTS 0-3 regions with geometries (GISCO, 1:20 million, EPSG:4326) |
| `LAU.rds` | `EEAaq_get_dataframe("LAU")`, `EEAaq_export()`, the maps | Local Administrative Units (municipalities) with geometries |
| `missing_cities.rds` | `get_stations()` (maintainers only) | city names and codes of the stations, collected by hand (see below) |

## How to update them

Build the new files with `dev/update-support-files.R` (see its header) on a
branch, check that the columns are unchanged, then commit and merge into
`main`: the update is live for all the users as soon as it reaches `main`.
Never put a GitHub token in a file of this repository; the files go through
git like any other file.

The sections below describe how each file is built.


## **1. LAU (Local Administrative Units)**
To get the final dataframe we combine two datasets: one taken from Eurostat [https://ec.europa.eu/eurostat/web/nuts/local-administrative-units] that includes City names and City IDs, essential for querying and associations. The other one taken from EEA which provides LAU information. The Latter dataset is updated automatically by selecting the most recent shapefile (SHP) available online,  while The Eurostat dataset URL needs to be manually updated with the latest download link to ensure the City-related data is current.  

## **2. NUTS (Nomenclature of Territorial Units for Statistics)**
It automatically updates the dataset by identifying the most recent available file, accessing the corresponding page, and downloading the SHP file  at the 1:20 Million scale with the EPSG:4326 reference system from this website [https://gisco-services.ec.europa.eu/distribution/v2/nuts/].


## **3. Pollutant**
it retrieves Pollutant Data from EEA Vocabulary [https://dd.eionet.europa.eu/vocabulary/aq/pollutant]. To update the dataset, you have to change the direct link for downloading the csv file.

## **4. Stations**
It downloads detailed information for each SamplingPointId. It performs a spatial join to merge the spatial information of LAU and NUTS (specifically, the geometries of LAU and the geometry of stations). It fills in the missing data for  `CITY_NAME` and  `CITY_ID` retrieved from [https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.b2g.AirQualityStatistics] through a left join based on the AirQualityStationEoICode column.  To update the dataset, you have to change the direct link for downloading the csv file.
  - **Missing cities**:  
    This file is obtained manually (from 2000 to 2024) because the website did not allow downloading more than 100,000 rows at a time. The data was collected in multiple batches, filtering SamplingPoints using the following criteria:
    - **Filter on data used in AQ Report**: Yes  
    - **Filter on data coverage**: Yes  

    For each station:
    - The column `AirQualityStationEoICode` (identical for all sensors at the same station) was used to select the first row containing unique values for `CITY_NAME` and `CITY_ID`.
    - No station reported more than one value for this pair of columns.
    - To support future uploads, it is necessary to integrate updated `AirQualityStationEoICode` values.

