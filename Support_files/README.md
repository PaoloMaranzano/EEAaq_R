# EEAaq_R_Support
Support folder for the EEAaq R package by Maranzano-Borgoni-Tassan Mazzocco-Doghmi (Update 2024)


## **1. LAU (Local Administrative Units)**
To get the final dataframe we combine two dataset: one taken from Eurostat [https://ec.europa.eu/eurostat/web/nuts/local-administrative-units] that includes City names and City IDs, essential for querying and associations. The other one taken from EEA which provides LAU information. The Latter dataset is updated automatically by selecting the most recent shapefile (SHP) available online,  while The Eurostat dataset URL needs to be manually updated with the latest download link to ensure the City-related data is current.

## **2. NUTS (Nomenclature of Territorial Units for Statistics)**
It automatically updates the dataset by identifying the most recent available file, accessing the corresponding page, and downloading the SHP file  at the 1:20 Million scale with the EPSG:4326 reference system from this website [https://gisco-services.ec.europa.eu/distribution/v2/nuts/].


## **3. Pollutant**
it retrieve Pollutant Data from EEA Vocabulary [https://dd.eionet.europa.eu/vocabulary/aq/pollutant]. To update the dataset, you have to change the direct link of downloading the csv file.

## **4. Stations**
It downloads detailed information for each SamplingPointId. It performs a spatial join to merge the spatial information of LAU and NUTS (specifically, the geometries of LAU and the geometry of stations) and fills in the missing data for CITY_NAME and CITY_ID [retrieved from https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.b2g.AirQualityStatistics] through a left join based on the AirQualityStationEoICode column.

    - **missing cities**:  
      This file is obtained manually (from 2000 to 2024) because the website did not allow downloading more than 100,000 rows at a time. The data was collected in multiple batches, filtering SamplingPoints using the following criteria:
        - Filter on data used in AQ Report: Yes  
        - Filter on data coverage: Yes  

      For each station:
      - The column `AirQualityStationEoICode` (identical for all sensors at the same station) was used to select the first row containing unique values for `CITY_NAME` and `CITY_ID`.
      - No station reported more than one value for this pair of columns.
      - To support future uploads, it is necessary to integrate updated `AirQualityStationEoICode` values.

