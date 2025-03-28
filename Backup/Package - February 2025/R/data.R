#' Dataset containing information about the available pollutants
#'
#' This dataset belongs to the European Environment Agency institution. Contains short information about the available
#' pollutants.
#'
#' @format A data frame with 658 rows and 4 variables:
#' \describe{
#'   \item{Code}{Unique pollutant code identifier}
#'   \item{Notation}{pollutant's short name}
#'   \item{Label}{pollutant's full name}
#'   \item{RecommendedUnit}{Measurement unit, recommended for the pollutant}
#' }
#' @source <http://dd.eionet.europa.eu/vocabulary/aq/pollutant/view>






#' Dataset containing information about the available Territorial units for statistics (NUTS)
#' @format A data frame with 1793 rows and 9 variables:
#' \describe{
#' \item{NUTS_ID}{NUTS ID code, consisting of country code and numbers (1 for NUTS 1, 2 for NUTS 2 and 3 for NUTS 3)}
#' \item{LEVL_CODE}{NUTS level code: 0 (national level), 1 (major socio-economic regions), 2 (basic regions for the application of regional policies) or 3 (small regions)}
#' \item{CNTR_CODE}{Country code: two-letter ISO code (ISO 3166 alpha-2)}
#' \item{NAME_LATN}{NUTS name in local language, transliterated to Latin script}
#' \item{NUTS_NAME}{NUTS name in local language, in local script}
#' \item{MOUNT_TYPE}{Mountain typology for NUTS  regions}
#' \item{URBN_TYPE}{Urban-rural typology for NUTS  regions}
#' \item{COAST_TYPE}{Coastal typology for NUTS  regions}
#' \item{geometry}{geospatial information}
#' }
#' @source <https://ec.europa.eu/eurostat/web/gisco/geodata/statistical-units/territorial-units-statistics>



#' Dataset containing information about European administrative boundaries down to commune level version
#' @format A data frame with 1793 rows and 9 variables:
#' \describe{
#' \item{GISCO_ID}{unique identifier consisting of the Country code and LAU code}
#' \item{ISO}{Country code: two-letter ISO code (ISO 3166 alpha-2)}
#' \item{LAU_ID}{LAU system identification code of the area at LAU level in which the measuring station is located}
#' \item{LAU_NAME}{Name in Latin characters of the area at the level LAU where the measuring station is located}
#' \item{POP_2021}{Total resident population for the reference year}
#' \item{POP_DENS_2}{Population density}
#' \item{AREA_KM2}{area of the polygons in LAEA spatial reference (EPSG:3035) of the original 100K scale data}
#' \item{YEAR}{the reference year for the population value}
#' \item{NUTS3_ID}{NUTS system identification code of the area at NUTS 3 level in which the measuring station is located}
#' \item{Lau_geometry}{geospatial information}
#' }
#' @source <https://ec.europa.eu/eurostat/web/nuts/local-administrative-units>



#' Air quality measurement stations information.
#'
#' This dataset belongs to the European Environment Agency. Assessment methods meta-data (data set D) describe technical facilities used for the measurement
#' of one pollutant or one of its compounds. It contains information about the measurement stations mapped by the EEA
#' in Europe. This dataset may be out of date, and for this reason the use of the \code{EEAaq_get_stations} function
#' is suggested.
#' For further information see <https://cmshare.eea.europa.eu/s/8LGQLRGX8YEiSg9/download>
#'
#' @format A data frame with 68859 rows, 80 variables and a geometry representing the station's location point:
#' \describe{
#'   \item{Country}{Country or territory name}
#'   \item{ISO}{ISO 3166-1 alpha-2 code, representing the country. It's possible to refer to this variable
#'   as the NUTS 0 level}
#'   \item{SamplingPointId}{Inspire identifier (Local Id) of sampling point, given by data provider}
#'   \item{AirQualityStationEoICode}{EoI code of air quality measurement station (used in AirBase), given by data provider}
#'   \item{AirQualityStationNatCode}{National code of air quality measurement station, given by data provider}
#'   \item{AirQualityStationName}{Name of air quality measurement station (as in AirBase), given by data provider}
#'   \item{AirPollutant}{Air polluting substance, level of which is measured and reported to the EEA.
#'   See \code{pollutants} for further information}
#'   \item{OperationalActivityBegin}{Start time of the sampling point}
#'   \item{OperationalActivityEnd}{End time of the sampling point}
#'   \item{SamplingPointStatus}{Categorical variable which assumes two possible values:
#'   \itemize{
#'   \item{\emph{active}: if the sampling point is still active (\code{OperationalActivityEnd} = \code{NA})}
#'   \item{\emph{closed}: if the sampling point activity is ceased}
#'   }}
#'   \item{Longitude}{Longitude of air quality measurement station,
#'   according to the geographical coordinate system WGS84 (decimal degrees)}
#'   \item{Latitude}{Latitude of air quality measurement station,
#'   according to the geographical coordinate system WGS84 (decimal degrees)}
#'   \item{Altitude}{Altitude of air quality measurement station (m.a.s.l.)}
#'   \item{NUTS1}{Name in Latin characters of the area at the level NUTS 1 where the measuring station is located}
#'   \item{NUTS1_ID}{NUTS system identification code of the area at NUTS 1 level in which the measuring station is located}
#'   \item{NUTS2}{Name in Latin characters of the area at the level NUTS 2 where the measuring station is located}
#'   \item{NUTS2_ID}{NUTS system identification code of the area at NUTS 2 level in which the measuring station is located}
#'   \item{NUTS3}{Name in Latin characters of the area at the level NUTS 3 where the measuring station is located}
#'   \item{NUTS3_ID}{NUTS system identification code of the area at NUTS 3 level in which the measuring station is located}
#'   \item{NUTS0}{Name in Latin characters of the area at the level NUTS 0 where the measuring station is located}
#'   \item{NUTS0_ID}{NUTS system identification code of the area at NUTS 0 level in which the measuring station is located}
#'   \item{LAU_NAME}{Name in Latin characters of the area at the level LAU where the measuring station is located}
#'   \item{LAU_ID}{LAU system identification code of the area at LAU level in which the measuring station is located}
#'   \item{AirQualityStationArea}{Area of Air Quality Measurement Station classification -
#'   information whether it is measuring air pollution in urban, suburban, rural (etc.) environment}
#'   \item{AirQualityStationType}{Type of Air Quality Measurement Station -
#'   information whether it is measuring background,industrial or traffic related air pollution}
#'   \item{B-GNamespace}{Inspire identifier/namespace of reporting entity, given by data provider}
#'   \item{Year}{Latest year for which the data flow item has been reported}
#'   \item{AirQualityNetwork}{Inspire identifier (Local Id) of air quality network, given by the data provider}
#'   \item{AirQualityNetworkName}{Name of air quality measurement network, given by the data provider}
#'   \item{Timezone}{Time zone in which aggregations and statistics are calculated}
#'   \item{AltitudeUnit}{Unit of measurement of the altitude of the station}
#'   \item{SampleId}{Inspire identifier (Local Id) of sample (Feature of Interest), given by data provider}
#'   \item{InletHeight}{Height of the sampling point inlet}
#'   \item{InletHeightUnit}{Unit of measurement of the height of the sampling point inlet}
#'   \item{BuildingDistance}{The horizontal distance of the inlet to the nearest building}
#'   \item{BuildingDistanceUnit}{Unit of measurement of the distance of the inlet to the nearest building}
#'   \item{KerbDistance}{The horizontal distance of the inlet to the nearest kerb}
#'   \item{KerbDistanceUnit}{Unit of measurement of the distance of the inlet to the nearest kerb}
#'   \item{DistanceSource}{The distance from predominant industrial source or source area}
#'   \item{DistanceSourceUnit}{Unit of measurement of the distance from predominant industrial source or source area}
#'   \item{MainEmissionSources}{The main emission source for the pollutant}
#'   \item{HeatingEmissions}{Amount of emissions from domestic heating for a representative area of approximately \eqn{1 km^{2}}}
#'   \item{HeatingEmissionsUnit}{Unit of measurement of the heating emissions}
#'   \item{Mobile}{Mobile station qualifier fixed (0) or mobile (1)}
#'   \item{TrafficEmissions}{Amount of emissions from road traffic for a section of road representative of at least 100 m}
#'   \item{TrafficEmissionsUnit}{Unit of measurement of the traffic emissions}
#'   \item{IndustrialEmissions}{Amount of emissions from industry for a representative area of approximately \eqn{1 km^{2}}}
#'   \item{IndustrialEmissionsUnit}{Unit of measurement of the industrial emissions}
#'   \item{Municipality}{The name of the municipality in which the monitoring station is located}
#'   \item{DispersionLocal}{The location of the station in relation to nearby buildings & trees using a controlled vocabulary}
#'   \item{DispersionRegional}{The regional dispersion characteristics or topographic situation
#'   on a scale of several kilometres affecting the station from a controlled vocabulary}
#'   \item{DistanceJunction}{Distance of the station from a major junction}
#'   \item{DistanceJunctionUnit}{Unit of measurement of the distance of the station from a major junction}
#'   \item{HeavyDutyFraction}{The fraction of the total traffic volume (assessed as AADT) that is composed of HGVs on the adjacent road}
#'   \item{HeightFacades}{The average height of the building facades adjacent to the station (in meters) at the location of the station}
#'   \item{StreetWidth}{The width of the street (in meters) at the location of the station}
#'   \item{TrafficSpeed}{The average speed of vehicles in km/h on the adjacent road}
#'   \item{TrafficVolume}{The total traffic volume (as an annual average daily traffic) on the adjacent road}
#'   \item{ProcessId}{Inspire identifier (Local Id) of sampling process (procedure), given by data provider}
#'   \item{ProcessActivityBegin}{Start time of the measurement process}
#'   \item{ProcessActivityEnd}{End time of the measurement process}
#'   \item{MeasurementType}{The classification (grouping) of measurement methods into generic types.
#'   The types of measurements include: Automatic analyser, Remote sensor, Active sampling and Passive sampling}
#'   \item{MeasurementMethod}{Information on method used for measuring air polluting substances}
#'   \item{OtherMeasurementMethod}{Other Measurement Method}
#'   \item{MeasurementEquipment}{Information on equipment used for measuring air polluting substances}
#'   \item{OtherMeasurementEquipment}{Other Measurement Equipment}
#'   \item{SamplingMethod}{Information on the sampling methods used for Active or passive sampling measurement types (i.e.Passive adsorbent, Low Volume Sampling withautomatic filter change)}
#'   \item{OtherSamplingMethod}{Other Sampling Method}
#'   \item{AnalyticalTechnique}{Information on analytical technique}
#'   \item{OtherAnalyticalTechnique}{Other Analytical Technique}
#'   \item{EquivalenceDemonstrated}{Specifies the equivalence status of the measuring/sampling
#'    process according to Annex VI.B of Dir. 2008/50EC and Annex V of Dir. 2004/107EC}
#'   \item{DemonstrationReport}{Link to the equivalence demonstration report}
#'   \item{DetectionLimit}{The measuring/sampling process including detection limit}
#'   \item{DetectionLimitUnit}{Unit of measurement of the detection limit}
#'   \item{Documentation}{Title of documentation on data quality}
#'   \item{QAReport}{Link to report with Quality Assurance information}
#'   \item{Duration}{The expected sampling duration of the measurement or sampling method}
#'   \item{DurationUnit}{Unit of measurement of the duration}
#'   \item{Cadence}{The time interval between the start of two consecutive measurements or samples}
#'   \item{CadenceUnit}{Unit of measurement of the cadence}
#'   \item{SourceDataURL}{URL of source data reported to the EEA}
#'   \item{Imported}{Date and time of source data import into EEA's databases}
#' }
#' @source <https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.b2g.Measurements>

