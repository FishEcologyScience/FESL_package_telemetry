#' Example Acoustic Telemetry Detection Data from Hamilton Harbour
#'
#' @description A subsampled dataset containing the first 100,000 acoustic
#' telemetry detections from Hamilton Harbour. This data is used for
#' demonstration and testing of the network analysis and detection processing
#' functions. Data follows GLATOS format conventions.
#'
#' @format A dataframe with 100,000 detection records including:
#' \describe{
#'   \item{animal_id}{Fish identification code}
#'   \item{station_no}{Receiver station identifier}
#'   \item{detection_timestamp_est}{Detection timestamp (EST)}
#'   \item{deploy_lat}{Latitude of receiver deployment}
#'   \item{deploy_long}{Longitude of receiver deployment}
#'   \item{date}{Detection date}
#'   \item{transmitter_codespace}{Transmitter manufacturer code}
#'   \item{transmitter_id}{Transmitter identification number}
#'   \item{length}{Fish length (mm)}
#'   \item{...}{Additional detection metadata columns}
#' }
#'
#' @source Hamilton Harbour acoustic telemetry study, Fish Ecology Science Lab (FESL)
"example_data_raw_dets"



#' Hamilton Harbour Shoreline Shapefile (WGS84)
#'
#' @description An sf spatial object containing the shoreline boundary of
#' Hamilton Harbour in WGS84 coordinate reference system (EPSG:4326). Used for
#' geographic context in network plots and spatial visualizations.
#'
#' @format An sf object with MULTIPOLYGON geometry representing the
#' Hamilton Harbour shoreline boundary.
#'
#' @source Hamilton Harbour GIS data, Fish Ecology Science Lab (FESL)
"shapefile_HH_WGS84"



#' Hamilton Harbour Base Map
#'
#' @description A ggmap raster object containing a base map of Hamilton Harbour
#' for use as a background in spatial visualizations and plots.
#'
#' @format A ggmap raster object
#'
#' @source Hamilton Harbour mapping data, Fish Ecology Science Lab (FESL)
"basemap_HH"
