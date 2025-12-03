#' Raw Acoustic Telemetry Detection Data
#'
#' @description A dataset containing raw acoustic telemetry detections from
#' Hamilton Harbour. This data is used for demonstration and testing of the
#' network analysis functions.
#'
#' @format A dataframe with detection records including:
#' \describe{
#'   \item{animal_id}{Fish identification code}
#'   \item{station_no}{Receiver station identifier}
#'   \item{deploy_lat}{Latitude of receiver deployment}
#'   \item{deploy_long}{Longitude of receiver deployment}
#'   \item{...}{Additional detection metadata columns}
#' }
#'
#' @source Hamilton Harbour acoustic telemetry study
"data_det_raw"

#' Hamilton Harbour Shapefile (WGS84)
#'
#' @description An sf spatial object containing the shoreline boundary of
#' Hamilton Harbour in WGS84 coordinate reference system. Used for geographic
#' context in network plots.
#'
#' @format An sf object with polygon geometry
#'
#' @source Hamilton Harbour GIS data
"HH_shapefile_WGS84"

#' Hamilton Harbour Full Map
#'
#' @description A ggmap object containing a full base map of Hamilton Harbour
#' for visualization purposes.
#'
#' @format A ggmap object
#'
#' @source Hamilton Harbour mapping data
"HHmapFull"
