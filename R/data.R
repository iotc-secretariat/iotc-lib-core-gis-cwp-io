#' All grids
#'
#' @format
#' \describe{
#'   \item{FISHING_GROUND_CODE}{The alphanumeric grid code}
#'   \item{LAT_X}{The latitude of the centroid for the ocean area within the grid}
#'   \item{LON_X}{The longitude of the centroid for the ocean area within the grid}
#'   \item{LAT}{The latitude of the centroid corresponding to the theoretical grid surface (land + ocean)
#'   \item{LON}{The longitude of the centroid corresponding to the theoretical grid surface (land + ocean)
#'   \item{OCEAN_AREA_SURFACE_KM2 }{The area (in km2) of the ocean part of the grid}
#'   \item{FISHING_GROUND_TYPE_ID }{The ID of the type of fishing ground for the grid}
#'   \item{FISHING_GROUND_TYPE }{The type of fishing ground for the grid}
#' }
"GRIDS_ALL"

#' All grid intersections
#' @format
#' \describe{
#'    \item{SOURCE_FISHING_GROUND_CODE}{The alphanumeric (source) grid code}
#'    \item{SOURCE_FISHING_GROUND_TYPE_ID}{The ID of the type of fishing ground for the source grid}
#'    \item{TARGET_FISHING_GROUND_CODE}{The alphanumeric (target) grid code}
#'    \item{TARGET_FISHING_GROUND_TYPE_ID}{The ID of the type of fishing ground for the target grid}
#'    \item{PROPORTION}{The proportion of ocean area from the source grid that overlaps with the target grid}
#' }
"ALL_GRIDS_INTERSECTIONS"
