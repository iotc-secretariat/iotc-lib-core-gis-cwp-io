#' Retrieves all details (code, exact center latitude / longitude of its ocean area, regular grid center latitude / longitude, ocean area surface in KM2 and type of fishing ground)
#' for a subset of the regular grids available in the \code{CL_FISHING_GROUNDS} table of the \code{IOTCStatistics} database
#'
#' @param grid_types A vector containing the grid types for the grids to be returned
#' @return A data frame with the following columns
#' \code{FISHING_GROUND_CODE},
#' \code{LAT_X},
#' \code{LON_X},
#' \code{LAT},
#' \code{LON},
#' \code{OCEAN_AREA_SURFACE_KM2},
#' \code{FISHING_GROUND_TYPE_ID},
#' \code{FISHING_GROUND_TYPE}
#' and containing a row for each of the fishing ground code with the a fishing ground code type ID among the provided \code{grid_types}
#' @examples
#' filter_grids(grid_types = c(1, 2))
#' @export
filter_grids = function(grid_types = grid_codes_ALL) {
  return(
    GRIDS_ALL[FISHING_GROUND_TYPE_ID %in% grid_types]
  )
}

#' Same as \code{filter_grids(c = grid_code_1x1)}
#' @export
cwp_grids_1x1 = function() {
  return(
    filter_grids(grid_code_1x1)
  )
}

#' Same as \code{filter_grids(c = grid_code_5x5)}
#' @export
cwp_grids_5x5 = function() {
  return(
    filter_grids(grid_code_5x5)
  )
}

#' Same as \code{filter_grids(c = grid_code_10x10)}
#' @export
cwp_grids_10x10 = function() {
  return(
    filter_grids(grid_code_10x10)
  )
}

#' Same as \code{filter_grids(c = grid_code_10x20)}
#' @export
cwp_grids_10x20 =function() {
  return(
    filter_grids(grid_code_10x20)
  )
}

#' Same as \code{filter_grids(c = grid_code_20x20)}
#' @export
cwp_grids_20x20 =function() {
  return(
    filter_grids(grid_code_20x20)
  )
}

#' Same as \code{filter_grids(c = grid_code_30x30)}
#' @export
cwp_grids_30x30 = function() {
  return(
    filter_grids(grid_code_30x30)
  )
}

#' Provides the details of (potential) intersections between two sets of grid codes (source and target).
#' The result contains also a column that indicates the fraction (0..1) of each source grid that intersects
#' each corresponding target grid
#'
#' @param source_grid_codes a sequence of grid codes for the source grids
#' @param target_grid_codes a sequence of grid codes for the target grids
#' @return the details of (potential) intersections between two sets of grid codes (source and target)
#' @export
grid_intersections = function(source_grid_codes, target_grid_codes) {
  return(
    ALL_GRIDS_INTERSECTIONS[SOURCE_FISHING_GROUND_CODE %in% source_grid_codes &
                            TARGET_FISHING_GROUND_CODE %in% target_grid_codes]
  )
}

#' Provides the details of (potential) intersections between two sets of grid codes (source and target).
#' The result contains also a column that indicates the fraction (0..1) of each source grid that intersects
#' each corresponding target grid
#'
#' @param source_grid_type_code one of the valid grid type codes (among { \code{grid_code_1x1}, \code{grid_code_5x5}, \code{grid_code_10x10},
#' \code{grid_code_10x20}, \code{grid_code_20x20}, \code{grid_code_30x30}, \code{grid_code_irregular} })
#' @param target_grid_codes a sequence of grid codes for the target grids
#' @return the details of (potential) intersections between two sets of grid codes (source and target)
#' @export
grid_intersections_by_source_grid_type = function(source_grid_type_code = grid_code_5x5, target_grid_codes) {
  intersections =
    ALL_GRIDS_INTERSECTIONS[SOURCE_FISHING_GROUND_TYPE_ID == source_grid_type_code &
                            TARGET_FISHING_GROUND_CODE %in% target_grid_codes]

  grids_in      = data.table(CODE = unique(target_grid_codes))
  grids_missing = grids_in[!CODE %in% intersections$TARGET_FISHING_GROUND_CODE]$CODE

  if(length(grids_missing) > 0)
    intersections =
      rbind(intersections,
            data.table(
              SOURCE_FISHING_GROUND_CODE = NA,
              SOURCE_FISHING_GROUND_TYPE_ID = source_grid_type_code,
              TARGET_FISHING_GROUND_CODE = grids_missing,
              TARGET_FISHING_GROUND_TYPE_ID = NA,
              PROPORTION = NA
              )
      )

  return(intersections)
}

#' Provides the details of (potential) intersections between two sets of grid codes (source and target).
#' The result contains also a column that indicates the fraction (0..1) of each source grid that intersects
#' each corresponding target grid
#'
#' @param source_grid_codes a sequence of grid codes for the target grids
#' @param target_grid_type_code one of the valid grid type codes (among { \code{grid_code_1x1}, \code{grid_code_5x5}, \code{grid_code_10x10},
#' \code{grid_code_10x20}, \code{grid_code_20x20}, \code{grid_code_30x30}, \code{grid_code_irregular} })
#' @return the details of (potential) intersections between two sets of grid codes (source and target)
#' @export
grid_intersections_by_target_grid_type = function(source_grid_codes, target_grid_type_code = grid_code_5x5) {
  intersections =
    ALL_GRIDS_INTERSECTIONS[SOURCE_FISHING_GROUND_CODE %in% source_grid_codes &
                            TARGET_FISHING_GROUND_TYPE_ID == target_grid_type_code]

  grids_in      = data.table(CODE = unique(source_grid_codes))
  grids_missing = grids_in[!CODE %in% intersections$SOURCE_FISHING_GROUND_CODE]$CODE

  if(length(grids_missing) > 0)
    intersections =
    rbind(intersections,
          data.table(
            SOURCE_FISHING_GROUND_CODE = grids_missing,
            SOURCE_FISHING_GROUND_TYPE_ID = NA,
            TARGET_FISHING_GROUND_CODE = NA,
            TARGET_FISHING_GROUND_TYPE_ID = target_grid_type_code,
            PROPORTION = NA
          )
    )

  return(intersections)
}

#' Provides the details of (potential) intersections between two sets of grid codes (source and target).
#' The result contains also a column that indicates the fraction (0..1) of each source grid that intersects
#' each corresponding target grid
#'
#' @param source_grid_type_code one of the valid grid type codes (among { \code{grid_code_1x1}, \code{grid_code_5x5}, \code{grid_code_10x10},
#' \code{grid_code_10x20}, \code{grid_code_20x20}, \code{grid_code_30x30}, \code{grid_code_irregular} })
#' @param target_grid_type_code one of the valid grid type codes (among { \code{grid_code_1x1}, \code{grid_code_5x5}, \code{grid_code_10x10},
#' \code{grid_code_10x20}, \code{grid_code_20x20}, \code{grid_code_30x30}, \code{grid_code_irregular} })
#' @return the details of (potential) intersections between two sets of grid codes (source and target)
#' @export
grid_intersections_by_grid_types = function(source_grid_type_code = grid_code_5x5, target_grid_type_code = grid_code_10x10) {
  return(
    ALL_GRIDS_INTERSECTIONS[SOURCE_FISHING_GROUND_TYPE_ID == source_grid_type_code &
                            TARGET_FISHING_GROUND_TYPE_ID == target_grid_type_code]
  )
}
