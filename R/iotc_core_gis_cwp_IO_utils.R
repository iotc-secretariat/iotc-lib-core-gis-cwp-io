CWP_CACHE = new.env(hash = TRUE)

#' Clears the CWP data cache
#' @export
clear_cwp_cache = function() {
  clear_cache(CWP_CACHE)
}

#' Retrieves all details (code, exact center latitude / longitude of its ocean area, regular grid center latitude / longitude, ocean area surface in KM2 and type of fishing ground)
#' for a subset of the regular grids available in the \code{CL_FISHING_GROUNDS} table of the \code{IOTCStatistics} database
#'
#' @param grid_types A vector containing the grid types for the grids to be returned
#' @param connection An ODBC connection to a RDBMS server hosting the \code{IOTCStatistics} database
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
#' filter_grids(grid_types = 2, connection = DB_IOTCSTATISTICS(server = "localhost"))
#' @export
filter_grids = function(grid_types = grid_codes_ALL, connection = DB_IOTCSTATISTICS()) {
  grid_types_filter = paste(grid_types, collapse=", ")

  key = paste0(dbGetInfo(connection)$servername, "|", cache_key_root())
  key = paste(key, paste(grid_types, collapse = "|"))

  return(
    cache_get_or_set(
      CWP_CACHE,
      key,
      {
        query(
          connection = connection,
          paste0("
            SELECT
              FG.CODE AS FISHING_GROUND_CODE,
              SIGN(FG.CENTER_LAT) * ROUND(ABS(FG.CENTER_LAT) * 1000, 0) * 0.001 AS LAT_X,
              ROUND(FG.CENTER_LON * 1000, 0) * 0.001 AS LON_X,
              [dbo].[GIS_CWP_GRID_CENTER_LAT](FG.CODE) AS LAT,
              [dbo].[GIS_CWP_GRID_CENTER_LON](FG.CODE) AS LON,
              FG.AREA_FRACTION_IO AS OCEAN_AREA_SURFACE_KM2,
              FG.CL_FISHING_GROUND_TYPE_ID AS FISHING_GROUND_TYPE_ID,
              FGT.CODE AS FISHING_GROUND_TYPE
            FROM
              dbo.CL_FISHING_GROUNDS FG
            INNER JOIN
              dbo.CL_FISHING_GROUND_TYPES FGT
            ON
              FG.CL_FISHING_GROUND_TYPE_ID = FGT.ID
            WHERE
              FG.CL_FISHING_GROUND_TYPE_ID IN (", grid_types_filter, ")
            ORDER BY
              FG.CODE ASC;
          ")
        )
      }
    )
  )
}

#' Same as \code{filter_grids(c = grid_code_1x1, connection = <connection>)}
#' @export
cwp_grids_1x1 = function(connection = DB_IOTCSTATISTICS()) {
  return (filter_grids(grid_code_1x1, connection))
}

#' Same as \code{filter_grids(c = grid_code_5x5, connection = <connection>)}
#' @export
cwp_grids_5x5 = function(connection = DB_IOTCSTATISTICS()) {
  return (filter_grids(grid_code_5x5, connection))
}

#' Same as \code{filter_grids(c = grid_code_10x10, connection = <connection>)}
#' @export
cwp_grids_10x10 = function(connection = DB_IOTCSTATISTICS()) {
  return (filter_grids(grid_code_10x10, connection))
}

#' Same as \code{filter_grids(c = grid_code_10x20, connection = <connection>)}
#' @export
cwp_grids_10x20 =function(connection = DB_IOTCSTATISTICS()) {
  return (filter_grids(grid_code_10x20, connection))
}

#' Same as \code{filter_grids(c = grid_code_20x20, connection = <connection>)}
#' @export
cwp_grids_20x20 =function(connection = DB_IOTCSTATISTICS()) {
  return (filter_grids(grid_code_20x20, connection))
}

#' Same as \code{filter_grids(c = grid_code_30x30, connection = <connection>)}
#' @export
cwp_grids_30x30 = function(connection = DB_IOTCSTATISTICS()) {
  return (filter_grids(grid_code_30x30, connection))
}

#' Provides the details of (potential) intersections between two sets of grid codes (source and target).
#' The result contains also a column that indicates the fraction (0..1) of each source grid that intersects
#' each corresponding target grid
#'
#' @param source_grid_codes a sequence of grid codes for the source grids
#' @param target_grid_codes a sequence of grid codes for the target grids
#' @return the details of (potential) intersections between two sets of grid codes (source and target)
#' @export
grid_intersections = function(source_grid_codes, target_grid_codes, connection = DB_IOTCSTATISTICS()) {
  source_grids_filter = iotc.core.utils.misc::join_strings(source_grid_codes)
  target_grids_filter = iotc.core.utils.misc::join_strings(target_grid_codes)

  key = paste0(dbGetInfo(connection)$servername, "|", cache_key_root())
  key = paste(key, paste(source_grid_codes, collapse = "|"))
  key = paste(key, paste(target_grid_codes, collapse = "|"))

  return(
    cache_get_or_set(
      CWP_CACHE,
      key,
      {
        query(
          connection = connection,
          paste0("
            SELECT
              FG.CODE AS SOURCE_FISHING_GROUND_CODE,
              AG.CODE AS TARGET_FISHING_GROUND_CODE,
              FGA.REVERSE_GRID_AREA_PROPORTION AS PROPORTION
            FROM
              dbo.CL_FISHING_GROUNDS FG
            LEFT JOIN
              dbo.CL_FISHING_GROUND_AGGREGATIONS FGA
            ON
              FG.ID = FGA.CL_REFERENCE_ID
            INNER JOIN
              dbo.CL_FISHING_GROUNDS AG
            ON
              AG.ID = FGA.CL_AGGREGATION_ID
            WHERE
              FG.CODE IN (", source_grids_filter, ") AND
              AG.CODE IN (", target_grids_filter, ")
            ORDER BY
              FG.CODE ASC,
              AG.CODE ASC;
          ")
        )
      }
    )
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
grid_intersections_by_source_grid_type = function(source_grid_type_code = grid_code_5x5, target_grid_codes, connection = DB_IOTCSTATISTICS()) {
  target_grids_filter = iotc.core.utils.misc::join_strings(target_grid_codes)

  key = paste0(dbGetInfo(connection)$servername, "|", cache_key_root())
  key = paste(key, paste(source_grid_type_code, collapse = "|"))
  key = paste(key, paste(target_grid_codes, collapse = "|"))

  data =
    cache_get_or_set(
      CWP_CACHE,
      key,
      {
        query(
          connection = connection,
          paste0("
            SELECT
              FG.CODE AS SOURCE_FISHING_GROUND_CODE,
              AG.CODE AS TARGET_FISHING_GROUND_CODE,
              FGA.REVERSE_GRID_AREA_PROPORTION AS PROPORTION
            FROM
              dbo.CL_FISHING_GROUNDS FG
            LEFT JOIN
              dbo.CL_FISHING_GROUND_AGGREGATIONS FGA
            ON
              FG.ID = FGA.CL_REFERENCE_ID
            INNER JOIN
              dbo.CL_FISHING_GROUNDS AG
            ON
              AG.ID = FGA.CL_AGGREGATION_ID
            WHERE
              FG.CL_FISHING_GROUND_TYPE_ID = ", source_grid_type_code, " AND
              AG.CODE IN (", target_grids_filter, ")
            ORDER BY
              FG.CODE ASC,
              AG.CODE ASC;
          ")
        )
      }
    )

  grids_in      = data.table(CODE = unique(target_grid_codes))
  grids_missing = grids_in[!CODE %in% data$TARGET_FISHING_GROUND_CODE]$CODE

  if(length(grids_missing) > 0)
    data = rbind(data, data.table(TARGET_FISHING_GROUND_CODE = grids_missing, SOURCE_FISHING_GROUND_CODE = NA, PROPORTION = NA))

  return(data)
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
grid_intersections_by_target_grid_type = function(source_grid_codes, target_grid_type_code = grid_code_5x5, connection = DB_IOTCSTATISTICS()) {
  source_grids_filter = iotc.core.utils.misc::join_strings(source_grid_codes)

  key = paste0(dbGetInfo(connection)$servername, "|", cache_key_root())
  key = paste(key, paste(source_grid_codes, collapse = "|"))
  key = paste(key, paste(target_grid_type_code, collapse = "|"))

  data =
    cache_get_or_set(
      CWP_CACHE,
      key,
      {
        query(
          connection = connection,
          paste0("
            SELECT
              FG.CODE AS SOURCE_FISHING_GROUND_CODE,
              AG.CODE AS TARGET_FISHING_GROUND_CODE,
              FGA.REVERSE_GRID_AREA_PROPORTION AS PROPORTION
            FROM
              dbo.CL_FISHING_GROUNDS FG
            LEFT JOIN
              dbo.CL_FISHING_GROUND_AGGREGATIONS FGA
            ON
              FG.ID = FGA.CL_REFERENCE_ID
            INNER JOIN
              dbo.CL_FISHING_GROUNDS AG
            ON
              AG.ID = FGA.CL_AGGREGATION_ID
            WHERE
              FG.CODE IN (", source_grids_filter, ") AND
              AG.CL_FISHING_GROUND_TYPE_ID = ", target_grid_type_code, "
            ORDER BY
              FG.CODE ASC,
              AG.CODE ASC;
          ")
        )
      }
    )

  grids_in      = data.table(CODE=unique(source_grid_codes))
  grids_missing = grids_in[!CODE %in% data$SOURCE_FISHING_GROUND_CODE]$CODE

  if(length(grids_missing) > 0)
    data = rbind(data, data.table(SOURCE_FISHING_GROUND_CODE = grids_missing, TARGET_FISHING_GROUND_CODE = NA, PROPORTION = NA))

  return(data)
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
grid_intersections_by_grid_types = function(source_grid_type_code = grid_code_5x5, target_grid_type_code = grid_code_10x10, connection = DB_IOTCSTATISTICS()) {
  key = paste0(dbGetInfo(connection)$servername, "|", cache_key_root())
  key = paste(key, paste(source_grid_type_code, collapse = "|"))
  key = paste(key, paste(target_grid_type_code, collapse = "|"))

  return(
    cache_get_or_set(
      CWP_CACHE,
      key,
      {
        query(
          connection = connection,
          paste0("
            SELECT
              FG.CODE AS SOURCE_FISHING_GROUND_CODE,
              AG.CODE AS TARGET_FISHING_GROUND_CODE,
              FGA.REVERSE_GRID_AREA_PROPORTION AS PROPORTION
            FROM
              dbo.CL_FISHING_GROUNDS FG
            LEFT JOIN
              dbo.CL_FISHING_GROUND_AGGREGATIONS FGA
            ON
              FG.ID = FGA.CL_REFERENCE_ID
            INNER JOIN
              dbo.CL_FISHING_GROUNDS AG
            ON
              AG.ID = FGA.CL_AGGREGATION_ID
            WHERE
              FG.CL_FISHING_GROUND_TYPE_ID = ", source_grid_type_code, " AND
              AG.CL_FISHING_GROUND_TYPE_ID = ", target_grid_type_code, "
            ORDER BY
              FG.CODE ASC,
              AG.CODE ASC;
          ")
        )
      }
    )
  )
}

#'Calculates the probability of all grids of a given type of being "coastal", i.e. having a fraction of their area on land
#'@param grid_type_code A grid type code
#'@param connection A connection to \code{\link{IOTCSTATISTICS}}
#'@return the identified grid code and the probability (1 - IO grid area / original grid area) of its being coastal
#'@export
grid_code_coastal_probability = function(grid_type_code = grid_code_1x1, connection = DB_IOTCSTATISTICS()) {
  return(
    cache_get_or_set(
      CWP_CACHE,
      cache_key_root(as.character(grid_type_code)),
      {
        query(
          connection = connection,
          paste0("
            SELECT
              CODE AS CWP_GRID_CODE,
              1 - ( EPSG_DATA.STArea() / EPSG_DATA_ORIGINAL.STArea() ) AS PROB_COASTAL
            FROM
              CL_FISHING_GROUNDS
            WHERE
              CL_FISHING_GROUND_TYPE_ID = ", grid_type_code, " AND
              EPSG_DATA.STArea() > 0 AND
            ( EPSG_DATA_ORIGINAL.STArea() / dbo.GREATEST(1, EPSG_DATA.STArea()) > 1.001 )
          ")
        )
      }
    )
  )
}
