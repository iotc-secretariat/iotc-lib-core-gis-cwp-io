library(iotc.base.common.data)

grids_by_type           = function(grid_types, connection = DB_IOTCSTATISTICS()) {
  return(
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
          FG.CL_FISHING_GROUND_TYPE_ID IN (", paste(grid_types, collapse=", "), ")
        ORDER BY
          FG.CODE ASC;
      ")
    )
  )
}
all_grid_intersections  = function(connection = DB_IOTCSTATISTICS()) {
  return(
    query(
      connection = connection,
      paste0("
        SELECT
          FG.CODE AS SOURCE_FISHING_GROUND_CODE,
          FG.CL_FISHING_GROUND_TYPE_ID AS SOURCE_FISHING_GROUND_TYPE_ID,
          AG.CODE AS TARGET_FISHING_GROUND_CODE,
          AG.CL_FISHING_GROUND_TYPE_ID AS TARGET_FISHING_GROUND_TYPE_ID,
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
       ORDER BY
          FG.CODE ASC,
          AG.CODE ASC;
      ")
    )
  )
}

GRIDS_ALL               = grids_by_type(grid_codes_ALL)
ALL_GRIDS_INTERSECTIONS = all_grid_intersections()

usethis::use_data(GRIDS_ALL, overwrite = TRUE)
usethis::use_data(ALL_GRIDS_INTERSECTIONS, overwrite = TRUE)
