#' Generates the track limit on the right and left side of the track
#'
#' Can save a GeoPackage and return a sf object of right and left side points
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digital Surface Model raster file as '.tif'.
#' @param export If 'TRUE' the GeoPackage will be exported to your wd.
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#'
#' @return
#' @export
#'
#' @examples
#' dsm <- read_dsm(system.file("tif/dsm_small.tif", package = "checkRtrack"))
#' tracks <- read_tracks(system.file("geopackage/tracks_d.gpkg", package = "checkRtrack"))
#' checkSides(dsm, tracks, export = FALSE, dist_cross = 1, profile_length = 1, dist_cross_points = 0.05)
#'
checkSides <- function(dsm, tracks, export = TRUE, dist_cross = 1, profile_length = 1, dist_cross_points = 0.05) {



  #adding tracks_id column to the tracks
  tracks$track_id <- seq.int(nrow(tracks))


  #first lets make the dsm a bit smaller

  tracks <- st_transform(tracks, crs=st_crs(dsm))

  bufferedtrack <- sf::st_buffer(tracks, profile_length, endCapStyle = "ROUND", joinStyle = "ROUND")




  # Clip dsm by buffer
  dsm_clipped <- mask(dsm, bufferedtrack)

  slope <- qgis_run_algorithm(
    algorithm = "native:slope",
    INPUT = dsm_clipped,
    Z_FACTOR = 1 #means no exaggeration
  )
  qgis_extract_output(slope)
  slope <- qgis_as_terra(slope)


  #points along geometry = PAG
  result <- qgis_run_algorithm(
    algorithm = "native:pointsalonglines",
    INPUT = tracks,
    DISTANCE = dist_cross #in meters e.g. 1
  )
  qgis_extract_output(result)
  pag <- sf::st_as_sf(result)




  #geometry by expression = GBE ## this creates vertical lines on each point of pag and its original path going through it
  #https://gis.stackexchange.com/questions/380361/creating-perpendicular-lines-on-line-using-qgis

  expression <- "extend(\r\n   make_line(\r\n      $geometry,\r\n       project (\r\
        \n          $geometry, \r\n          tobechanged, \r\n          radians(\"angle\"-90))\r\
        \n        ),\r\n   tobechanged,\r\n   0\r\n)"


  profilelengthhalf <- profile_length/2

  newexpression <- gsub('tobechanged', profilelengthhalf, expression)



  #trackRcheck::changelengthofprofile(profilelength)
  profiles <- qgis_run_algorithm(
    algorithm = "native:geometrybyexpression",
    INPUT = pag,
    EXPRESSION = newexpression,
    OUTPUT_GEOMETRY = 1
  )

  qgis_extract_output(profiles)
  gbe <- sf::st_as_sf(profiles)
  gbe[["line_id"]] <- 1:nrow(gbe)#defining line_ad as field
  gbe$line_id <- 1:nrow(gbe)#adding line_ad as field to gbe


  # SAGA Profile from lines = sagaPFL # NEW title: creating points on vertical lines


  sagaPFL <- qgis_run_algorithm(
    algorithm = "native:pointsalonglines",
    INPUT = gbe,
    DISTANCE = dist_cross_points #in meters
  )
  qgis_extract_output(sagaPFL)
  pfl <- sf::st_as_sf(sagaPFL)

  #buffer around points along vertical lines = BVL
  bufferedpoints <- sf::st_buffer(pfl, 0.001, endCapStyle = "ROUND", joinStyle = "ROUND")

  #join attributes by location

  joinedL <- st_join(bufferedpoints, gbe, left = T)#until here the package worked 25.03.2024

  # recreate center points of buffers to later add the DSM data

  centerpoints <- sf::st_centroid(joinedL)

  # adding the slope values to the points
  slopepoints <- terra::extract(slope,centerpoints)
  centerpoints$slope <- slopepoints[, -1]
  #adding dsm values for bringing it together later
  dsmpoints <- terra::extract(dsm,centerpoints)
  centerpoints$z <- dsmpoints[, -1]




  #removing the unnecessary line_id.y.x
  centerpoints <- centerpoints %>%
    dplyr::select(!ends_with("y"))

  centerpoints_newcol <- gsub("\\.x$", "", colnames(centerpoints))

  colnames(centerpoints) <- centerpoints_newcol


  #here i have to split the upper and down parts
  sidebuff_distance <- profile_length/2

  upbuff <- qgis_run_algorithm(
    algorithm = "native:singlesidedbuffer",
    INPUT = tracks,
    DISTANCE = sidebuff_distance ,
    SIDE = 1,
    SEGMENTS =8,
    JOIN_STYLE=0,
    MITER_LIMIT= 2
  )
  qgis_extract_output(upbuff)
  upbuff <- sf::st_as_sf(upbuff)

  downbuff <- qgis_run_algorithm(
    algorithm = "native:singlesidedbuffer",
    INPUT = tracks,
    DISTANCE = sidebuff_distance ,
    SIDE = 0,
    SEGMENTS =8,
    JOIN_STYLE=0,
    MITER_LIMIT= 2
  )

  qgis_extract_output(downbuff)
  downbuff <- sf::st_as_sf(downbuff)


  upperslope <- st_filter(centerpoints,upbuff)

  downerslope <- st_filter(centerpoints,downbuff)



  #categorial statistics

  upperstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = upperslope,
    VALUES_FIELD_NAME = "slope",
    CATEGORIES_FIELD_NAME = "line_id"

  )

  downerstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = downerslope,
    VALUES_FIELD_NAME = "slope",
    CATEGORIES_FIELD_NAME = "line_id"

  )

  su <- qgis_extract_output(upperstats)
  stats_up <- sf::st_as_sf(su)

  sd <- qgis_extract_output(downerstats)
  stats_down <- sf::st_as_sf(sd)




  #join attributs from points layer with dsm info by line_id and min(z)
  slope_up_stats <- dplyr::left_join(upperslope, stats_up, by = "line_id")
  slope_down_stats <- dplyr::left_join(downerslope, stats_down, by = "line_id")



  #select objects where slope value is the same as max value (so we only have the max slope object of the profiles)
  selected_up <- slope_up_stats[slope_up_stats$slope == slope_up_stats$max,]
  selected_down <- slope_down_stats[slope_down_stats$slope == slope_down_stats$max,]



    selected_up$Pointtype <- "Left"
    selected_down$Pointtype <- "Right"

    joined_points <- dplyr::bind_rows(selected_up,selected_down)
    joined_points <- na.omit(joined_points)



  if(isTRUE(export)) {

    st_write(joined_points, "right_left_points.gpkg", driver = "GPKG")


  }

return(joined_points)

}

