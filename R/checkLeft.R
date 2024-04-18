#' Generates the track limit on the left side of the track
#'
#' Can save a GeoPackage and return a sf object of left side points
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digital Surface Model raster file as '.tif'.
#' @param export If 'TRUE' the GeoPackage will be exported to your wd.
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#'
#' @return Points along the left side of the track representing the outer left
#'         extent of the track
#' @export
#'

#' @examples
#' dsm <- read_dsm(system.file("tif/dsm.tif", package = "checkRtrack"))
#' tracks <- read_tracks(system.file("geopackage/tracks.gpkg", package = "checkRtrack"))
#' left <- checkLeft(dsm, tracks, export = FALSE, dist_cross = 1, profile_length = 1,
#'          dist_cross_points = 0.05)
#' str(left)
#' checkMap(dsm, tracks, points = left)
#'# run checkMap() as many times as you like to get different examples

checkLeft <- function(dsm, tracks, export = TRUE, dist_cross = 1,
                      profile_length = 1, dist_cross_points = 0.05) {


  # adding tracks_id column to maintain identification of the tracks
  tracks$track_id <- seq.int(nrow(tracks))

  # making sure, that the dsm and the tracks have the same crs
  tracks <- st_transform(tracks, crs=st_crs(dsm))

  # lets make the dsm a bit smaller for faster computation
  bufferedtrack <- sf::st_buffer(tracks, profile_length, endCapStyle = "ROUND",
                                 joinStyle = "ROUND")


  # Clip dsm by buffer
  dsm_clipped <- mask(dsm, bufferedtrack)

  #calculate the slope of the dsm
  slope <- qgis_run_algorithm(
    algorithm = "native:slope",
    INPUT = dsm_clipped,
    Z_FACTOR = 1                                                                # 1 means no exaggeration
  )
  qgis_extract_output(slope)                                                    # needed to make output readable
  slope <- qgis_as_terra(slope)                                                 # slope result



  # Creating points along the track (pag = points algong geometry)
  result <- qgis_run_algorithm(
    algorithm = "native:pointsalonglines",
    INPUT = tracks,
    DISTANCE = dist_cross
  )
  qgis_extract_output(result)                                                   # needed to make output readable
  pag <- sf::st_as_sf(result)                                                   # the points along the track




  # creating an expression to create vertical lines on each point of 'pag' and its original track going through it
  # (gbe = geometry by expression)
  expression <- "extend(\r\n   make_line(\r\n      $geometry,\r\n       project (\r\
        \n          $geometry, \r\n          tobechanged, \r\n          radians(\"angle\"-90))\r\
        \n        ),\r\n   tobechanged,\r\n   0\r\n)"

  # user will input full length of crossprofiles
  profilelengthhalf <- profile_length/2

  # creating the adapted expression with new profile length
  newexpression <- gsub('tobechanged', profilelengthhalf, expression)



  # now the profiles are being created
  profiles <- qgis_run_algorithm(
    algorithm = "native:geometrybyexpression",
    INPUT = pag,
    EXPRESSION = newexpression,
    OUTPUT_GEOMETRY = 1
  )

  qgis_extract_output(profiles)                                                 # needed to make output readable
  gbe <- sf::st_as_sf(profiles)

  #defining and adding line_id as field to maintain identification
  gbe[["line_id"]] <- 1:nrow(gbe)
  gbe$line_id <- 1:nrow(gbe)



  # creating points along the new created profiles to get 'crossprofiles' of the track
  # (pfl = profile from lines)
  sagaPFL <- qgis_run_algorithm(
    algorithm = "native:pointsalonglines",
    INPUT = gbe,
    DISTANCE = dist_cross_points #in meters
  )
  qgis_extract_output(sagaPFL)                                                  # needed to make output readable
  pfl <- sf::st_as_sf(sagaPFL)

  # buffer around points along vertical lines
  # needed because joining by location would not work
  bufferedpoints <- sf::st_buffer(pfl, 0.001, endCapStyle = "ROUND",
                                  joinStyle = "ROUND")

  # join attributes by location
  joinedL <- st_join(bufferedpoints, gbe, left = T)


  # recreate center points of buffers to later add the DSM (slope) data
  centerpoints <- sf::st_centroid(joinedL)


  # adding the slope values to the centerpoints
  slopepoints <- terra::extract(slope,centerpoints)
  centerpoints$slope <- slopepoints[, -1]

  # adding dsm z values for later join
  dsmpoints <- terra::extract(dsm,centerpoints)
  centerpoints$z <- dsmpoints[, -1]




  #removing the unnecessary line_id.y.x that have been created by joining
  centerpoints <- centerpoints %>%
    dplyr::select(!ends_with("y"))                                              # first filter all without 'y'-ending

  centerpoints_newcol <- gsub("\\.x$", "", colnames(centerpoints))              # then deleting the 'x' -endings

  colnames(centerpoints) <- centerpoints_newcol                                 # and making the new clean colnames



  # splitting the track sides into to to get the left side only

  sidebuff_distance <- profile_length/2                                         # buffer exactly long as the profile of the side

  upbuff <- qgis_run_algorithm(                                                 # creating the single sided buffer
    algorithm = "native:singlesidedbuffer",
    INPUT = tracks,
    DISTANCE = sidebuff_distance ,
    SIDE = 1,                                                                   # 1 is left side
    SEGMENTS =8,
    JOIN_STYLE=0,
    MITER_LIMIT= 2
  )

  qgis_extract_output(upbuff)                                                   # needed to unwrap the output
  upbuff <- sf::st_as_sf(upbuff)


  # filter the centerpoints by ony side only
  upperslope <- st_filter(centerpoints,upbuff)


  # createt categorial statistics to get the maximum slope of one side

  upperstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = upperslope,
    VALUES_FIELD_NAME = "slope",
    CATEGORIES_FIELD_NAME = "line_id"
  )



  su <- qgis_extract_output(upperstats)                                         # needed to unwrap the output
  stats_up <- sf::st_as_sf(su)


  # join attributes from pointslayer with the slope info by the line_id
  slope_up_stats <- dplyr::left_join(upperslope, stats_up, by = "line_id")

  # select objects where slope value is the same as max value (so we only have the max slope point of the profiles)
  selected_up <- slope_up_stats[slope_up_stats$slope == slope_up_stats$max,]

    selected_up$Pointtype <- "Left"                                             # to categorize the point
    selected_up <- na.omit(selected_up)                                         # delete possible NAs


    drop <- c("angle", "unique", "distance",                                    # defining which columns are not needed
              "count", "range","mean","sum",
              "max", "median", "minority", "majority",
              "q1", "q3", "iqr", "min")
    selected_up <- selected_up[,!(names(selected_up) %in% drop)]                # deleting unncessesary columns that have been created during the process


 # export the points as GeoPackage
  if(isTRUE(export)) {

    st_write(selected_up, "left_points.gpkg", driver = "GPKG")


  }

# return the points with maximum slope of the left side of the track
return(selected_up)


}
