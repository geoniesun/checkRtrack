
#' Generates the lowest points along the paths
#'
#' Can save a GeoPackage and return a sf object of minimumpoints.
#'
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digital Surface Model raster file as '.tif'.
#' @param export If 'TRUE' the GeoPackage will be exported to your wd.
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#' @param st_dev Minimum standarddeviation of crossprofile dsm-value 'z'. Acts as a filter. Defaults to '0.06'.
#'
#' @return Points along the track representing the lowest points of it.
#' @export
#'
#' @examples
#' dsm <- read_dsm(system.file("tif/dsm.tif", package = "checkRtrack"))
#' tracks <- read_tracks(system.file("geopackage/tracks.gpkg", package = "checkRtrack"))
#' mini <- checkMin(dsm, tracks, export = FALSE, dist_cross = 1, profile_length = 1, dist_cross_points = 0.05, st_dev = 0)
#' checkMap(dsm, tracks, points = mini)
#' # run checkMap() as many times as you like to get different examples

checkMin <- function(dsm, tracks, export = TRUE, dist_cross = 1,
                     profile_length = 1, dist_cross_points = 0.05,
                     st_dev = 0.06) {


  # adding tracks_id column to maintain identification of the tracks
  tracks$track_id <- seq.int(nrow(tracks))


  # making sure, that the dsm and the tracks have the same crs
  tracks <- st_transform(tracks, crs=st_crs(dsm))

  # Creating points along the track (pag = points algong geometry)
  result <- qgis_run_algorithm(
    algorithm = "native:pointsalonglines",
    INPUT = tracks,
    DISTANCE = dist_cross #in meters e.g. 1
  )
  qgis_extract_output(result)                                                   # needed to make output readable
  pag <- sf::st_as_sf(result)                                                   # the points along the track




  # creating an expression to create vertical lines on each point of 'pag' and its original track going through it
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

  # defining and adding line_id as field to maintain identification
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

   # recreate center points of buffers to later add the DSM (z) data
  centerpoints <- sf::st_centroid(joinedL)

  # adding dsm z values
  dsmpoints <- terra::extract(dsm,centerpoints)
  centerpoints$z <- dsmpoints[, -1]



  #removing the unnecessary line_id.y.x that have been created by joining
  centerpoints <- centerpoints %>%
    dplyr::select(!ends_with("y"))                                              # first filter all without 'y'-ending

  centerpoints_newcol <- gsub("\\.x$", "", colnames(centerpoints))              # then deleting the 'x' -endings

  colnames(centerpoints) <- centerpoints_newcol                                 # and making the new clean colnames

  # createt categorial statistics to get the minimum z

  catstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = centerpoints,
    VALUES_FIELD_NAME = "z",
    CATEGORIES_FIELD_NAME = "line_id"

  )

  s <- qgis_extract_output(catstats)                                            # needed to unwrap the output
  stats <- sf::st_as_sf(s)


  #join attributs from points layer with dsm z info by line_id
  pointsandstats <- dplyr::left_join(centerpoints, stats, by = "line_id")


  # select objects where Z value is the same as minimum value (so we only have the minimum object of the profiles)
  selected <- pointsandstats[pointsandstats$z == pointsandstats$min,]
  selected <- selected[selected$stddev > st_dev,]
  selected$Pointtype <- "Minimum"                                               # to categorize the point
  selected <- na.omit(selected)                                                 # delete possible NAs

  drop <- c("angle", "unique", "distance", "count", "range","mean", "median",   # defining which columns are not needed
            "min", "minority", "sum","majority", "q1", "q3", "iqr","max")

  selected <- selected[,!(names(selected) %in% drop)]                           # deleting unncessesary columns that have been created during the process



  # export the points as GeoPackage
  if(export) {
    st_write(selected, "minimumpoints.gpkg", driver = "GPKG")
    message("You now have a GPKG Layer with minimumpoints along your track in your outputfolder")
 }

  # return the points with the minimum z of the track
  return(selected)





}

