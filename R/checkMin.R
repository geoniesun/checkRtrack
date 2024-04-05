
#' Generates the lowest points along the paths
#'
#' 'min_points()' saves a GeoPackage or returns an sf object of minimumpoints.
#' For a better workflow, use the function as 'minimumpoints <- min_points(...)'.
#' You can use it then directly as an input for the 'maptrack()' function.
#'



#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks GDigital Surface Model raster file as '.tif'.
#' @param export
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#' @param st_dev Minimum standarddeviation of crossprofile dsm-value 'z'. Acts as a filter. Defaults to '0.06'.
#'
#' @return Saves a GeoPackage in the outputfolder of the minimumpoints
#' @export
#'
#' @examples
#'

min_points <- function(dsm, tracks, export = TRUE, dist_cross = 1, profile_length = 1, dist_cross_points = 0.05, st_dev = 0.06) {

  checkFunction <- function() {
    user_input <- readline("Are you sure your Tracks-Layer provides the needed conditions for this function?(y/n):")
    if(user_input != "y") stop("Exiting since you did not press y.
                               Please import your tracks layer with the import function 'read_tracks() to check the conditions.")
  }

  checkFunction() #if

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

  joinedL <- st_join(bufferedpoints, gbe, left = T)

  # recreate center points of buffers to later add the DSM data

  centerpoints <- sf::st_centroid(joinedL)

  # adding the dsm values to the points
  centerpoints <- st_transform(centerpoints, crs = st_crs(dsm))

  dsmpoints <- terra::extract(dsm,centerpoints)
  centerpoints$z <- dsmpoints[, -1]



  #removing the unnecessary line_id.y.x
  centerpoints <- centerpoints %>%
    dplyr::select(!ends_with("y"))

  if ("fade_scr.x" %in% colnames(centerpoints)) {

    centerpoints <-  centerpoints %>%
      dplyr::rename(
        class_id = class_id.x,
        line_id = line_id.x,
        fade_scr = fade_scr.x,
        distance = distance.x,
        angle = angle.x
      )
  }
  else {
    centerpoints <-  centerpoints %>%
      dplyr::rename(
        class_id = class_id.x,
        line_id = line_id.x,
        distance = distance.x,
        angle = angle.x
      )

  }




  #categorial statistics

  catstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = centerpoints,
    VALUES_FIELD_NAME = "z",
    CATEGORIES_FIELD_NAME = "line_id"

  )

  s <- qgis_extract_output(catstats)
  stats <- sf::st_as_sf(s)


  #join attributs from points layer with dsm info by line_id and min(z)
  pointsandstats <- dplyr::left_join(centerpoints, stats, by = "line_id")


  #select objects where Z value is the same as minimum value (so we only have the minimum object of the profiles)
  selected <- pointsandstats[pointsandstats$z == pointsandstats$min,]
  selected <- selected[selected$stddev > st_dev,]

  if ("fade_scr" %in% colnames(selected)) {
    minimumpoints <- selected[,c("class_id","fade_scr","line_id","z","min","stddev","median", "mean")]
    minimumpoints$Pointtype <- "Minimum"

  }
  else{
    minimumpoints <- selected[,c("class_id","line_id","z","min","stddev","median", "mean")]
    minimumpoints$Pointtype <- "Minimum"
  }



  if(export) {
    st_write(minimumpoints, "minimumpoints.gpkg", driver = "GPKG")
    return("You now have a GPKG Layer with minimumpoints along your track in your outputfolder")
 }
 if(!export){
   return(minimumpoints)
 }







}

