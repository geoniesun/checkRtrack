#' Generates the track limit on the right side of the track.
#'
#' Can save a GeoPackage and return a sf object of right side points.
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digitized tracks as GeoPackage '.gpkg'.
#' @param export If 'TRUE' the GeoPackage will be exported to your wd.
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#'
#' @return Points along the right side of the track representing the outer right
#'         extent of the track.
#' @export
#'
#' @examples
#' dsm <- read_dsm(system.file("tif/dsm.tif", package = "checkRtrack"))
#' tracks <- read_tracks(system.file("geopackage/tracks.gpkg", package = "checkRtrack"))
#' right <- checkRight(dsm, tracks, export = FALSE, dist_cross = 1, profile_length = 1,
#'           dist_cross_points = 0.05)
#' str(right)
#' checkMap(dsm, tracks, points = right) #run checkMap() as many times as you like to get different examples
#' # run checkMap() as many times as you like to get different examples


checkRight <- function(dsm, tracks, export = TRUE, dist_cross = 1,
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

  # calculate the slope of the dsm
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
    DISTANCE = dist_cross #in meters e.g. 1
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

  # defining and adding line_id as field to maintain identification
  gbe[["line_id"]] <- 1:nrow(gbe)                                               #defining line_ad as field
  gbe$line_id <- 1:nrow(gbe)                                                    #adding line_ad as field to gbe


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
  joinedL <- st_join(bufferedpoints, gbe, left = T)#until here the package worked 25.03.2024

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

  # splitting the track sides into to to get the right side only
  sidebuff_distance <- profile_length/2                                         # buffer exactly long as the profile of the side

  downbuff <- qgis_run_algorithm(                                               # creating the single sided buffer
    algorithm = "native:singlesidedbuffer",
    INPUT = tracks,
    DISTANCE = sidebuff_distance ,
    SIDE = 0,                                                                   # 0 is right side
    SEGMENTS =8,
    JOIN_STYLE=0,
    MITER_LIMIT= 2
  )
  qgis_extract_output(downbuff)                                                 # needed to unwrap the output
  downbuff <- sf::st_as_sf(downbuff)


  # filter the centerpoints by ony side only
  downperslope <- st_filter(centerpoints,downbuff)


  # created categorial statistics to get the maximum slope of one side

  downperstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = downperslope,
    VALUES_FIELD_NAME = "slope",
    CATEGORIES_FIELD_NAME = "line_id"

  )



  su <- qgis_extract_output(downperstats)                                       # needed to unwrap the output
  stats_down <- sf::st_as_sf(su)


  # join attributes from pointslayer with the slope info by the line_id
  slope_down_stats <- dplyr::left_join(downperslope, stats_down, by = "line_id")

  # select objects where slope value is the same as max value (so we only have the max slope point of the profiles)
  selected_down <- slope_down_stats[slope_down_stats$slope == slope_down_stats$max,]


    selected_down$Pointtype <- "Right"                                          # to categorize the point
    selected_down <- na.omit(selected_down)                                     # delete possible NAs

    drop <- c("angle", "unique", "distance", "count", "range","mean", "min",    # defining which columns are not needed
              "sum", "max", "median", "minority", "majority", "q1", "q3", "iqr")

    selected_down <- selected_down[,!(names(selected_down) %in% drop)]          # deleting unncessesary columns that have been created during the process



    # export the points as GeoPackage
  if(isTRUE(export)) {

    st_write(selected_down, "right_points.gpkg", driver = "GPKG")


  }
    # return the points with maximum slope of the right side of the track
return(selected_down)

}
