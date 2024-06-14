#' Generates the average width of the tracks.
#'
#' It is recommended to use the other check-functions first. You can plot and export your track widths. The width is defined here as the distance of the steepest points from each side of the track calculated with cross profiles.
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digitized tracks as GeoPackage '.gpkg'.
#' @param export If 'TRUE' the GeoPackage will be exported to your wd.
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#' @param st_dev Minimum standarddeviation of crossprofile dsm-value 'z'. Acts as a filter. Defaults to '0.06'.
#'
#' @return The width information in meters added to the imported tracks objects attribute table and a plot
#' of the width vs. the track type (labelled).
#' @export
#'
#' @examples
#' #better look into the README instead of running it here
#' # https://github.com/geoniesun/checkRtrack/tree/main?tab=readme-ov-file#example-work-flow
#' #! loads quite a bit. Only a very small piece of the tif is available for an example
#' dsm <- read_dsm(system.file("tif/dsm.tif", package = "checkRtrack"))
#' tracks <- read_tracks(system.file("geopackage/tracks.gpkg", package = "checkRtrack"))
#' width <- checkWidth(dsm, tracks, export = FALSE, plot = TRUE, dist_cross = 1, profile_length = 1, dist_cross_points = 0.05, st_dev = 0.06)
#' width
#'

checkWidth <- function(dsm, tracks, export = FALSE, dist_cross = 1,
                       profile_length = 1, dist_cross_points = 0.05,
                       st_dev = 0.06) {

  ## preparation for the width

  # removing possible NAs
  tracks <- na.omit(tracks)
  dsm <- na.omit(dsm)

  # calculating minimum track points
  mini <- checkMin(dsm, tracks, export = FALSE, dist_cross, profile_length,
                   dist_cross_points, st_dev)

  # calculating left extent track points
  left <- checkLeft(dsm, tracks, export = FALSE, dist_cross, profile_length,
                    dist_cross_points)
  # calculating right extent track points
  right <- checkRight(dsm, tracks, export = FALSE, dist_cross, profile_length,
                      dist_cross_points)

  # filter the right and left points with the stddev
  left_std <- left[left$line_id %in%
                     mini$line_id,]

  right_std <- right[right$line_id %in%
                       mini$line_id,]


  ## calculation of the distances to the minimumpoints for each side

  # distance between left and min points (dlm)
  dist_left <- qgis_run_algorithm(
    algorithm = "qgis:distancetonearesthubpoints",
    INPUT = mini,
    HUBS = left_std,
    FIELD = 'line_id',
    UNIT = 0,                                                                     # 0 is meters
    OUTPUT = qgis_tmp_vector()
  )

  qgis_extract_output(dist_left)                                                  # needed to make output readable
  dlm <- sf::st_as_sf(dist_left)                                                  # the points along the track

  # adding the left distance field
  dlm <- dlm %>%
    rename(
      LeftDistance = HubDist
    )


  # distance between right and min points (drm)
  dist_right <- qgis_run_algorithm(
    algorithm = "qgis:distancetonearesthubpoints",
    INPUT = mini,
    HUBS = right_std,
    FIELD = 'line_id',
    UNIT = 0,                                                                     # 0 is meters
    OUTPUT = qgis_tmp_vector()
  )

  qgis_extract_output(dist_right)                                                 # needed to make output readable
  drm <- sf::st_as_sf(dist_right)

  # adding the right distance field
  drm <- drm %>%
    rename(
      RightDistance = HubDist
    )


  #extracting only the distance and id info to merge the datasets
  drm <- drm[, c("RightDistance", "line_id")]

  # merging both datasets
  merged <- st_join(drm, dlm, by = line_id)

  # filter away the new colname issues
  merged <- merged %>%
    dplyr::select(!ends_with("y"))                                                # first filter all without 'y'-ending

  merged_newcol <- gsub("\\.x$", "", colnames(merged))                            # then deleting the 'x' -endings

  colnames(merged) <- merged_newcol


  merged$width <- merged$RightDistance + merged$LeftDistance

  #take out width which is longer than profile length
  merged_filtered <- dplyr::filter(merged, width <= profile_length)

  # created categorial statistics to get the mean width of the track

  widthstats <- qgis_run_algorithm(
    algorithm = "qgis:statisticsbycategories",
    INPUT = merged_filtered,
    VALUES_FIELD_NAME = "width",
    CATEGORIES_FIELD_NAME = "track_id",
    OUTPUT = qgis_tmp_vector()
  )


  sw <- qgis_extract_output(widthstats)                                           # needed to unwrap the output
  stats_width <- sf::st_as_sf(sw)

  # adding mean width of tracks to the tracks layer

  tracks_width <- tracks

  # adding tracks_id column to maintain identification of the tracks
  tracks_width$track_id <- seq.int(nrow(tracks))

  #join attributs from points layer with dsm z info by line_id
  tracks_width_join <- dplyr::left_join(tracks_width, stats_width, by = "track_id")
  tracks_width_join <- na.omit(tracks_width_join)


  tracks_width_join$width <- tracks_width_join$mean

  drop <- c("unique", "count", "mean", "range", "median",   # defining which columns are not needed
            "min", "minority", "sum", "majority", "q1", "q3", "iqr", "max", "stddev")

  tracks_width_join <- tracks_width_join[, !(names(tracks_width_join) %in% drop)]


  # export the points as GeoPackage
  if (isTRUE(export)) {

    st_write(tracks_width_join, "tracks_width.gpkg", driver = "GPKG")
  }

  return(tracks_width_join)

}
