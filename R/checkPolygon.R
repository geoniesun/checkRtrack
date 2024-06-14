#' Generates a polygon around the input lines as wide as the detected width of the track
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digitized tracks as GeoPackage '.gpkg'.
#' @param export If 'TRUE' the GeoPackage will be exported to your wd.
#' @param dist_cross Distance between each crossprofile in meter. Defaults to '1'.
#' @param profile_length Length of the crossprofile in meter. Defaults to '1'.
#' @param dist_cross_points Distance of the points on the crossprofile in meter. Defaults to '0.05'.
#' @param st_dev Minimum standarddeviation of crossprofile dsm-value 'z'. Acts as a filter. Defaults to '0.06'.
#'
#' @return a buffer around the input lines representing the calculated width of the tracks. It includes a column with the width in meters.
#' @export
#'
#' @examples
#' polygon_as_width <- checkPolygon(dsm, tracks, export = FALSE, dist_cross = 1,profile_length = 1, dist_cross_points = 0.05,st_dev = 0.06)
#' polygon_as_width


checkPolygon <- function(dsm, tracks, export = FALSE, dist_cross = 1,
                         profile_length = 1, dist_cross_points = 0.05,
                         st_dev = 0.06) {

  # extract the width of mean values for segments
  line_width_mean <- checkWidth(dsm, tracks, export, dist_cross, profile_length, dist_cross_points, st_dev)

  halfdist <- (line_width_mean$width) / 2
  polygons <- st_buffer(line_width_mean, dist = halfdist)

  # export the points as GeoPackage
  if (isTRUE(export)) {

    st_write(polygons, "width_buffer_tracks.gpkg", driver = "GPKG")
  }

  return(polygons)
}


# show_waiting_dots <- function(task_function, dsm, tracks) {
#   # Plan to use multisession for asynchronous processing
#   future::plan(future::multisession)
#
#   # Run the task in a separate future with the provided arguments
#   task_future <- future::future({
#     task_function(dsm, tracks)
#   })
#
#   # Show dots while waiting for the task to complete
#   while (!future::resolved(task_future)) {
#     for (i in 1:5) {
#       if (future::resolved(task_future)) break
#       cat(".")
#       flush.console()
#       Sys.sleep(0.1)  # Adjust the sleep time as needed
#     }
#
#     if (!future::resolved(task_future)) {
#       cat("\r     \r")  # Clear the dots
#       flush.console()
#     }
#   }
#
#   cat("\r     \r")
#   flush.console()
#   cat("\n")
#
#   # Retrieve the result or handle the error
#   task_result <- future::value(task_future)
#
#   if (is.null(task_result)) {
#     message("The task returned NULL. An error might have occurred.")
#     stop("Future task encountered an error. Check the logs for details.")
#   }
#
#   return(task_result)
# }
#
#
# # Example usage with custom arguments
# result <- show_waiting_dots(checkPolygon, wrap(dsm), tracks)
# cat("Result:", result, "\n")
