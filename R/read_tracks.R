#' Imports your track data
#'
#' This functions helps to prepare your track-data for the other steps.
#' This step is highly recommended before using the other functions.
#' For a better workflow use function as "tracks <- read_track(yourTracksPath)"
#' @param yourTracksPath Your path to your GeoPackage file
#'
#'
#' @export
#' @import ggplot2
#' @import sf
#' @import terra
#' @import qgisprocess
#' @import dplyr
#' @import ggnewscale
#' @import tidyterra
#' @import colorspace
#' @import ggspatial
#' @examples
#' tracks <- read_tracks(system.file("geopackage/tracks_d.gpkg", package = "checkRtrack"))
#' tracks
#'
#'
read_tracks <- function(yourTracksPath) {

  if(!file.exists(yourTracksPath)) stop("Track file does not exist. The file you tried to import: ", yourTracksPath, call = FALSE)
  #TASK: converting to gpkg to be included here
  if(!grep("gpkg", yourTracksPath)) stop("Your chosen Track file is not in the right format (.gpkg) and must be convertet first")

  format <- if(grepl("gpkg", yourTracksPath)) "GPKG"

  if(format == "GPKG") {

    thetracks <-  sf::st_read(yourTracksPath)

        message("The next step will be easier if you named the output as 'tracks' ('tracks <- read_track(yourpath)'). If you did already, great!")

    }
return(thetracks)

  }






