checkPolygon <- function(dsm , tracks, export = FALSE, dist_cross = 1,
                       profile_length = 1, dist_cross_points = 0.05,
                       st_dev = 0.06) {


  #extract the width of mean values for segments
line_width_mean <- checkWidth(dsm, tracks, export, plot, dist_cross, profile_length, dist_cross_points, st_dev)


  polygons <- st_buffer(line_width_mean, dist = line_width_mean$width)

  # export the points as GeoPackage
  if(isTRUE(export)) {

    st_write(polygons, "width_buffer_tracks.gpkg", driver = "GPKG")
  }
return(polygons)
}
