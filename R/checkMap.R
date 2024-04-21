#' Creates an overview map of a random scene of the data to check the results and general quality of digitization.
#'
#' An overview op of the entire tracks and points would be too messy,
#' so each time running this function, a random spot on your tracks will be
#' mapped to see how it worked out (so zooming in).
#' The idea is to adjust the parameters used in the functions that created the
#'  points.
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param tracks Digitized tracks as GeoPackage '.gpkg'.
#' @param points A Geopackage of Points that you want to plot.
#' @param morepoints A Geopackage of Points that you want to plot additionally.
#' @param evenmorepoints A Geopackage of Points that you want to plot additionally.
#' @param export If 'TRUE', the plot as png will be exported to your wd.
#' @param zoom Zoom on your track. 3 as default.
#'
#' @return A Map of a random spot in your AOI showing the process of your points.
#' @export
#' @examples
#'
#' dsm <- read_dsm(system.file("tif/dsm.tif", package = "checkRtrack"))
#' tracks <- read_tracks(system.file("geopackage/tracks.gpkg", package = "checkRtrack"))
#' mini <- sf::st_read(system.file("geopackage/mini.gpkg", package = "checkRtrack"))
#' sides <- sf::st_read(system.file("geopackage/sides.gpkg", package = "checkRtrack"))
#' checkMap(dsm, tracks, points = mini, morepoints = sides)
#' # run checkMap() as many times as you like to get different examples
#'

checkMap <- function(dsm, tracks, points, morepoints=NULL,evenmorepoints =NULL,
                     export = FALSE, zoom = 3) {



## Creating a Map extent and prepare the data


  # adding tracks_id column to maintain identification of the tracks
  tracks$track_id <- seq.int(nrow(tracks))



  # Selecting a random points of the pointslayer (for random mapping)
  randompoint <- points[sample(nrow(points), 1),]

  # defining the extent of the map extent by using the parameter 'zoom'
  buffer <- st_buffer(randompoint, zoom, enCapStyle = "SQUARE")


  # creating an extent for the map
  dsm_subset <- crop(dsm, buffer)
  dsm_extent <- ext(dsm_subset)
  dsm_extent_poly <- as.polygons(dsm_extent, crs = tracks)
  dsm_sf <- sf::st_as_sf(dsm_extent_poly)                                       # creates an sf object that you can clip/crop with


  # subsetting the other input to the map extent
  lines_subset <- st_crop(tracks, dsm_sf)
  points_subset <- st_crop(points, dsm_sf)


  # creating a dataframe of the subsetted dsm to use it as geom_raster
  dsm_df <- as.data.frame(dsm_subset, xy=T)%>%
    na.omit()




## Creating a Hillshade Layer for the Map



  # calculating the slope with terra
  sl <- terrain(dsm_subset, "slope", unit = "radians")

  # estimate the aspect or orientation
  asp <- terrain(dsm_subset, "aspect", unit = "radians")

  # calculate the hillshade effect with 45º of elevation
  hill <- shade(sl, asp,
                angle = 45,
                direction = 300,
                normalize= TRUE)

  # hillshade as dataframe to use as geom
  hill_sf <- as.data.frame(hill, xy= TRUE)




  ## Setting the map

  # gg as map
  gg <- ggplot() +
    geom_raster(data=hill_sf, aes(x,y, fill = hillshade),                       # hillshade
                show.legend = FALSE) +                                          # just in the background for nicer vis
    scale_fill_distiller(palette = "Greys") +
    new_scale_fill() +                                                          # from ggnewscale
    geom_raster(data = dsm_df, aes(x,y, fill = dsm), alpha = 0.7) +             # the dsm (z values)
    scale_fill_continuous_sequential(palette="lajolla", na.value = NA) +
    guides(fill = guide_colorsteps(barwidth = 1,
                                   barheight = 15,
                                   title.position = "top",
                                   show.limits =F,
                                   even.steps = T,
                                   reverse = F)) +
    labs(fill = "Elevation [m]", title = "This is going on in your process..") +
    theme(legend.position = "right") +
    geom_sf(data=lines_subset, show.legend = FALSE) +                           # tracks
    theme(   #for an overview i decided to not put coordinates
     axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
     panel.background = element_rect(fill = "aliceblue")) +

    annotation_scale(location="bl", width_hint = 0.5, pad_x = unit(0.5, "in"),  #scale
                     pad_y = unit(0.5, "in")) +
    annotation_north_arrow(location = "bl",
                           which_north = "true",
                           pad_x = unit(0.5, "in"), pad_y = unit(0.7, "in"),    # north arrow
                           style = north_arrow_fancy_orienteering)


  ## Creating the map depending on which points will be mapped


  # if only one point layer should be mapped :
  if(is.null(morepoints)) {

    # plotting
    ggone <- gg + geom_sf(data=points_subset, aes(colour = Pointtype))

           return(ggone)

  }


  # if two pointlayers should be mapped :
  else {

    if(is.null(evenmorepoints)) {

      # cropping the other points to the extent of the map
      morepoints_crop <- st_crop(morepoints, dsm_sf)

      # filter the points with the stddev
      morepoints_std <- morepoints_crop[morepoints_crop$line_id %in%
                                        points_subset$line_id,]

    # bind the points together again
    allpoints <- bind_rows(points_subset, morepoints_std)


    #plotting
    ggmore <- gg + geom_sf(data=allpoints, aes(colour = Pointtype))

    return(ggmore)

    }


    # if three pointlayers should be mapped :
    else {

      # cropping the other points to the extent of the map
      morepoints_crop <- st_crop(morepoints, dsm_sf)

      # filter the points with the stddev
      morepoints_std <- morepoints_crop[morepoints_crop$line_id %in%
                                          points_subset$line_id,]



      # cropping the other points to the extent of the map
      evenmore_crop <- st_crop(evenmorepoints, dsm_sf)

      # filter the points with the stddev
      evenmore_std <- evenmore_crop[evenmore_crop$line_id %in%
                                      points_subset$line_id,]
      # binding all points together
      firstbind <- bind_rows(points_subset, morepoints_std)
      allpoints <- bind_rows(firstbind, evenmore_std)


      #plotting
      ggmore <- gg + geom_sf(data=allpoints, aes(colour = Pointtype))

      return(ggmore)



    }




  }

  # export the map as png
if(isTRUE(export==TRUE)) {

  ggsave("plot_from_checkRtrack.png")}

}





