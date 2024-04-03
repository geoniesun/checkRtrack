

#' Creating an overview map of a random scene of the data to check the results
#' A follow up after using function 'min_points' and 'outer_points'. Their
#' output files will be used here.
#' An overview op of the entire tracks and points would be too messy
#' so each time running this function, a random spot on your tracks will be mapped to see how it worked out.
#' The idea is to adjust the parameters used in the functions that created the points.
#'
#'
#' @param dsm Digital Surface Model raster file as '.tif'.
#' @param minimumpoints Output file of function 'min_points' as GeoPackage.
#' @param tracks Digital Surface Model raster file as '.tif'.
#' @param side1_extent Output file of the function 'outer_points'. Left.
#' @param side2_extent Output file of the function 'outer_points'. Right.
#'
#' @return A plot, each time changing when running the function.
#' @export
#'
#' @examples
maptrack <- function(dsm, minimumpoints, tracks, side1_extent=NULL, side2_extent=NULL) {




  # Step 2: Generate random coordinates within your data extent
  randompoint <- minimumpoints[sample(nrow(minimumpoints), 1),]

  #randompoint <- st_sample(minimumpoints, size=1)

  #randompoint <- sample(minimumpoints, size= 1)

  buffer <- st_buffer(randompoint, 2, enCapStyle = "SQUARE")


  # Step 3: Filter your data to a smaller area around the random coordinates

  dsm_subset <- crop(dsm, buffer)
  dsm_extent <- ext(dsm_subset)
  dsm_extent_poly <- as.polygons(dsm_extent, crs = tracks)
  dsm_sf <- sf::st_as_sf(dsm_extent_poly)



  lines_subset <- st_crop(tracks, dsm_sf)
  points_subset <- st_crop(minimumpoints, dsm_sf)
  points_subset$Pointtype <- "Minimum"














  # Step 4: Create ggplot object and plot layers

  dsm_df <- as.data.frame(dsm_subset, xy=T)%>%
    na.omit()


  #Step 5: Hillshade

  #estimate the hillshade

  sl <- terrain(dsm_subset, "slope", unit = "radians")

  # estimate the aspect or orientation
  asp <- terrain(dsm_subset, "aspect", unit = "radians")

  # calculate the hillshade effect with 45º of elevation
  hill <- shade(sl, asp,
                angle = 45,
                direction = 300,
                normalize= TRUE)
  # creating the ggplot

  hill_sf <- as.data.frame(hill, xy= T)
  # Regular gradient
  grad <- hypso.colors(10, "dem_poster")

  my_lims <- minmax(dsm_subset) %>% as.integer() + c(-2, 2)

  gg <- ggplot() +
    geom_raster(data=hill_sf, aes(x,y, fill = hillshade), show.legend = FALSE) +
    scale_fill_distiller(palette = "Greys") +
    new_scale_fill() +
    geom_raster(data = dsm_df, aes(x,y, fill = dsm), alpha = 0.7) +
    #scale_fill_gradientn(colours = grad, na.value = NA)
    #scale_fill_hypso_tint_c(palette = "viridis", limits = my_lims) +
    #scale_fill_viridis_c(option = "plasma") +
    scale_fill_continuous_sequential(palette="lajolla", na.value = NA) +
    #scale_fill_gradient(low = "yellow", high = "brown", na.value = NA)+
    guides(fill = guide_colorsteps(barwidth = 21,
                                   barheight = .5,
                                   title.position = "right",
                                   show.limits =F,
                                   even.steps = T,
                                   reverse = F
    )) +
    labs(fill = "m", title = "Overview of what is going on") +
    coord_sf() +
    theme(legend.position = "bottom") +
    geom_sf(data=lines_subset) +
    #geom_sf(data=points_subset, aes(shape = Pointtype)) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank())




  if(is.null(side1_extent) & is.null(side2_extent) ) {

    gg + geom_sf(data=points_subset, aes(colour = Pointtype))


  }

  else if(is.null(side1_extent)) {

    side2_subset <- st_crop(side2_extent, dsm_sf)
    side2_subset_std <- side2_subset[side2_subset$line_id %in% points_subset$line_id,]
    side2_subset_std$Pointtype <- "Right"


    gg + geom_sf(data=side2_subset_std, aes(shape = Pointtype)) +
      geom_sf(data=points_subset, aes(colour = Pointtype))

  }

  else if(is.null(side2_extent)) {


    side1_subset <- st_crop(side1_extent, dsm_sf)
    side1_subset_std <- side1_subset[side1_subset$line_id %in% points_subset$line_id,]
    side1_subset_std$Pointtype <- "Left"


    gg +

      geom_sf(data=side1_subset_std, aes(shape = Pointtype)) +
      geom_sf(data=points_subset, aes(colour = Pointtype))


  }

  else {

    side1_subset <- st_crop(side1_extent, dsm_sf)
    side1_subset_std <- side1_subset[side1_subset$line_id %in% points_subset$line_id,]
    side1_subset_std$Pointtype <- "Left"



    side2_subset <- st_crop(side2_extent, dsm_sf)
    side2_subset_std <- side2_subset[side2_subset$line_id %in% points_subset$line_id,]
    side2_subset_std$Pointtype <- "Right"

    gg + geom_sf(data=side2_subset_std, aes(colour = Pointtype)) +
      geom_sf(data=side1_subset_std, aes(colour = Pointtype)) +
      geom_sf(data=points_subset, aes(colour = Pointtype))

  }




}


