#' Imports your track data
#'
#' This functions helps to prepare your track-data for the other steps.
#' This step is highly recommended before using the other functions.
#' For a better workflow use function as "tracks <- read_track(yourTracksPath)"
#'
#' @param yourTracksPath Your path to your GeoPackage file
#'
#' @return The imported tracks
#' @export
#'
#' @examples
#'
#'
read_tracks <- function(yourTracksPath) {

  if(!file.exists(yourTracksPath)) stop("Track file does not exist. The file you tried to import: ", yourTracksPath, call = FALSE)
  #TASK: converting to gpkg to be included here
  if(!grep("gpkg", yourTracksPath)) stop("Your chosen Track file is not in the right format (.gpkg) and must be convertet first")

  format <- if(grepl("gpkg", yourTracksPath)) "GPKG"

  if(format == "GPKG") {

    thetracks <-  sf::st_read(yourTracksPath)


    # Check column names
    if (all(colnames(thetracks) %in% c("class_id", "fade_scr", "geom")) | all(colnames(thetracks) %in% c("class_id", "geom"))) {
      message("Next step will be easier if you named the output as 'tracks' ('tracks <- read_track(yourpath)'). If you did already, great!")

      return(thetracks)




    } else {


      message("Column names are not as expected.
              They can be changed but need to be already in this order:
              'track class, fade score, geometry' or 'track class, geometry'!")
      adjust_names <- readline(prompt = "Do you want to adjust the column names? (y/n): ")
      if (tolower(adjust_names) == "y") {

        ask_fade <- readline(prompt = "You wish to change to
        1: 'track class, fade score, geometry' or
        2: 'track class, geometry'?
        3: Please let me see what my columns look like right now! (Then you have to start the function again after though..)
            enter your answer (1/2/3):")


        if(tolower(ask_fade)== "1") {
          new_colnames <- c("class_id", "fade_scr", "geom")
        }
        if(tolower(ask_fade)== "2"){
          new_colnames <- c("class_id", "geom")
        }
        if(tolower(ask_fade)== "3"){
          message("Run the function again now after checking your structure and columnnames..")
          return(str(thetracks))

          }



        colnames(thetracks) <- new_colnames
        message("You successfully changed your column names. Next step will be easier if you named the output as 'tracks' ('tracks <- read_track(yourpath)'). If you did already, great!")
        return(thetracks)
      } else {

        message("Exiting. Please make sure the column names match the expected names. Check out how your structure at the moment:")
        return(str(thetracks))
      }


    }


  }
}








