#' Imports the raster (a Digital Surface Model - dsm).
#'
#' For better workflow use function as "dsm <- read_dsm(yourDSMpath)"
#'
#' @param yourDSMpath Your path to your GEOTIFF file (.tif)
#'
#' @return the imported dsm file
#' @export
#'
#' @examples
read_dsm <- function(yourDSMpath) {

  if(!file.exists(yourDSMpath)) stop("DSM file does not exist. The file you tried to import: ", yourDSMpath, call = FALSE)
  #TASK: converting to tif to be included here
  if(!grep("tif", yourDSMpath)) stop("Your chosen DSM file is not in GEOTIFF format and must be convertet first")

  format <- if(grepl("tif", yourDSMpath)) "TIFF"

  if(format == "TIFF") {
    message("Next step will be easier if you named the output as 'dsm' ('dsm <- read_dsm(yourDSMpath)'). If you did already, great!")

    dsm <- terra::rast(yourDSMpath)

  }




}
