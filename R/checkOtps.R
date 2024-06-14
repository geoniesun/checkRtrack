#' Set global options for checkRtrack
#'
#' shortcut to options(checkRtrack.*)
#'
#' @param verbose Logical. If \code{TRUE} many functions will print a status animation that tasks are still running and are not frozen.
#' @export
#' @return
#' No return, just a setter for the verbosiness of the checkRtrack package
#' @examples
#' checkOpts(verbose=TRUE)
#'
checkOpts <- function(verbose){
    options(checkRtrack.verbose=verbose)
}