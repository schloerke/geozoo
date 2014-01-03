#' Print
#'
#' Prints geozoo objects with tourr or prints them
#'
#' @S3method print geozoo
#' @method print geozoo
#' @param x geozoo object
#' @param ... other arguements
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/}
#' @author Barret Schloerke
#' @examples
#' \dontrun{
#' example(boy.surface)
#' example(cube.face)
#' }
#' @keywords dynamic
print.geozoo <- function(x, ...){
  colnames(x$points) <- paste("V", seq_len(ncol(x$points)))

  if(nrow(x$points) > 1000){
    message("Reduce points to improve performance\n")
  }

  if(!is.null(x$edges)){
    message("Edges will not be printed using 'tourr'\n")
  }

  if (! require(tourr) ) {
    as.data.frame(x)
  } else {
    tourr::animate_xy(x$points)
  }

}
