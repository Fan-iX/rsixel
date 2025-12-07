#' Read a SIXEL image
#'
#' Reads an image from a SIXEL file into a raster array.
#'
#' @param source character, name of the file to read from.
#'
#' @return A raster array with values ranging from 0 to 1.
#'   The array has dimensions (height, width, 3) where the third dimension
#'   represents the R, G, and B color channels.
#' @export
#'
#' @examples
#' # read a sample file
#' img <- readSIXEL(system.file("snake.six", package="rsixel"))
#'
readSIXEL <- function(source) {
  data <- paste(readLines(source, warn = FALSE), collapse = "")
  sixelDecode(data)
}
