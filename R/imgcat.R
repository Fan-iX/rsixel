load_image <- function(file){
  ext <- gsub(".*(\\..*$)", "\\1", file)
  if(ext %in% c(".jpg", ".jpeg") && requireNamespace("jpeg", quietly = TRUE)){
    x <- jpeg::readJPEG(file)
  } else if(ext == ".png" && requireNamespace("png", quietly = TRUE)){
    x <- png::readPNG(file)
  } else if(requireNamespace("magick", quietly = TRUE)){
    x <- as.numeric(magick::image_data(magick::image_read(file)[1]))
  } else {
    stop("Please install `jpeg`, `png` or `magick` package to read image files.", call. = FALSE)
  }
  x
}

# Blend image with background color for pixels with transparency
blend_alpha <- function(image, background) {
  background <- grDevices::col2rgb(background) / 255
  if (dim(image)[3] == 4) {
    alpha <- image[, , 4]
    image <- image[, , 1:3]
    image[, , 1] <- image[, , 1] * alpha + background[1] * (1 - alpha)
    image[, , 2] <- image[, , 2] * alpha + background[2] * (1 - alpha)
    image[, , 3] <- image[, , 3] * alpha + background[3] * (1 - alpha)
  }
  image
}

#' Create SIXEL escape sequence for image file
#'
#' Create SIXEL escape sequence for image file. `jpeg`, `png` or `magick` packages 
#' are required to read image files. Image with alpha channel will be blended with
#' the specified background color.
#'
#' @param path character, path to a image file.
#' @param ... other positional arguments will be omitted.
#' @param max.colors integer, max colors of the palette. The maximum is 256. 
#' This parameter will be passed to [`sixelEncode`].
#' @param iter.max integer, maximum number of iterations for k-means clustering.
#' This parameter will be passed to [`sixelEncode`].
#' @param background character, background color to blend with for pixel with
#' transparency. Default is "white".
#' @param file A connection, or a character string naming the file to print to.
#' This parameter will be passed to `cat`
#'
#' @return None (invisible 'NULL').
#' @export
#'
#' @examples
#' imgcat(jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg")))
imgcat <- function(
  path,
  ...,
  max.colors = 256,
  iter.max = 10,
  background = "white",
  file = ""
) {
  image <- load_image(path)
  image <- blend_alpha(image, background)
  sixel_sequence <- sixelEncode(image, max.colors, iter.max)
  cat(sixel_sequence, file=file)
  if(file == "") { cat("\n") }
}
