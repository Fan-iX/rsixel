#' Create SIXEL escape sequence from image data
#'
#' Create SIXEL escape sequence from image data. Quantization is done 
#' by k-means clustering.
#'
#' @importFrom stats kmeans
#'
#' @param image a three dimensional RGB array with values ranging from 0 to 1.
#' @param max.colors integer, max colors of the palette. The maximum is 256. 
#' Default is 256.
#' @param iter.max integer, maximum number of iterations for k-means clustering.
#'
#' @return SIXEL escape sequence
#' @export
#'
#' @examples
#' img <- png::readPNG(system.file("img", "Rlogo.png", package="png"))
#' cat(sixelEncode(img, 4))
#'
sixelEncode <- function(image, max.colors = 256, iter.max = 10) {
  if(max.colors < 2) {
    stop("max.colors must be at least 2.", call. = FALSE)
  } else if (max.colors > 256) {
    warning(
      "SIXEL supports a maximum of 256 colors. ",
      "Palette with more than 256 colors may not be displayed correctly."
    )
  }
  height <- dim(image)[1]
  width <- dim(image)[2]
  data <- apply(image, 3, identity)
  max.colors <- min(max.colors,nrow(unique(data)))
  kms <- kmeans(data, max.colors, iter.max = iter.max)
  palette <- round(kms$centers * 100)
  data <- matrix(kms$cluster - 1, nrow = height)
  colors <- apply(palette[, 1:3], 1, paste, collapse = ";")
  str_pal <- paste(paste0("#", seq_along(colors) - 1, ";2;", colors), collapse = "")
  str_data <- lapply(split(seq_len(height), (seq_len(height) - 1) %/% 6), function(l) {
      band <- data[l, ]
      str_band <- lapply(unique(as.vector(band)), function(c) {
        s <- rle(apply(band == c, 2, function(v) {
          sum(2 ^ (which(v) - 1))
        }))
        paste0("#", c, paste0(unlist(
          Map(function(l, v) {
            if (l > 3)
              paste0("!", l, intToUtf8(v + 63))
            else
              rep(intToUtf8(v + 63), l)
          }, s$lengths, s$values)
        ), collapse = ""), "$")
      })
      paste0(paste0(str_band, collapse = ""), "-\n")
    })
  str_data <- paste0(str_data, collapse = "")
  paste0('\x1bP0;2;q"1;1;', width, ";", height, str_pal, str_data, '\x1b\\')
}
