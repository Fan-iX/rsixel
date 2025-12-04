#' SIXEL graphics device
#'
#' A graphics device that outputs SIXEL sequences to the console when closed.
#' This device wraps the png() device and encodes the output as SIXEL.
#'
#' @param width integer, width of the output image in pixels. Default is 480.
#' @param height integer, height of the output image in pixels. Default is 480.
#' @param max.colors integer, max colors of the palette. The maximum is 256.
#' This parameter will be passed to [`sixelEncode`]. Default is 256.
#' @param iter.max integer, maximum number of iterations for k-means clustering.
#' This parameter will be passed to [`sixelEncode`]. Default is 10.
#' @param background character, background color to blend with for pixel with
#' transparency. Default is "white".
#' @param file A connection, or a character string naming the file to print to.
#' This parameter will be passed to `cat`. Default is "" (stdout).
#' @param ... Additional arguments passed to `png()`.
#'
#' @return The device number (invisible).
#' @export
#'
#' @examples
#' \dontrun{
#' sixel()
#' plot(c(1, 2))
#' dev.off()
#' }
sixel <- function(
  width = 480,
  height = 480,
  max.colors = 256,
  iter.max = 10,
  background = "white",
  file = "",
  ...
) {
  # Create a temporary PNG file
  tmp_file <- tempfile(fileext = ".png")

  # Open the png device
  grDevices::png(
    filename = tmp_file,
    width = width,
    height = height,
    ...
  )

  # Get the device number
  dev_num <- grDevices::dev.cur()

  # Store the device info in the package environment
  .sixel_env$devices[[as.character(dev_num)]] <- list(
    file = tmp_file,
    max.colors = max.colors,
    iter.max = iter.max,
    background = background,
    output = file
  )

  invisible(dev_num)
}

# Package environment to store sixel device info
.sixel_env <- new.env(parent = emptyenv())
.sixel_env$devices <- list()

#' Close a SIXEL device
#'
#' Close the SIXEL device and print the encoded image to the console.
#' This function is called automatically when `dev.off()` is called
#' on a sixel device.
#'
#' @param which integer, device number to close. Default is the current device.
#'
#' @return The number and name of the new active device (invisible).
#' @keywords internal
.sixel_dev_off <- function(which = grDevices::dev.cur()) {
  # Validate device number - dev.cur() returns 1 for null device
  if (which == 1) {
    stop("cannot shut down device 1 (the null device)", call. = FALSE)
  }

  dev_key <- as.character(which)

  # Check if this is a sixel device
  if (dev_key %in% names(.sixel_env$devices)) {
    device_info <- .sixel_env$devices[[dev_key]]

    # Close the underlying png device first using the stored original function
    result <- .sixel_env$original_dev.off(which)

    # Read and encode the PNG file
    if (file.exists(device_info$file)) {
      # Use the existing load_image and sixelEncode functions
      image <- load_image(device_info$file)

      # Apply background blending if there's an alpha channel
      background <- grDevices::col2rgb(device_info$background) / 255
      if (dim(image)[3] == 4) {
        alpha <- image[, , 4]
        image <- image[, , 1:3]
        image[, , 1] <- image[, , 1] * alpha + background[1] * (1 - alpha)
        image[, , 2] <- image[, , 2] * alpha + background[2] * (1 - alpha)
        image[, , 3] <- image[, , 3] * alpha + background[3] * (1 - alpha)
      }

      # Encode to sixel
      sixel_sequence <- sixelEncode(
        image,
        max.colors = device_info$max.colors,
        iter.max = device_info$iter.max
      )

      # Print to output
      cat(sixel_sequence, file = device_info$output)
      if (device_info$output == "") {
        cat("\n")
      }

      # Clean up temp file
      unlink(device_info$file)
    }

    # Remove device info from the environment
    .sixel_env$devices[[dev_key]] <- NULL

    invisible(result)
  } else {
    # Not a sixel device, call the stored original dev.off
    .sixel_env$original_dev.off(which)
  }
}

# Store the original dev.off when package is loaded
.onLoad <- function(libname, pkgname) {
  .sixel_env$original_dev.off <- grDevices::dev.off
}

.onAttach <- function(libname, pkgname) {
  # Override dev.off when the package is attached
  assign("dev.off", .sixel_dev_off, envir = .GlobalEnv)
}

.onDetach <- function(libpath) {
  # Remove the overridden dev.off from .GlobalEnv
  # The original grDevices::dev.off will be used again
  if (exists("dev.off", envir = .GlobalEnv, inherits = FALSE)) {
    rm("dev.off", envir = .GlobalEnv)
  }
}
