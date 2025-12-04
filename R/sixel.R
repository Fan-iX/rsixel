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

  # Register this device for SIXEL encoding when closed
  .Call(C_sixel_register, as.integer(dev_num), tmp_file, 
        as.integer(max.colors), as.integer(iter.max), background, file)
  
  # Ensure task callback is active
  .sixel_ensure_callback()

  # Return the device number
  invisible(dev_num)
}

# Internal function called to encode and output SIXEL
.sixel_encode_and_output <- function(filename, max_colors, iter_max, 
                                      background, output) {
  if (file.exists(filename)) {
    # Load and process the image
    image <- load_image(filename)
    image <- blend_alpha(image, background)
    
    # Encode to sixel
    sixel_sequence <- sixelEncode(
      image,
      max.colors = max_colors,
      iter.max = iter_max
    )
    
    # Print to output
    cat(sixel_sequence, file = output)
    if (output == "") {
      cat("\n")
    }
    
    # Clean up temp file
    unlink(filename)
  }
  
  invisible(NULL)
}

# Task callback to check for closed sixel devices
.sixel_task_callback <- function(expr, value, ok, visible) {
  if (.Call(C_sixel_has_devices)) {
    # Get list of registered devices
    registered <- .Call(C_sixel_get_devices)
    
    # Get currently open devices
    open_devices <- grDevices::dev.list()
    
    # Find devices that were closed
    for (dev_num in registered) {
      if (!(dev_num %in% open_devices)) {
        # This device was closed, process it
        info <- .Call(C_sixel_pop_device, as.integer(dev_num))
        if (!is.null(info)) {
          .sixel_encode_and_output(
            info$filename,
            info$max_colors,
            info$iter_max,
            info$background,
            info$output
          )
        }
      }
    }
  }
  
  # Keep callback active
  TRUE
}

# Ensure the task callback is registered
.sixel_ensure_callback <- function() {
  # Check if callback is already registered
  callbacks <- getTaskCallbackNames()
  if (!("rsixel_callback" %in% callbacks)) {
    addTaskCallback(.sixel_task_callback, name = "rsixel_callback")
  }
}

# Package initialization
.onLoad <- function(libname, pkgname) {
  # Task callback will be added when first sixel() device is created
}

.onUnload <- function(libpath) {
  # Remove the task callback
  tryCatch(
    removeTaskCallback("rsixel_callback"),
    error = function(e) NULL
  )
}
