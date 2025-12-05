#' Decode SIXEL escape sequence to image data
#'
#' Parse a SIXEL escape sequence and convert it to a three dimensional RGB array.
#'
#' @param data character, SIXEL escape sequence.
#'
#' @return A three dimensional RGB array with values ranging from 0 to 1.
#'   The array has dimensions [height, width, 3] where the third dimension
#'   represents the R, G, and B color channels.
#' @export
#'
#' @examples
#' # Encode an image to SIXEL and decode it back
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' sixel_data <- sixelEncode(img, 4)
#' decoded <- sixelDecode(sixel_data)
#'
sixelDecode <- function(data) {
  # Remove escape sequences at start and end
  # Start: \x1bP..q or \x1bPq (DCS with optional parameters)
  # End: \x1b\\
  data <- gsub("^\x1bP[^q]*q", "", data)
  data <- gsub("\x1b\\\\$", "", data)

  # Parse the raster attributes "Pan;Pad;Ph;Pv where Ph=width, Pv=height
  # Format is "aspect_num;aspect_denom;width;height
  raster_match <- regmatches(data, regexpr("^\"[0-9]+;[0-9]+;[0-9]+;[0-9]+", data))
  if (length(raster_match) > 0 && nchar(raster_match) > 0) {
    raster_parts <- strsplit(sub("^\"", "", raster_match), ";")[[1]]
    width <- as.integer(raster_parts[3])
    height <- as.integer(raster_parts[4])
    # Remove the raster attributes from data
    data <- sub("^\"[0-9]+;[0-9]+;[0-9]+;[0-9]+", "", data)
  } else {
    stop("Could not parse SIXEL raster attributes (dimensions)", call. = FALSE)
  }

  # Parse color palette - format: #index;2;R;G;B (RGB are percentages 0-100)
  # Note: mode 2 is RGB, mode 1 is HLS
  palette <- list()
  palette_pattern <- "#([0-9]+);([12]);([0-9]+);([0-9]+);([0-9]+)"
  palette_matches <- gregexpr(palette_pattern, data, perl = TRUE)
  if (palette_matches[[1]][1] != -1) {
    match_data <- regmatches(data, palette_matches)[[1]]
    for (m in match_data) {
      parts <- strsplit(sub("^#", "", m), ";")[[1]]
      idx <- as.integer(parts[1])
      mode <- as.integer(parts[2])
      if (mode == 2) {
        # RGB mode - values are percentages (0-100)
        r <- as.numeric(parts[3]) / 100
        g <- as.numeric(parts[4]) / 100
        b <- as.numeric(parts[5]) / 100
      } else {
        # HLS mode - convert to RGB
        h <- as.numeric(parts[3])
        l <- as.numeric(parts[4]) / 100
        s <- as.numeric(parts[5]) / 100
        rgb_vals <- .hls_to_rgb(h, l, s)
        r <- rgb_vals[1]
        g <- rgb_vals[2]
        b <- rgb_vals[3]
      }
      palette[[as.character(idx)]] <- c(r, g, b)
    }
  }

  # Initialize the image array (height x width x 3)
  img <- array(0, dim = c(height, width, 3))

  # Remove all palette definitions from data for parsing
  data <- gsub("#[0-9]+;[12];[0-9]+;[0-9]+;[0-9]+", "", data)

  # Helper function to set pixels for a sixel value at current position
  set_sixel_pixels <- function(sixel_value, color, base_row, col) {
    for (bit in 0:5) {
      if (bitwAnd(sixel_value, 2L^bit) > 0L) {
        row <- base_row + bit
        if (row <= height && col <= width) {
          img[row, col, 1] <<- color[1]
          img[row, col, 2] <<- color[2]
          img[row, col, 3] <<- color[3]
        }
      }
    }
  }

  # Split by band separators (- or $)
  # $ = carriage return (go to start of current band)
  # - = newline (move to next 6-row band)
  current_band <- 0
  current_color <- NULL
  x <- 1L  # Current x position (1-indexed)

  # Process character by character
  i <- 1L
  chars <- strsplit(data, "")[[1]]
  n <- length(chars)

  while (i <= n) {
    ch <- chars[i]

    if (ch == "#") {
      # Color selector: #n where n is the color index
      j <- i + 1L
      while (j <= n && grepl("[0-9]", chars[j])) {
        j <- j + 1L
      }
      if (j > i + 1L) {
        current_color <- paste(chars[(i + 1L):(j - 1L)], collapse = "")
      }
      i <- j
    } else if (ch == "!") {
      # RLE: !count<char>
      j <- i + 1L
      while (j <= n && grepl("[0-9]", chars[j])) {
        j <- j + 1L
      }
      # Validate we have digits for the count
      if (j > i + 1L) {
        count <- as.integer(paste(chars[(i + 1L):(j - 1L)], collapse = ""))
        if (!is.na(count) && count > 0L && j <= n) {
          sixel_char <- chars[j]
          sixel_value <- utf8ToInt(sixel_char) - 63L
          if (sixel_value >= 0L && sixel_value <= 63L) {
            # Apply the sixel to 'count' columns
            if (!is.null(current_color)) {
              color <- palette[[current_color]]
              if (!is.null(color)) {
                base_row <- current_band * 6L + 1L
                for (k in seq_len(count)) {
                  if (x <= width) {
                    set_sixel_pixels(sixel_value, color, base_row, x)
                  }
                  x <- x + 1L
                }
              } else {
                x <- x + count
              }
            } else {
              x <- x + count
            }
          }
          i <- j + 1L
        } else {
          i <- j
        }
      } else {
        i <- j
      }
    } else if (ch == "$") {
      # Carriage return - go back to start of current band
      x <- 1L
      i <- i + 1L
    } else if (ch == "-") {
      # Line feed - move to next 6-row band
      current_band <- current_band + 1L
      x <- 1L
      i <- i + 1L
    } else if (ch == "\n" || ch == "\r") {
      # Ignore line breaks in data
      i <- i + 1L
    } else {
      # Regular sixel character (? to ~, i.e., 63-126)
      sixel_value <- utf8ToInt(ch) - 63L
      if (sixel_value >= 0L && sixel_value <= 63L) {
        if (x <= width && !is.null(current_color)) {
          color <- palette[[current_color]]
          if (!is.null(color)) {
            base_row <- current_band * 6L + 1L
            set_sixel_pixels(sixel_value, color, base_row, x)
          }
        }
        x <- x + 1L
      }
      i <- i + 1L
    }
  }

  img
}

# Convert HLS to RGB (internal function)
.hls_to_rgb <- function(h, l, s) {
  if (s == 0) {
    return(c(l, l, l))
  }

  h <- h / 360  # Normalize hue to 0-1

  if (l < 0.5) {
    q <- l * (1 + s)
  } else {
    q <- l + s - l * s
  }
  p <- 2 * l - q

  r <- .hue_to_rgb(p, q, h + 1/3)
  g <- .hue_to_rgb(p, q, h)
  b <- .hue_to_rgb(p, q, h - 1/3)

  c(r, g, b)
}

.hue_to_rgb <- function(p, q, t) {
  if (t < 0) t <- t + 1
  if (t > 1) t <- t - 1
  if (t < 1/6) return(p + (q - p) * 6 * t)
  if (t < 1/2) return(q)
  if (t < 2/3) return(p + (q - p) * (2/3 - t) * 6)
  return(p)
}
