#' Decode SIXEL escape sequence to image data
#'
#' Parse a SIXEL escape sequence and convert it to a three dimensional RGB array.
#'
#' @importFrom utils read.table
#'
#' @param data character, SIXEL escape sequence.
#'
#' @return A three dimensional RGB array with values ranging from 0 to 1.
#'   The array has dimensions (height, width, 3) where the third dimension
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
  data <- gsub("\n", "", data)
  data <- gsub("^\x1bP[^q]*q", "", data)
  data <- gsub("\x1b\\\\$", "", data)
  
  meta <- regmatches(data, regexpr("^\"[0-9]+;[0-9]+;[0-9]+;[0-9]+", data))
  if (length(meta) == 0)
    stop("Could not parse SIXEL raster attributes (dimensions)", call. = FALSE)
  
  meta <- strsplit(sub("^\"", "", meta), ";")[[1]]
  width <- as.integer(meta[3])
  height <- as.integer(meta[4])
  img <- array(0, dim = c(height, width, 3))
  data <- sub("^\"[0-9]+;[0-9]+;[0-9]+;[0-9]+", "", data)
  
  palette <- regmatches(data, gregexpr("#[0-9]+;[12];[0-9]+;[0-9]+;[0-9]+", data))[[1]]
  palette <- read.table(text=substring(palette,2),sep=";",row.names=1,col.names=c(".","mode","V1","V2","V3"))
  palette$V1 <- palette$V1 / ifelse(palette$mode==1, 360, 100)
  palette$V2 <- palette$V2 / 100
  palette$V3 <- palette$V3 / 100
  palette[palette$mode==1,-1] <- hls_to_rgb(palette[palette$mode==1,-1])
  palette <- as.matrix(palette[,-1])
  
  data <- gsub("#[0-9]+;[12];[0-9]+;[0-9]+;[0-9]+", "", data)
  data <- strsplit(data, "-", fixed = TRUE)[[1]]
  irow <- 0
  cur_color <- c(0,0,0)
  
  for (softlines in strsplit(data, "$", fixed = TRUE)) {
    for (softline in regmatches(softlines, gregexpr("#[0-9]+|![0-9]+.|.", softlines))) {
      icol <- 1
      for (part in softline) {
        if(startsWith(part,"#")) {
          part <- substring(part, 2)
          if(!part %in% rownames(palette))
            stop("invalid color index: #", part, call. = FALSE)
          cur_color <- palette[part,]
        } else if (startsWith(part,"!")) {
          ncol <- as.integer(substring(part, 2, nchar(part) - 1))
          part <- substring(part, nchar(part))
          brow <- which(intToBits(utf8ToInt(part) - 63) == TRUE)
          if(length(brow)>0){
            img[irow + brow, icol + seq_len(ncol) - 1, ] <- rep(cur_color, each = length(brow) * ncol)}
          icol <- icol + ncol
        } else if (utf8ToInt(part)>=63){
          brow <- which(intToBits(utf8ToInt(part) - 63) == TRUE)
          if(length(brow)>0){
            img[irow + brow, icol, ] <- rep(cur_color, each=length(brow))}
          if(irow==6&&icol==1){print(img[6:12,1:10,])}
          icol <- icol + 1
        }
      }
    }
    irow <- irow + 6
  }
  img
}

hls_to_rgb<- function(hls) {
  H <- hls[,1]
  S <- hls[,3]
  L <- hls[,2]
  rgb <- matrix(0, nrow=nrow(hls), ncol=3)
  
  C <- (1 - abs(2*L - 1)) * S
  X <- C * (1- abs( ((H*6) %% 2) - 1))
  
  iX <- c(2, 1, 3, 2, 1, 3)
  iC <- c(1, 2, 2, 3, 3, 1)
  
  for(i in 1:6) {
    idx <- 60 * (i - 1) <= H & H < 60 * i
    rgb[idx,iX[i]] <- X[idx]
    rgb[idx,iC[i]] <- C[idx]
  }
  
  rgb + L - C/2
}
