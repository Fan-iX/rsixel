# rsixel

SIXEL image encoding / decoding for R.

## Installation

```
pak::pak("Fan-iX/rsixel")
```

You may also need `jpeg`, `png` or `magick` package to read image files.

## Usage

### SIXEL graphics device

The `sixel()` function creates a graphics device that outputs SIXEL sequences
to the console when closed. This allows you to display plots directly in
terminals that support SIXEL graphics.

```r
library(rsixel)
sixel()
plot(c(1, 2))
dev.off()
```

The sixel sequence of the plot will be printed to the console after `dev.off()`.

### Encoding an image to sixel format

```r
library(rsixel)
image <- png::readPNG("path/to/image.png")
# For example, the R logo from the `png` package:
# image <- png::readPNG(system.file("img", "Rlogo.png", package="png"))
sixel_sequence <- sixelEncode(image, max.colors = 256, iter.max = 10)
cat(sixel_sequence)
```

You can use `imgcat` to preview images in R sessions directly.

```r
library(rsixel)
imgcat("path/to/image.png")
# imgcat(system.file("img", "Rlogo.png", package="png"))

png("mpg.png")
ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
dev.off()
imgcat("mpg.png")
```

### Decoding sixel sequence to an image

```r
img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
sequence <- sixelEncode(img)
img2 <- sixelEncode(sequence)
```
