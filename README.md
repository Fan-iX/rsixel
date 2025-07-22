# rsixel

SIXEL image encoding / decoding for R.

## Installation

```
pak::pak("Fan-iX/rsixel")
```

You may also need `jpeg`, `png` or `magick` package to read image files.

## Usage

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

not implemented yet.
