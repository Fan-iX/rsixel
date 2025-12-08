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
plot(iris$Petal.Width, iris$Petal.Length)
dev.off()
```

The sixel sequence of the plot will be printed to the console after `dev.off()`.

> [!NOTE]
> You need a terminal emulator that [support SIXEL graphics format](https://www.arewesixelyet.com/) to see the result.

### Encoding an image to sixel format

```r
library(rsixel)
image <- png::readPNG("path/to/image.png")
# For example, the R logo from the `png` package:
# image <- png::readPNG(system.file("img", "Rlogo.png", package="png"))
sixel_sequence <- sixelEncode(image, max.colors = 256, iter.max = 10)
cat(sixel_sequence)
```

A helper function, `imgcat`, is provided to preview images in R sessions directly.

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
sixel_file <- system.file("snake.six", package="rsixel")
sixel_data <- readChar(sixel_file, file.size(sixel_file))
img <- sixelDecode(sixel_data)
```

You can also use `readSIXEL` to read a SIXEL file directly.

```r
img <- readSIXEL(system.file("snake.six", package="rsixel"))
```
