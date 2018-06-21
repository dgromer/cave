#' 2D Point class
#'
#' A S3 class for 2d spatial points
#'
#' @param x numeric, x coordinate
#' @param y numeric, y coordinate
#' @return an object of class `point2d`
#' @examples
#' library(tidyverse)
#' df <- data.frame(x = c(1, 2, 3, 4), y = c(4, 5, 2, 8))
#' mutate(df, p = map2(x, y, point2d))
#'
#' @export
point2d <- function(x, y)
{
  structure(list(x = x, y = y), class = "point2d")
}

#' 3D Point class
#'
#' A S3 class for 3d spatial points
#'
#' @param x numeric, x coordinate
#' @param y numeric, y coordinate
#' @param z numeric, z coordinate
#' @return an object of class `point3d`
#' @examples
#' library(tidyverse)
#' df <- data.frame(x = c(1, 2, 3, 4), y = c(4, 5, 2, 8), z = c(2, 5, 7, 8))
#' mutate(df, p = pmap(list(x, y, z), point3d))
#'
#' @export
point3d <- function(x, y, z)
{
  structure(list(x = x, y = y, z = z), class = "point3d")
}

#' Calculate distance between two points
#'
#' @param a an object of class `point2d` or `point3d`
#' @param b an object of class `point2d` or `point3d`
#'
#' @export
`%--%` <- function(x, ...)
{
  UseMethod("%--%", x)
}

#' @export
`%--%.point2d` <- function(a, b)
{
  sqrt((b$x - a$x) ^ 2 + (b$y - a$y) ^ 2)
}

#' @export
`%--%.point3d` <- function(a, b)
{
  sqrt((b$x - a$x) ^ 2 + (b$y - a$y) ^ 2 + (b$z - a$z) ^ 2)
}

#' @export
`%--%.POINT` <- function(a, b)
{
  if (inherits(a, "XY"))
  {
    sqrt((b[1] - a[1]) ^ 2 + (b[2] - a[2]) ^ 2)
  }
  else if (inherits(a, "XYZ"))
  {
    sqrt((b[1] - a[1]) ^ 2 + (b[2] - a[2]) ^ 2 + (b[3] - a[3]) ^ 2)
  }
  else
  {
    stop("Only supports typse 'XY' and 'XYZ'")
  }
}
