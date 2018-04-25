#' Point class
#'
#' A S3 class for 2d spatial points
#'
#' @param x numeric, x coordinate
#' @param y numeric, y coordinate
#' @return an object of class `point`
#' @examples
#' library(tidyverse)
#' df <- data.frame(x = c(1, 2, 3, 4), y = c(4, 5, 2, 8))
#' mutate(df, p = map2(x, y, point))
#'
#' @export
point <- function(x, y)
{
  structure(list(x = x, y = y), class = "point")
}

#' Calculate distance between two points
#'
#' @param a an object of class `point`
#' @param b an object of class `point`
#'
#' @export
`%--%` <- function(x, ...)
{
  UseMethod("%--%", x)
}

#' @export
`%--%.point` <- function(a, b)
{
  sqrt((b$x - a$x) ^ 2 + (b$y - a$y) ^ 2)
}