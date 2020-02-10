#' Compute empirical cumulative distribution
#'
#' The empirical cumulative distribution function (ECDF) provides an alternative
#' visualisation of distribution. Compared to other visualisations that rely on
#' density (like [geom_histogram()]), the ECDF doesn't require any
#' tuning parameters and handles both continuous and categorical variables.
#' The downside is that it requires more training to accurately interpret,
#' and the underlying visual tasks are somewhat more challenging.
#'
#' @inheritParams layer
#' @inheritParams geom_point
#' @param na.rm If `FALSE` (the default), removes missing values with
#'    a warning.  If `TRUE` silently removes missing values.
#' @param n if NULL, do not interpolate. If not NULL, this is the number
#'   of points to interpolate with.
#' @param pad If `TRUE`, pad the ecdf with additional points (-Inf, 0)
#'   and (Inf, 1)
#' @section Computed variables:
#' \describe{
#'   \item{x}{x in data}
#'   \item{y}{cumulative density corresponding x}
#' }
#' @export
#' @examples
#' df <- data.frame(
#'   x = c(rnorm(100, 0, 3), rnorm(100, 0, 10)),
#'   g = gl(2, 100)
#' )
#' ggplot(df, aes(x)) + stat_ecdf(geom = "step")
#'
#' # Don't go to positive/negative infinity
#' ggplot(df, aes(x)) + stat_ecdf(geom = "step", pad = FALSE)
#'
#' # Multiple ECDFs
#' ggplot(df, aes(x, colour = g)) + stat_ecdf()
stat_tailcdf <- function(mapping = NULL, data = NULL,
                      geom = "step", position = "identity",
                      ...,
                      n = NULL,
                      pad = TRUE,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatTailcdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      pad = pad,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatTailCdf <- ggproto("StatTailCdf", Stat,
  compute_group = function(data, scales, n = NULL, pad = TRUE) {
    # If n is NULL, use raw values; otherwise interpolate
    if (is.null(n)) {
      y <- unique(data$y)
    } else {
      y <- seq(min(data$y), max(data$y), length.out = n)
    }

    if (pad) {
      y <- c(-Inf, y, Inf)
    }
    x <- ecdf(data$y)(y)

    new_data_frame(list(x = x, y = y), n = length(y))
  },

  default_aes = aes(x = after_stat(x)),

  required_aes = c("y")
)

new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }
  
  class(x) <- "data.frame"
  
  attr(x, "row.names") <- .set_row_names(n)
  x
}
