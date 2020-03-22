#' Apply inversed log transformation to the axis.
#'
#' The 'zoom' factor of the log transformation will focus on the maximum of the
#' values. This kind of transformation is usefull to visualize tail latency
#' distributions, when used in combination with stat_ecdf.
#'
#' @param base base of logarithm
#' @param scale number of decimal nines after 99% to display
#'
#' @return scales::trans_new object
#' @export
#' @examples
#' trans_logtail(base=10, scale=5)
#'
trans_logtail <- function(base = exp(1), scale=3) {
  scale = scale + 2
  domain = c(base^(-scale), 1.0)
  trans <- function(x) -log((domain[2]-squish(x, domain))+domain[1], base=base)
  inv <- function(y) (domain[2]-base^(-y))+domain[1]
  breaks <- function(n=scale+4)
  {
    function(b) seq(from=min(b), to=max(b), by=(max(b)-min(b))/n)
  }
  scales::trans_new(
    name = paste0("tailcdf-", format(base)),
    transform = trans,
    inverse = inv,
    breaks = breaks(),
    domain = domain)
}

