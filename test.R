library(tidyverse)
library(scales)

d = tibble('i'=1/(1:10000), 'v'=log10(1:10000))

reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

reverselog_label <- function(fun) {
  function(l) {
    fun(1-l)
  }
}

reverse_percent <- function(l) {
  1-l
}

ggplot(d, 
       aes(i,v)) +
  geom_point() + 
  # scale_x_reverse(trans="log10") +
  # scale_x_continuous(trans="log10")
  scale_x_continuous(
    trans = reverselog_trans(10),
    labels = compose(percent, reverse_percent)
    )
  NULL

