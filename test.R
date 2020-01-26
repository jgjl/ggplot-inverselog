library(tidyverse)
library(scales)
library(glue)
library(here)

# Constants ------------------

testdata_dir = "testdata"
output_dir = "testresults"


# Get test data --------------
d_test_small = tibble('i'=1/(1:10000), 'v'=log10(1:10000))

d_test_big = read_csv(here(testdata_dir, "testdata_big.csv"),
                      col_types = cols(
                        Percentiles = col_double(),
                        test = col_character(),
                        t = col_double()
                      ))

  
# Core functions -------------

reverselog_trans <- function(base = exp(1),
                             data_trans = identity,
                             data_trans_inverse = identity) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), 
            compose(trans, data_trans),
            compose(data_trans_inverse, inv),
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


# Plot small data set -----

p_test_small <- ggplot(d_test_small,  aes(i,v)) +
  geom_point() + 
  scale_x_continuous(
    trans = reverselog_trans(10),
    labels = compose(percent, reverse_percent)
    )
  NULL

ggsave(
  filename = here(output_dir, "test_small_dataset.pdf"),
  plot = p_test_small,
  height = 60,
  width = 84.75,
  units = "mm"
)


# Plot big data set ------
  
p_test_big = ggplot(d_test_big,  aes(x=Percentiles,
                       y=t, 
                       group = test,
                       color = test,
                       linetype = test)) +
  geom_step(direction = "hv") +
  scale_x_continuous(
    trans = reverselog_trans(base=10,
                             data_trans = function(x) 1-x,
                             data_trans_inverse = function(x) x),
    labels = compose(percent)
    ) +
  scale_y_continuous(
    name = "Latency [ms]",
    trans = "log10",
    expand = expand_scale(mult=c(00.1,0)),
    labels = compose(comma, function(x) x/1000)
  ) +
  scale_color_brewer(palette="Dark2") +
  guides(
    color = guide_legend(title = "Test"),
    linetype = guide_legend(title = "Test")
  ) +
  theme_bw() +
  theme(
    legend.position="right",
    text = element_text(size=9),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    legend.margin=margin(0,0,0,0),
    legend.key.width = unit(4,"mm"),
    legend.box.margin=margin(-10,-2,-10,-10)
    ) +
  NULL
  
ggsave(
  filename = here(output_dir, "test_big_dataset.pdf"),
  plot = p_test_big,
  height = 60,
  width = 84.75,
  units = "mm"
)
