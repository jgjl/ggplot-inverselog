library(tidyverse)
library(scales)
library(glue)
library(here)

# Constants ------------------

testdata_dir = "testdata"
output_dir = "testresults"


# Get test data --------------
d_test_small = tibble('i'=1-1/(1:100), 'v'=log10(1:100))

d_test_big = read_csv(here(testdata_dir, "testdata_big.csv"),
                      col_types = cols(
                        Percentiles = col_double(),
                        test = col_character(),
                        t = col_double()
                      ))

  
# Core functions -------------

reverse_percent <- function(l) {
  1-l
}

dpn <- 0
debug_print <- function(prefix) {
  function(x) {
    dpn <<- dpn + 1
    print(paste(paste(prefix, dpn), x))
    print(glue("min={min(x, na.rm=T)}, max={max(x, na.rm=T)}"))
    x
  }
}

# Source: https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2
reverselog_trans <- function(base = exp(1),
                             data_trans = identity,
                             data_trans_inverse = identity) {
  limiting_value <- 1.0
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  scales::trans_new(
    name = paste0("reverselog-", format(base)), 
    transform = purrr::compose(debug_print("transform before"), 
                               data_trans, 
                               trans, 
                               debug_print("transform after"),
                               .dir="forward"),
    inverse = purrr::compose(debug_print("inverse before"), 
                             function(x) squish(x, range=c(1e-100, Inf)),
                             debug_print("inverse aftersquish"),
                             inv, 
                             debug_print("inverse inbetween"),
                             data_trans_inverse, 
                             function(x) {
                               if (length(x)==2) {
                                 print(glue("length==2: {x}"))
                                return(round(x))
                               } else {
                                 return(x)
                               }
                             },
                             debug_print("inverse after"),
                             .dir="forward"),
    breaks = purrr::compose(function(x) squish(x, range=c(1e-10, 1-1e-10)),
                            debug_print("breaks squished"),
                            scales::log_breaks(base = base, n = 8),
                            debug_print("breaks after"),
                            reverse_percent,
                            # rev,
                            # debug_print("breaks rev after"),
                            # function(x) append(head(x, -1), limiting_value),
                            debug_print("breaks complete"),
                            .dir="forward"),
    domain = c(1e-100, 1))
}


# Plot small data set -----

# y = 1 - x
# x = 1 - y

p_test_small <- ggplot(d_test_small,  aes(i,v)) +
  geom_point() + 
  scale_x_continuous(
    trans = reverselog_trans(base = 10,
                             data_trans = reverse_percent,
                             data_trans_inverse = reverse_percent),
    labels = label_percent(accuracy = 0.00001),
    breaks = c(0,0.9,0.99,0.999,0.9999,1-1e-10),
    ) +
  NULL

ggsave(
  filename = here(output_dir, "test_small_dataset.pdf"),
  plot = p_test_small,
  height = 8,
  width = 10,
  units = "cm"
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
                             data_trans_inverse = function(x) 1-x),
    labels = label_percent(accuracy = 0.00001)
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

