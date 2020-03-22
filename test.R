library(tidyverse)
library(scales)
library(glue)
library(here)
# install.packages("devtools")
# devtools::install_github("jgjl/ggplot2", ref="patch-1")

# Constants ------------------

testdata_dir = "testdata"
output_dir = "testresults"


# Get test data --------------
d_test_small = tibble('i'=1:100, 'v'=log10(1:100))

# Plot small data set -----

p_test_tailcdf <- ggplot(d_test_small,  aes(y=v)) +
  stat_ecdf() +
  scale_x_continuous(
    trans = trans_logtail(base=10, scale = 4),
    breaks = c(0,0.5,0.9,0.99,0.999,0.9999),
    label = percent
  ) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
  ) +
  NULL

ggsave(
  filename = here(output_dir, "test_small_tailcdf.pdf"),
  plot = p_test_tailcdf,
  height = 8,
  width = 10,
  units = "cm"
)

# ECDF: Plot big data set ------

# Tailcdf ---------------------

d_test_raw = read_csv(here(testdata_dir, "latency.csv")) %>%
  unite(dut, speed, col = "test")

p_test_tailcdf = ggplot(d_test_raw,
                        aes(y=latency,
                            group = test,
                            color = test,
                            linetype = test)) +
  stat_ecdf() +
  scale_x_continuous(
    trans = trans_logtail(base=10, scale = 4),
    # breaks = c(0,0.5,0.9,0.99,0.999,0.9999),
    label = percent
  ) +
  scale_y_continuous(
    name = "Latency [ms]",
    trans = "log10",
    label = comma
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
  filename = here(output_dir, "test_tailcdf.pdf"),
  plot = p_test_tailcdf,
  height = 60,
  width = 84.75,
  units = "mm"
)

