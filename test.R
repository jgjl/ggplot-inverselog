library(tidyverse)
library(scales)
library(glue)
library(here)
# install.packages("devtools")
# devtools::install_github("easystats/see")
# devtools::install_github("jgjl/ggplot2", ref="patch-1")
library(see)

# Constants ------------------

testdata_dir = "testdata"
output_dir = "testresults"


# Get test data --------------
d_test_small = tibble('i'=1:100, 'v'=log10(1:100))

# Core functions -------------

tailcdf_trans <- function(base = exp(1), scale=10) {
  domain = c(base^(-scale), 1.0)
  trans <- function(x) 
  {
    result = -log((domain[2]-squish(x, domain))+domain[1], base=base)
    return(result)
  }
  inv <- function(y)
  {
    result = (domain[2]-base^(-y))+domain[1]
    return(result)
  }
  breaks <- function(n=5)
  {
    function(b) {
      b_trans = trans(squish(b, range = domain))
      result = seq(from=min(b_trans), to=max(b_trans), by=(max(b_trans)-min(b_trans))/n)
      return(result)
    } 
  }
  scales::trans_new(
    name = paste0("tailcdf-", format(base)), 
    transform = trans,
    inverse = inv,
    breaks = breaks(),
    domain = domain)
}


# Plot small data set -----

p_test_raw <- ggplot(d_test_small,  aes(i,v)) +
  geom_point() + 
  scale_x_continuous() +
  NULL

ggsave(
  filename = here(output_dir, "test_small_raw.pdf"),
  plot = p_test_raw,
  height = 8,
  width = 10,
  units = "cm"
)

p_test_ref <- ggplot(d_test_small,  aes(i,v)) +
  geom_point() + 
  scale_x_continuous(trans="log10") +
  NULL

ggsave(
  filename = here(output_dir, "test_small_ref.pdf"),
  plot = p_test_ref,
  height = 8,
  width = 10,
  units = "cm"
)

p_test_tailcdf <- ggplot(d_test_small,  aes(y=v)) +
  stat_ecdf() + 
  scale_x_continuous( 
    trans = tailcdf_trans(base=10, scale = 4),
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
  
d_test_raw = read_csv(here(testdata_dir, "latency.csv"))

p_test_ecdf = ggplot(d_test_raw,  
                    aes(x=latency,
                       group = dut,
                       color = dut,
                       linetype = dut)) +
  stat_ecdf() +
  scale_x_continuous( ) +
  scale_y_reverse(
    name = "Latency [ms]",
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
  filename = here(output_dir, "test_ecdf.pdf"),
  plot = p_test_ecdf,
  height = 60,
  width = 84.75,
  units = "mm"
)

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
    trans = tailcdf_trans(base=10, scale = 3),
    # trans = "log10",
    breaks = c(0,0.5,0.9,0.99,0.999,0.9999),
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

