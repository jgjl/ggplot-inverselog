library(tidyverse)
library(scales)
library(glue)
library(here)
# install.packages("devtools")
# devtools::install_github("easystats/see")
library(see)

source("stat-tailcdf.r")

# Constants ------------------

testdata_dir = "testdata"
output_dir = "testresults"


# Get test data --------------
# d_test_small = tibble('i'=1-1/(1:100), 'v'=log10(1:100))
# d_test_small = tibble('i'=1:100, 'v'=log10(1:100))
d_test_small = tibble('i'=1:100, 'v'=log10(1:100))

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
  trans <- function(x) log(x, base)
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

taildist_trans <- function(base = exp(1)) {
  x_min <- NaN
  x_max <- NaN
  x_trans_max <- NaN
  domain = c(1e-100, Inf)
  trans <- function(x) 
  {
    x_min <<- min(x)
    x_max <<- max(x)
    x_log <- log(x_max+x_min-x, base=base)
    x_trans_max <<- max(x_log)
    result <- x_trans_max - x_log
    print(paste("Trans:",result, length(x), length(result), length(x_log)))
    return(result)
  }
  inv <- function(y) 
  {
    print(paste("Inv: x_max=",x_max,"x_min=",x_min,"x_trans_max=",x_trans_max))
    result <- x_max + x_min - base^(x_trans_max-squish(y, range = domain))
    print(paste("Inv:",result))
    return(result)
  }
  breaks <- function(n=5)
  {
    function(b) {
      b_trans = trans(b)
      trans_min= min(b_trans)
      trans_max= max(b_trans)
      result = seq(from=trans_min, to=trans_max, by=(trans_max-trans_min)/n)
      print(paste("Breaks:", result)) 
      return(result)
    } 
  }
  scales::trans_new(
    name = paste0("taildist-", format(base)), 
    transform = trans,
    inverse = inv,
    breaks = breaks(),
    domain = domain)
}

tailcdf_trans <- function(base = exp(1), scale=10) {
  domain = c(base^(-scale), 1.0)
  trans <- function(x) 
  {
    print("Trans")
    print(x)
    result = -log((domain[2]-squish(x, domain))+domain[1], base=base)
    tib = tibble(raw=x, trans=result)
    summary(tib %>% filter(trans==min(trans)))
    print(paste("Trans:", result)) 
    return(result)
  }
  inv <- function(y)
  {
    print("Inv")
    print(y)
    result = (domain[2]-base^(-y))+domain[1]
    tib = tibble(raw=y, trans=result)
    summary(tib %>% filter(trans==min(trans)))
    print(paste("Inv:", result)) 
    return(result)
  }
  breaks <- function(n=5)
  {
    function(b) {
      print("Breaks")
      print(b)
      b_trans = trans(squish(b, range = domain))
      result = seq(from=min(b_trans), to=max(b_trans), by=(max(b_trans)-min(b_trans))/n)
      print(paste("Breaks:", result)) 
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

# y = 1 - x
# x = 1 - y

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

p_test_small_log <- ggplot(d_test_small,  aes(i,v)) +
  geom_point() + 
  scale_x_continuous(
    trans = reverselog_trans(base = 10),
    # labels = label_percent(accuracy = 0.00001),
    ) +
  NULL

ggsave(
  filename = here(output_dir, "test_small_log.pdf"),
  plot = p_test_small_log,
  height = 8,
  width = 10,
  units = "cm"
)

p_test_small_rev <- ggplot(d_test_small,  aes(i,v)) +
  geom_point() + 
  scale_x_continuous(
    trans = tailcdf_trans(base = 10),
    ) +
  NULL


ggsave(
  filename = here(output_dir, "test_small_rev.pdf"),
  plot = p_test_small_rev,
  height = 8,
  width = 10,
  units = "cm"
)

p_test_small_tailcdf <- ggplot(d_test_small,  aes(i,v)) +
  stat_tailcdf() + 
  scale_x_continuous(
    trans=tailcdf_trans(base=10),
    breaks = pretty_breaks()
    ) +
  NULL

ggsave(
  filename = here(output_dir, "test_small_tailcdf.pdf"),
  plot = p_test_small_tailcdf,
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
                             data_trans = reverse_percent,
                             data_trans_inverse = reverse_percent),
    labels = label_percent(accuracy = 1e-10),
    expand = expand_scale(mult=c(1,2)),
    ) +
  scale_y_continuous(
    name = "Latency [ms]",
    trans = "log10",
    # expand = expand_scale(mult=c(00.1,0)),
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
    # trans = "log10",
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

d_test_raw = read_csv(here(testdata_dir, "latency.csv"))

p_test_tailcdf = ggplot(d_test_big,  
                    aes(x=Percentiles,
                        y=t,
                        group = test,
                        color = test,
                        linetype = test)) +
  # stat_ecdf() +
  geom_step() +
  scale_x_continuous( 
    trans = tailcdf_trans(base=10, scale = 4),
    # trans = "log10",
    breaks = c(0,0.5,0.9,0.99,0.999,0.9999,0.99999),
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

# Transform -----

d = 1:10/10
d_max = 1.0
d_min = 1e-100
d_log = log10(d)
t0 = d/max(d)
# t1 = d_log/max(d_log)
# t2 = 1-(log10(d_max+d_min-d))/max(log10(d_max+d_min-d))
# t3 = -log10(d_max+d_min-d)
# t4 = d_max + d_min - 10^(-t3)
test_trans <- tailcdf_trans(base=10)
d_trans <- bind_rows(
  tibble(o=d, i = t0, v = "raw"),
  # tibble(o=d, i = t1, v = "log10"),
  # tibble(o=d, i = t2, v = "1-(log10(max(d)-d+min(d)))/max(log10(max(d)-d+min(d)))"),
  tibble(o=d, i = test_trans$transform(d), v = "1-(log10(d_max+d_min-d))"),
  tibble(o=d, i = test_trans$inverse(d), v = "(1-(log10(d_max+d_min-d)))^1"),
)

p_test_trans <- ggplot(d_trans, aes(i,v, group=v, label=o, fill=v)) +
  geom_label(size=1.5, label.padding = unit(0.05, "lines")) +
  scale_x_continuous() +
  theme(legend.position = 'none') +
  NULL

ggsave(
  filename = here(output_dir, "p_test_trans.pdf"),
  plot = p_test_trans,
  height = 8,
  width = 20,
  units = "cm"
)

