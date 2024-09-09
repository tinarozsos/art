library(tidyverse)
library(ambient)
library(ggforce)

sample_palette <- function(seed) {
  set.seed(seed)
  palettes <- list(
    baker = c("#fe6ab4", "#fe0000", "#fe8d01", "#ffff01", "#008d00", "#00c1c0", "#42009b", "#8f008e"),
    traditional = c("#e40203","#ff8b00", "#feed01", "#007f24", "#004dff", "#760789"),
    philly = c("#010101", "#785016", "#fe0000", "#fd8c00", "#ffe500", "#109e0a", "#0644b3", "#c22edc"),
    bi = c("#d6006f", "#724e94", "#0038a7"),
    pan = c("#ff228c", "#ffd801", "#20b2ff"),
    ace = c("#000000","#7f7f7f","#ffffff","#660066"),
    trans = c("#5bcefa", "#f5a8b8", "#ffffff"),
    gfluid = c("#fe75a1", "#ffffff", "#bd16d6", "#000000", "#323dbb"),
    gqueer = c("#b57edc", "#ffffff", "#4A8123"),
    lesbian = c("#a40061", "#b75592", "#d063a6", "#ededeb", "#e3abce", "#c54e54", "#8a1e04"),
    nb = c("#fef433", "#ffffff", "#9a59cf", "#2d2d2d")
  )
  sample(palettes, 1)[[1]]
}


# Pride flag 1 ------------------------------------------------------------

# sample_noise <- function(x, y, seed) {
#   noises <- c(gen_simplex, gen_worley, gen_perlin, gen_cubic, gen_spheres)
#   frequency <- 2^(-3:5)
#   curl_noise(generator = sample(noises, 1)[[1]], x, y, frequency = sample(frequency, 1), seed = seed)
# }
# 
# pride_flag <- function(seed) {
#   palette <- sample_palette(seed)
#   n_groups <- length(palette)
#   long_grid(x = seq(0, 1, length.out = 1000),
#             y = seq(0, 1, length.out = 800)) |> 
#     mutate(color = cut(y, n_groups),
#            x = x + sample_noise(x, y, seed)$x,
#            y = y + sample_noise(x, y, seed)$y) |> 
#     ggplot(aes(x, y)) +
#     geom_point(aes(color = color), size = 0.5) +
#     scale_y_reverse() +
#     scale_color_manual(values = palette) +
#     theme_void() +
#     theme(legend.position = "none")
# }
# 
# walk(1:100, ~pride_flag(.x) |> ggsave(paste0("fig/pride1/pride", .x, ".png"), plot = _, width = 6, height = 4))

# Pride flag 2 ------------------------------------------------------------

sample_curl <- function(seed, n = 1) {
  set.seed(seed)
  
  noises <- c(gen_simplex, gen_perlin)
  frequency <- seq(0.125, 4, 0.125)
  
  list(noise = sample(noises, n, replace = TRUE), 
       frequency = sample(frequency, n, replace = TRUE))
}

add_curl <- function(d, seed, step = 0.1, n = 1) {

  curls <- sample_curl(seed, n)
  
  for (i in 1:n) {
    d <- d |> 
      rename(x_init = x, y_init = y) |>
      bind_cols(curl_noise(generator = curls$noise[[i]],
                           d$x, d$y, 
                           frequency = curls$frequency[[i]], 
                           seed = seed)) |> 
      mutate(x = x_init + x * step,
             y = y_init + y * step) |> 
      select(-x_init, -y_init)
  }
  d
}

pride_flag <- function(d, seed, step = 0.1, n = 1) {
  palette <- sample_palette(seed)
  n_groups <- length(palette)
  d |> 
    mutate(color = cut(y, n_groups)) |> 
    add_curl(seed, step, n) |> 
    ggplot(aes(x, y)) +
    geom_path(aes(color = color)) +
    # geom_point(aes(color = color), shape = 15, size = 1) +
    scale_y_reverse() +
    scale_color_manual(values = palette) +
    theme_void() +
    theme(legend.position = "none")
}

d <- long_grid(x = seq(0, 1, length.out = 400),
               y = seq(0, 1, length.out = 600))
pride_flag(d, 7, 0.1, 3)

tibble(seed = 51:70,
       step = rep(c(0.05, 0.1, 0.2, 0.5), times = 5),
       n = rep(c(5, 4, 3, 2, 1), times = 4)) |> 
  pwalk(~ pride_flag(d, ..1,  ..2, ..3) |> 
          ggsave(paste0("fig/pride2/pride", ..1, "step", ..2, "n", ..3, "line.png"), 
                 plot = _, width = 10, height = 6))

# Pride flag 3 ------------------------------------------------------------

d <- long_grid(x = seq(0, 1, length.out = 50),
               y = seq(0, 1, length.out = 50))

sample_curl <- function(seed, n = 1) {
  set.seed(seed)
  
  noises <- c(gen_simplex, gen_perlin)
  frequency <- seq(0.125, 4, 0.125)
  
  list(noise = sample(noises, n, replace = TRUE), 
       frequency = sample(frequency, n, replace = TRUE))
}

pride_flag <- function(d, seed, step = 0.1, n = 1) {
  palette <- sample_palette(seed)
  n_groups <- length(palette)
  d |> 
    mutate(color = cut(y, n_groups)) |> 
    add_curl(seed, step, n) |> 
    ggplot(aes(x, y)) +
    geom_voronoi_tile(aes(fill = color, group = -1), 
                      expand = unit(-0.5, 'mm'), alpha = 0.5) +
    # geom_delaunay_segment2(aes(group = -1), colour = palette[1], 
    #                        linewidth = 0.8, alpha = 0.5) +
    scale_y_reverse() +
    scale_color_manual(values = palette) +
    scale_fill_manual(values = palette) +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = margin(-2, -2, -2, -2, "in"),
          panel.background = element_rect(fill = "white"))
}

tibble(seed = 501:560,
       step = rep(c(0.1, 0.2, 0.3), times = 20),
       n = rep(c(3, 2, 2, 1), times = 15)) |> 
  pwalk(~ pride_flag(d, ..1,  ..2, ..3) |> 
          ggsave(paste0("fig/pride3/pride", ..1, "step", ..2, "n", ..3, "line.png"), 
                 plot = _, width = 10, height = 6))

pride_flag(d, 1, 0.2, 2) |> 
  ggsave("fig/pride3/pride1step0.2n2.png", plot = _, width = 10, height = 6)
