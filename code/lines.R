library(tidyverse)
library(colorspace)
library(ambient)

# Helper functions --------------------------------------------------------

#' Randomly select greyscale color (weighted towards black/white)
#' @param seed Random seed
#' @return A character of a greyscale color
sample_greyscale <- function(seed) {
  set.seed(seed)
  paste0("grey", sample(0:100, 1, prob = seq(-1, 1, length.out = 101)^2))
}

#' Randomly select R color from colors()
#' @param seed Random seed
#' @return A character of a color
sample_color <- function(seed) {
  set.seed(seed)
  sample(colors(), 1)
}

#' Define a circle outline
#' @param Number of points making up the line
#' @param r Radius of the circle
#' @return A tibble with x and y coordinates
circle <- function(n = 100, r = 1) {
  theta <- seq(0, 2 * pi, length.out = n)
  tibble(x = r * cos(theta), y = r * sin(theta))
}

# Connected circle --------------------------------------------------------

#' Draw circle with random lines connecting points, shaded background
#' @param color Color of circle outline
#' @param bgcolor Background color
#' @param seed Random seed
#' @param n Number of points making up the circle
#' @param bgn Number of points making up the background (per axis)
#' @return A ggplot object
draw_circle <- function(color, bgcolor, seed = 1, n = 100, bgn = 500) {
  set.seed(seed)
  
  # circle with randomly selected points to be connected
  d <- circle(n) |> 
    rename(xend = x, yend = y) |> 
    slice_sample(prop = 1) |> 
    bind_cols(circle(n)) |> 
    mutate(shade = runif(n),
           group = shade < sample(0.5:1, 1))
  
  # background as raster image
  bg <- expand.grid(x = seq(-1.2, 1.2, length.out = bgn),
                    y = seq(-1.2, 1.2, length.out = bgn)) |> 
    # distance from center for shading
    mutate(dist = normalize(abs(1-sqrt(x^2 + y^2)), to = c(0, 0.2)) + 
             rnorm(n(), sd = 0.1),
           # color shade based on distance
           color = darken(bgcolor, dist, space = "HLS"))
  
  ggplot(d, aes(x, y)) +
    # background raster image
    geom_raster(data = bg, aes(fill = color)) +
    # make circle insides darker
    geom_polygon(fill = darken(bgcolor, 0.2, space = "HLS"), alpha = 0.3) +
    # connecting lines
    geom_segment(data = filter(d, group),
                 aes(color = shade, xend = xend, yend = yend),
                 linewidth = 0.5) +
    # circle outline
    geom_path(color = color, linewidth = 0.5) +
    scale_color_gradient(low = darken(color, 0.2), high = lighten(color, 0.2)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none")
}

# draw figures
walk(1:20, ~ draw_circle(sample_greyscale(.x), sample_color(.x), .x) |> 
       ggsave(paste0("fig/lines/connectedcircle", .x, ".png"), 
              plot = _, width = 6, height = 6))

# Points in a circle ------------------------------------------------------

#' Draw circle of negative space with points
#' @param color Color of points
#' @param seed Random seed
#' @return A ggplot object
draw_circle <- function(color, seed = 1) {
  
  # either add vertical cuts to the points
  lines <- tibble(
    x = seq(-4, 4, 0.4),
    xend = x, y = -4, yend = 4,
    # linewidth decreasing from the center
    width = -sqrt(abs(x))
  )
  
  # or connect random points of the circle outline
  lines <- circle(100) |> 
    rename(xend = x, yend = y) |> 
    slice_sample(prop = 1) |> 
    bind_cols(circle(100)) |> 
    mutate(group = runif(n()) < 0.5)
  
  set.seed(seed)
  # generate concentric circles
  map_df(seq(1, 4, 0.1), ~circle(800, .x)) |> 
    # calculate distance of each point from the center
    # add random noise to the points
    mutate(dist = sqrt(x^2 + y^2),
           nudge_x = rnorm(n(), sd = 0.05),
           nudge_y = rnorm(n(), sd = 0.05),
           x = x + nudge_x, y = y + nudge_y) |> 
    ggplot(aes(x, y), size = 0.3) +
    # add points in concentric circles
    geom_point(aes(color = dist), size = 0.3) +
    # block out middle circle
    geom_polygon(data = circle(100), fill = "black") +
    # add either connecting lines or vertical cuts
    geom_segment(data = filter(lines, group),
                aes(xend = xend, yend = yend#, linewidth = width
                    ), color = color) +
    scale_color_gradient(low = color, high = darken(color, 1.5)) +
    scale_linewidth_continuous(range = c(0.5, 5)) +
    coord_fixed() +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "black"))
}

# draw figures
walk(c(38,44), ~ draw_circle(sample_color(.x), .x) |> 
       ggsave(paste0("fig/lines/circlepoints", .x, ".png"), 
              plot = _, width = 10, height = 10))


# Circular patterns from lines --------------------------------------------
## 1 

# define equidistant horizontal lines
d <- tibble(
  x = -1.5,
  xend = 1.5,
  y = seq(-1.5, 1.5, length.out = 20),
  yend = y
)

# define a circle
# change number of points, color, number of defined circles 
circle(100) |> 
  # connect each quadrant to corresponding corner
  mutate(xend = ifelse(x > 0, 1.5, -1.5), 
         yend = ifelse(y > 0, 1.5, -1.5)) |> 
  # add smaller circle inside main circle
  bind_rows(
    circle(100, r = 0.7) |> 
      # connect each quadrant to corresponding top/side center
      mutate(xend = case_when(x < -0.7/sqrt(2) ~ -1.5,
                              x > 0.7/sqrt(2) ~ 1.5,
                              TRUE ~ 0),
             yend = case_when(y < -0.7/sqrt(2) ~ -1.5,
                              y > 0.7/sqrt(2) ~ 1.5,
                              TRUE ~ 0))) |>
  # add even smaller circle
  bind_rows(
    circle(100, 0.5) |> 
      # connect each point in the circle to the center
      mutate(xend = 0, yend = 0)
  ) |> 
  ggplot() +
  # plot all lines
  geom_segment(aes(x, y, xend = xend, yend = yend), size = 0.5, color = "white") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = "black"))

# save figure
ggsave("fig/lines/circleline3.png", width = 6, height = 6)

## 2

# define equidistant horizontal lines outside a circle
h <- circle(60) |> 
  mutate(xend = sign(x)*1.5,
         yend = y)
# define equidistant vertical lines inside a circle
v <- circle(60) |> 
  mutate(yend = -y,
         xend = x)

# plot horizontal and vertical lines
# change colors, number of points in circle
bind_rows(h, v) |> 
  ggplot() +
  geom_segment(aes(x, y, xend = xend, yend = yend), size = 0.5, color = "white") +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = "black"))

# save figure
ggsave("fig/lines/circleline5.png", width = 6, height = 6)

# Line --------------------------------------------------------------------

# connect points on the edges of a square
# initialize points along x axis
tibble(x = seq(0:50),
       y = 0) |> 
  # add end points on y axis
  mutate(yend = max(x)-x,
         xend = 0) |> 
  # repeat for other edges
  bind_rows(
    tibble(y = seq(0:50),
           x = 51) |> 
      mutate(xend = y,
             yend = 0)
  ) |>
  bind_rows(
    tibble(x = seq(0:50),
           y = 51) |> 
      mutate(yend = max(x)-x,
             xend = 51)
  ) |>
  bind_rows(
    tibble(y = seq(0:50),
           x = 0) |> 
      mutate(xend = y,
             yend = 51)
  ) |>
  ggplot() +
  # plot all lines
  geom_segment(aes(x, y, xend = xend, yend = yend), size = 0.5, color = "deepskyblue4") +
  coord_fixed(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white"))
ggsave("fig/lines/line.png", width = 6, height = 6)
