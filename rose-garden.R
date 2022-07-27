# see https://mathworld.wolfram.com/StarrRose.html

library(tidyverse)

# helper function to pick random rose parameters
rand <- function() round(runif(1, 1, 20))

# generate a star from given parameters
starr <- function(a, b, c,
                  n = 200,
                  coils = 20,
                  expansion = 10) {

  points <- n * coils

  theta <- seq(0,2*pi*coils, length.out = points)

  spiral_radius <- theta * seq(1, expansion, length.out = points)
  spiral_radius <- spiral_radius / max(spiral_radius)

  rose <- tibble(x = numeric(points),
                 y = numeric(points))

  for(i in 1:length(theta)) {

    t <- theta[i] %% (pi*2)

    one <- 2 + sin(a * t) / 2
    two <- t + sin(b * t) / c

    rose$x[i] <- spiral_radius[i] * one * cos(two)
    rose$y[i] <- spiral_radius[i] * one * sin(two)

  }

  rose / 2

}

# eg <- starr(3, 18, 20, coils = 50, expansion = 10)
# eg <- starr(0, 0, 1, coils = 50, expansion = 10)
# ggplot(eg) + geom_path(aes(x, y)) + coord_fixed()

# parameters for grid
w <- 4
h <- 5
n <- 200
c <- 20
s <- 2.75
seed <- 3


palette <- c('#7689a6', '#ff6381', '#a0a776', '#f8ca76')
# palette <- c('#d00001', '#ac0000', '#d3635f', '#b91556') # reds

set.seed(seed)

rose_grid <- map_df(.x = 1:(w * h),
                    .f = ~starr(a = rand(), b = rand(), c = rand(),
                                n = n, coils = c),
                    .id = "group") |>
  mutate(color = rep(sample(length(palette), w * h, replace = TRUE), each = c * n),
         xoff = rep(1:w * s, each = c * n, length.out = c * n * w * h),
         yoff = rep(1:h * s, each = c * n * w, length.out = c * n * w * h),
         x0 = x + xoff,
         y0 = y + yoff) %>%
  select(x = x0, y = y0, group, color)

grid2 <-  rose_grid |>
  mutate(x = x - min(x),
         y = y - min(y),
         x = x - max(x)*.5,
         y = y - max(y)*.5)

# max_x <- max(grid2$x)
# max_y <- max(grid2$y)
# ratio <- max(max_x, max_y)
#
# grid2 <-  rose_grid |>
#   mutate(x = x * 1/ratio,
#          y = y * 1/ratio,
#          x = x - max())
#
# min(grid2$y)

# bg <- data.frame(x = c(-.5, .5, .5, -.5),
#                  y = c(.5, .5, -.5, -.5))
#
# bg <- data.frame(x = c(.5-s/2, 1+s/2, 1+s/2,.5 -s/2),
#                  y = c(.5, .5, -.5, -.5))

bg_col <- hsv(.25, .03, .97)

ggplot() +
  # geom_polygon(data = draw::rectangle(width = 1+max(grid2$x)/(s*w)*.5,
  #                                     height = 1+max(grid2$y)/(s*h)*.5) + .5, #bg * (1+1/7*(1/8)) + .5,
  #              aes(x, y),
  #              fill = bg_col,
  #              color = bg_col) + #bg_col) +
  geom_polygon(data = draw::rectangle(width = s * w, #max(grid2$x) * 2 + max(grid2$x)/(s*w),
                                 height = s * h), #max(grid2$y) * 2 + max(grid2$y)/(s*h)),
          aes(x, y),
          fill = bg_col,
          color = bg_col) +
  geom_path(data = grid2,
            aes(x, y, group = group,
                color = factor(color)),
            size = .3) +
  # geom_point(data = NULL, aes(x = c(s/2, s+s/2),
  #                             y = c(0, 0))) +
  # geom_point(data = NULL, aes(x = c(s/2, s/2, s/2),
  #                             y = c(0, s, s+s/2))) +
  # geom_point(data = NULL, aes(x = c(s, s+s),
  #                             y = c(0, 0)),
  #            color = 'red') +
  scale_color_manual(values = palette) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = 'none',
        plot.margin = unit(c(0,0,0,0), "in"),
        panel.background = element_rect(fill = bg_col,
                                        color = bg_col))

filename <- glue::glue("outputs/rose-garden_{w}x{h}_seed-{seed}.png")

ggsave(filename, width = w, height = h)
