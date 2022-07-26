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

  rose <- tibble(x = numeric(points),
                 y = numeric(points))

  for(i in 1:length(theta)) {

    t <- theta[i] %% (pi*2)

    one <- 2 + sin(a * t) / 2
    two <- t + sin(b * t) / c

    rose$x[i] <- spiral_radius[i] * one * cos(two)
    rose$y[i] <- spiral_radius[i] * one * sin(two)

  }

  rose

}

# eg <- starr(3, 18, 20, coils = 50, expansion = 1)
# ggplot(eg) + geom_path(aes(x, y)) + coord_fixed()

# parameters for grid
w <- 4
h <- 5
n <- 200
c <- 20


palette <- c('#7689a6', '#ff6381', '#a0a776', '#f8ca76')
# palette <- c('#d00001', '#ac0000', '#d3635f', '#b91556') # reds

set.seed(3)

rose_grid <- map_df(.x = 1:(w * h),
                    .f = ~starr(a = rand(), b = rand(), c = rand(),
                                n = n, coils = c),
                    .id = "group") |>
  mutate(color = rep(sample(length(palette), w * h, replace = TRUE), each = c * n),
         xoff = rep(1:w * 7000, each = c * n, length.out = c * n * w * h),
         yoff = rep(1:h * 7000, each = c * n * w, length.out = c * n * w * h),
         x0 = x + xoff,
         y0 = y + yoff) %>%
  select(x = x0, y = y0, group, color)



ggplot(rose_grid, aes(x, y, group = group,
                      color = factor(color))) +
  geom_path(size = .3) +
  scale_color_manual(values = palette) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = hsv(.25, .03, .97),
                                        color = NA))

ggsave("rose-garden.png", width = 5, height = 7)
