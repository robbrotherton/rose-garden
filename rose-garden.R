# see https://mathworld.wolfram.com/StarrRose.html

library(tidyverse)

# helper function
rand <- function() round(runif(1, 1, 20))
# rand <- function() sample(18, 1)

# generate a star from given paramaters
starr <- function(a, b, c, n = 200, coils = 20, expansion = 10, spiral_b = .01) {

  theta <- seq(0,2*pi*coils, length.out = n*coils)

  spiral_radius <- theta * seq(1, expansion, length.out = n*coils)

  rose <- tibble(x = numeric(n*coils),
                 y = numeric(n*coils))

  for(i in 1:length(theta)) {

    t <- theta[i] %% (pi*2)

    one <- 2+sin(a*t)/2
    two <- t+sin(b*t)/c

    rose$x[i] <- spiral_radius[i] * one * cos(two)
    rose$y[i] <- spiral_radius[i] * one * sin(two)

  }

  rose

}

# eg <- starr(6, 18, 18, coils = 50, expansion = 1)
# ggplot(eg) + geom_path(aes(x, y))

# parameters for rose grid

w <- 4
h <- 5
points <- 200
coils <- 20


palette <- c('#7689a6', '#ff6381', '#a0a776', '#f8ca76')
# palette <- c('#d00001', '#ac0000', '#d3635f', '#b91556') # reds
set.seed(3)

rose_grid <- map_df(.x = 1:(w*h),
                    .f = ~starr(a = rand(), b = rand(), c = rand()),
                    .id = "group") |>
  mutate(group2 = rep(1:(w*h), each = coils*points),
         color = rep(sample(length(palette), w*h, replace = TRUE), each = coils*points),
         xoff = rep(1:w*7000, each = coils*points, length.out = n()),
         yoff = rep(1:h*7000, each = coils*points*w, length.out = n()),
         x0 = x + xoff,
         y0 = y + yoff) %>%
  select(x = x0, y = y0, group, color)



ggplot(rose_grid, aes(x, y, group = group,
                      color = factor(color))) +
  geom_path() +
  scale_color_manual(values = palette) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = hsv(.25, .05, .95))) +
  NULL

ggsave("rose-garden.png", width = 8, height = 10)
