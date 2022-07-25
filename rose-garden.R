library(tidyverse)



rand <- function() round(runif(1, 0, 20))


starr <- function(a, b, c, n, coils, expansion = 10, spiral_b = .01) {

  theta <- seq(0,2*pi*coils, length.out = n*coils)
  # spiral_r <- exp(spiral_b*theta)
  spiral_r <- theta * seq(1, expansion, length.out = n*coils)
  # return(spiral_r)
  # a <- 10
  # b <- 1
  # r <- a + b * theta

  rose <- tibble(x = numeric(n*coils),
                 y = numeric(n*coils))

  for(i in 1:length(theta)) {

    t <- theta[i] %% (pi*2)
    # r <- sin(n*k)
    one <- 2+sin(a*t)/2
    two <- t+sin(b*t)/c

    # r <- 1-seq(1, .1, length.out = n)^.5
    # r <- sqrt(seq(1, 0, length.out = n))
    # r <- seq(1, 0, length.out = n)
    # spiral_r <- cos(theta[i])*theta[i]
    # rose$x[i] <- one * cos(two) * r
    # rose$y[i] <- one * sin(two) * r

    # rose$x[i] <- theta[i] * one * cos(two)
    # rose$y[i] <- theta[i] * one * sin(two)

    rose$x[i] <- spiral_r[i] * one * cos(two)
    rose$y[i] <- spiral_r[i] * one * sin(two)

  }

  return(rose)

}


points <- 200
coils <- 20
w <- 4
h <- 5
set.seed(3)

rose_grid <- map_df(.x = 1:(w*h),
                    .f = ~starr(a = rand(), b = rand(), c = rand(),
                                n = points, coils = coils, expansion = 10))

rose_grid2 <- rose_grid %>%
  mutate(group = rep(1:(w*h), each = coils*points),
         xoff = rep(1:w*7000, each = coils*points, length.out = n()),
         yoff = rep(1:h*7000, each = coils*points*w, length.out = n()),
         x0 = x + xoff,
         y0 = y + yoff) %>%
  select(x = x0, y = y0, group = group)



pic <- ggplot(rose_grid2, aes(x, y, group = group)) +
  geom_path() +
  coord_fixed() +
  theme_void() +
  NULL

ggsave("rose-garden.png", width = 8, height = 10)
