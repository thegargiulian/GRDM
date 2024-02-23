
pacman::p_load(dplyr, ggplot2)

time <- seq(0, 50, by = 0.1)

type1 <- ggplot() +
    geom_segment(aes(x = 0, y = 50,
                     xend = 25, yend = 50),
                 linewidth = 1) +
    geom_segment(aes(x = 25, y = 50,
                     xend = 25, yend = 25),
                 linetype = "dashed",
                 linewidth = 1) +
    geom_segment(aes(x = 25, y = 25,
                     xend = 50, yend = 25),
                 linewidth = 1) +
    geom_point(aes(x = 0, y = 50),
               size = 3) +
    geom_point(aes(x = 50, y = 25),
               size = 3) +
    theme_void()

r <- 0.02

segment1 <- tibble(time = seq(0, 10, by = 0.1),
                   pop = 25 * exp(r * time))
segment2 <- tibble(time = seq(10, 20, by = 0.1),
                   pop = 20 * exp(r * (time - 10)))

type2 <- ggplot() +
    geom_line(data = segment1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = 10, y = segment1$pop[segment1$time == 10],
                     xend = 10, yend = segment2$pop[segment2$time == 10]),
                 linetype = "dashed",
                 linewidth = 1) +
    geom_point(aes(x = 0, y = 25),
               size = 3) +
    geom_point(aes(x = 20, y = segment2$pop[segment2$time == 20]),
               size = 3) +
    theme_void()



