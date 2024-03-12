#
# Authors:     MG, HG
# Maintainers: MG
# =========================================
# GRDM/code/method-types/src/graph-methods.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, ggplot2)

args <- list(method1 = "output/method-1.png",
             method2 = "output/method-2.png",
             method3 = "output/method-3.png",
             method4 = "output/method-4.png",
             method5 = "output/method-5.png",
             method6 = "output/method-6.png",
             method7 = "output/method-7.png")

# ----- functions


graph_method1 <- function() {

    ggplot() +
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

}


graph_method2 <- function(r = 0.025) {

    segment1 <- tibble(time = seq(0, 10, by = 0.1),
                       pop = 25 * exp(r * time))
    segment2 <- tibble(time = seq(10, 20, by = 0.1),
                       pop = 15 * exp(r * (time - 10)))

    ggplot() +
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

}


graph_method3 <- function(r = 0.03) {

    segment1 <- tibble(time = seq(0, 20, by = 0.1),
                       pop = 25 * exp(r * time))

    ggplot() +
        geom_line(data = segment1,
                  aes(y = pop, x = time),
                  linewidth = 1) +
        geom_segment(aes(x = 20, y = segment1$pop[segment1$time == 20],
                         xend = 20, yend = 23),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 3) +
        geom_point(aes(x = 20, y = 23),
                   size = 3) +
        theme_void()

}


graph_method4 <- function(r = 0.03) {

    segment1 <- tibble(time = seq(0, 7.5, by = 0.1),
                       pop = 25 * exp(r * time))
    segment2 <- tibble(time = seq(12.5, 20, by = 0.1),
                       pop = 18 * exp(r * (time - 12.5)))

    ggplot() +
        geom_line(data = segment1,
                  aes(y = pop, x = time),
                  linewidth = 1) +
        geom_line(data = segment2,
                  aes(y = pop, x = time),
                  linewidth = 1) +
        geom_segment(aes(x = 7.5, xend = 12.5,
                         y = segment1$pop[segment1$time == 7.5],
                         yend = segment2$pop[segment2$time == 12.5]),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 3) +
        geom_point(aes(x = 20, y = segment2$pop[segment2$time == 20]),
                   size = 3) +
        theme_void()

}


graph_method5 <- function(r = 0.03) {

    segment1 <- tibble(time = seq(0, 12.5, by = 0.1),
                       pop = 22 * exp(r * time))

    ggplot() +
        geom_line(data = segment1,
                  aes(x = time, y = pop),
                  linewidth = 1) +
        geom_segment(aes(x = 12.5, xend = 20,
                         y = segment1$pop[segment1$time == 12.5],
                         yend = 20),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 22),
                   size = 3) +
        geom_point(aes(x = 20, y = 20),
                   size = 3) +
        theme_void()

}


graph_method6 <- function(r = 0.03) {

    segment2 <- tibble(time = seq(0, 20, by = 0.1),
                       pop = 10 * exp(r * time))

    ggplot() +
        geom_segment(aes(x = 0, xend = 0, y = 10, yend = 25),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_line(data = segment2,
                  aes(x = time, y = pop),
                  linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 3) +
        geom_point(aes(x = 20, y = segment2$pop[segment2$time == 20]),
                   size = 3) +
        theme_void()

}


graph_method7 <- function(r = 0.03) {

    segment1 <- tibble(time = seq(0, 7, by = 0.1)) %>%
        mutate(pop = 10 * exp(r * time))

    segment2 <- tibble(time = seq(7, 14, by = 0.1)) %>%
        mutate(pop = 8 * exp(r * (time - 7)))

    segment3 <- tibble(time = seq(14, 21, by = 0.1)) %>%
        mutate(pop = 6 * exp(r * (time - 14)))

    ggplot() +
        geom_line(data = segment1,
                  aes(x = time, y = pop),
                  linewidth = 1) +
        geom_line(data = segment2,
                  aes(x = time, y = pop),
                  linewidth = 1) +
        geom_line(data = segment3,
                  aes(x = time, y = pop),
                  linewidth = 1) +
        geom_segment(aes(x = 7, xend = 7,
                         y = segment1$pop[segment1$time == 7],
                         yend = segment2$pop[segment2$time == 7]),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_segment(aes(x = 14, xend = 14,
                         y = segment2$pop[segment2$time == 14],
                         yend = segment3$pop[segment3$time == 14]),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_segment(aes(x = 21, xend = 21,
                         y = segment3$pop[segment3$time == 21],
                         yend = 4),
                     linetype = "dashed",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 10),
                   size = 3) +
        geom_point(aes(x = 7, y = 8),
                   size = 3) +
        geom_point(aes(x = 14, y = 6),
                   size = 3) +
        geom_point(aes(x = 21, y = 4),
                   size = 3) +
        theme_void()

}


# ----- main

ggsave(args$method1,
       plot = graph_method1(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

ggsave(args$method2,
       plot = graph_method2(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

ggsave(args$method3,
       plot = graph_method3(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

ggsave(args$method4,
       plot = graph_method4(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

ggsave(args$method5,
       plot = graph_method5(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

ggsave(args$method6,
       plot = graph_method6(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

ggsave(args$method7,
       plot = graph_method7(),
       width = 2.5, height = 2.5, units = "in",
       dpi = 320)

# done.
