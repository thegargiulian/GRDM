#
# Authors:     MG, HG
# Maintainers: MG
# =========================================
# GRDM/code/method-types/src/graph-methods.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, ggplot2, patchwork, ggpubr)

args <- list(method1 = here::here("code/method-types/output/method-1.png"),
             method2 = here::here("code/method-types/output/method-2.png"),
             method3 = here::here("code/method-types/output/method-3.png"),
             method4 = here::here("code/method-types/output/method-4.png"),
             method5 = here::here("code/method-types/output/method-5.png"),
             method6 = here::here("code/method-types/output/method-6.png"),
             method7 = here::here("code/method-types/output/method-7.png"),
             figure = here::here("code/method-types/output/Figure-1-method-typology.png"))

# ----- functions


graph_method1 <- function() {

    lineA <- data.frame(x = 0, y = 50, xend = 25, yend = 50,
                        linetype = "1", col = "1")
    lineB <- data.frame(x = 25, y = 50, xend = 25, yend = 25,
                        linetype = "2", col = "2")
    lineC <- data.frame(x = 25, y = 25, xend = 50, yend = 25,
                        linetype = "1", col = "1")
    lines <- rbind.data.frame(lineA, lineB, lineC)

    pointA <- data.frame(x = 0, y = 50, col = "3")
    pointB <- data.frame(x = 50, y = 25, col = "3")
    points <- rbind.data.frame(pointA, pointB)

    ggplot() +
        geom_segment(data = lines,
                     aes(x = x, y = y, xend = xend, yend = yend,
                         col = col, linetype = linetype),
                     linewidth = 1) +
        geom_point(data = points,
                   aes(x = x, y = y, fill = col),
                   size = 2) +
        scale_color_manual(name = NULL,
                           labels = c("Extrapolated population\nchange",
                                      "Estimated population\nloss"),
                           values = c("black", "red")) +
        scale_linetype_manual(name = NULL,
                              labels = c("Extrapolated population\nchange",
                                         "Estimated population\nloss"),
                              values = c("solid", "dashed")) +
        scale_fill_manual(name = NULL,
                          values = c("black"),
                          labels = "Population estimate") +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0)),
              legend.text = element_text(size = 11),
              legend.margin = margin(-12, 0, 0, 12)) +
        ggtitle("Method 1") +
        theme(legend.position = "none")

}

graph_legend <- function() {

    plot_w_legend <- graph_method1() +
        theme(legend.position = "right",
              legend.key.size = unit(1.1, "cm"))

    plot_w_legend %>%
        get_legend() %>%
        as_ggplot()

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
                     col = "red",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 2) +
        geom_point(aes(x = 20, y = segment2$pop[segment2$time == 20]),
                   size = 2) +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0))) +
        ggtitle("Method 2")

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
                     col = "red",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 2) +
        geom_point(aes(x = 20, y = 23),
                   size = 2) +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0))) +
        ggtitle("Method 3")

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
                     col = "red",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 2) +
        geom_point(aes(x = 20, y = segment2$pop[segment2$time == 20]),
                   size = 2) +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0))) +
        ggtitle("Method 4")

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
                     col = "red",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 22),
                   size = 2) +
        geom_point(aes(x = 20, y = 20),
                   size = 2) +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0))) +
        ggtitle("Method 5")

}


graph_method6 <- function(r = 0.03) {

    segment2 <- tibble(time = seq(0, 20, by = 0.1),
                       pop = 10 * exp(r * time))

    ggplot() +
        geom_segment(aes(x = 0, xend = 0, y = 10, yend = 25),
                     linetype = "dashed",
                     col = "red",
                     linewidth = 1) +
        geom_line(data = segment2,
                  aes(x = time, y = pop),
                  linewidth = 1) +
        geom_point(aes(x = 0, y = 25),
                   size = 2) +
        geom_point(aes(x = 20, y = segment2$pop[segment2$time == 20]),
                   size = 2) +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0))) +
        ggtitle("Method 6")

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
                     col = "red",
                     linewidth = 1) +
        geom_segment(aes(x = 14, xend = 14,
                         y = segment2$pop[segment2$time == 14],
                         yend = segment3$pop[segment3$time == 14]),
                     linetype = "dashed",
                     col = "red",
                     linewidth = 1) +
        geom_segment(aes(x = 21, xend = 21,
                         y = segment3$pop[segment3$time == 21],
                         yend = 4),
                     linetype = "dashed",
                     col = "red",
                     linewidth = 1) +
        geom_point(aes(x = 0, y = 10),
                   size = 2) +
        geom_point(aes(x = 7, y = 8),
                   size = 2) +
        geom_point(aes(x = 14, y = 6),
                   size = 2) +
        geom_point(aes(x = 21, y = 4),
                   size = 2) +
        theme_void() +
        theme(plot.title = element_text(face = "bold",
                                        margin = margin(10, 0, 10, 0))) +
        ggtitle("Method 7")

}

combinded_plot <- function() {

    graph_method1() +
        theme(plot.title = element_text(margin = margin(-10, 0, 10, 0))) +
        graph_method2() +
        theme(plot.title = element_text(margin = margin(-10, 0, 10, 0))) +
        graph_method3() + graph_method4() +
        graph_method5() + graph_method6() +
        graph_method7() + graph_legend() +
        plot_layout(nrow = 4) &
        theme(plot.tag = element_text(face = "bold"),
              plot.margin = margin(t = 0.1, unit = "in"))

}


# ----- main

ggsave(args$method1,
       plot = graph_method1(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$method2,
       plot = graph_method2(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$method3,
       plot = graph_method3(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$method4,
       plot = graph_method4(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$method5,
       plot = graph_method5(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$method6,
       plot = graph_method6(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$method7,
       plot = graph_method7(),
       width = 3.25, height = 2, units = "in",
       dpi = 720)

ggsave(args$figure,
       plot = combinded_plot(),
       width = 4.9, height = 5.7, units = "in",
       dpi = 720)

# done.
