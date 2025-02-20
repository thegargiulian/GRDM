#
# Authors:     HG
# Maintainers: HG, MG
# =========================================
# GRDM/code/simulation/src/simulation.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, ggplot2, MetBrewer, cowplot, pBrackets)

args <- list(output = here::here("code/simulation/output/Figure-2-simulation.png"))

# ----- main

# base values
T_X <- 1911.5
X <- 315.156

T_Y <- 1921.5
Y <- 318.942

T_D <- 1918.9

#average all South Asia 1901-11
R_F <- 1.0068

#average all South Asia 1921-31
R_B <- 1.0102

birth_effect <- 0
migration_effect <- 0

# base case
K1 <- X * R_F ^ (T_D - T_X)
K2 <- Y / R_B ^ (T_Y - T_D)
P <- K1 - K2
D_C_base <- P + birth_effect + migration_effect

# state space 1: vary the census completeness
space1 <- NULL

min_completeness_1911 <- 0.90
max_completeness_1911 <- 0.95

min_completeness_change <- -0.02
max_completeness_change <- 0.02

granularity <- 100

i <- 1

for (X_completeness in seq(min_completeness_1911, max_completeness_1911,
                           length.out = granularity)) {

    for (completeness_change in seq(min_completeness_change, max_completeness_change,
                                    length.out = granularity)) {

        Y_completeness <- X_completeness + completeness_change

        K1 <- (X / X_completeness) * R_F ^ (T_D - T_X)
        K2 <- (Y / Y_completeness) / R_B ^ (T_Y - T_D)
        P <- K1 - K2
        D_C <- P + birth_effect + migration_effect

        error_of_base <- D_C_base - D_C

        space1[[i]] <- cbind.data.frame(set = "Varying census completeness",
                                        X_completeness,
                                        Y_completeness,
                                        completeness_change,
                                        D_C,
                                        error_of_base)

        i <- i + 1

    }

}

space1 <- bind_rows(space1)

# state space 2: vary the unobserved growth rates
space2 <- NULL

min_rel_R <- 0.75
max_rel_R <- 1.25

i <- 1

for (rel_R_F in seq(min_rel_R, max_rel_R,
                    length.out = granularity)) {

    for (rel_R_B in seq(min_rel_R, max_rel_R,
                        length.out = granularity)) {

        R_F_unobserved <- (R_F - 1) * rel_R_F + 1
        R_B_unobserved <- (R_B - 1) * rel_R_B + 1

        K1 <- X * R_F_unobserved ^ (T_D - T_X)
        K2 <- Y / R_B_unobserved ^ (T_Y - T_D)
        P <- K1 - K2
        D_C <- P + birth_effect + migration_effect

        error_of_base <- D_C_base - D_C

        space2[[i]] <- cbind.data.frame(set = "Varying true growth rates",
                                        R_F_unobserved,
                                        R_B_unobserved,
                                        rel_R_F,
                                        rel_R_B,
                                        D_C,
                                        error_of_base)

        i <- i + 1

    }

}

space2 <- bind_rows(space2)

# state space 3: vary the fertility and migration components
space3 <- NULL

min_birth_effect <- -8
max_birth_effect <- 1
min_migration_effect <- -1
max_migration_effect <- 1

i <- 1

for (true_birth_effect in seq(min_birth_effect, max_birth_effect,
                              length.out = granularity)) {

    for (true_migration_effect in seq(min_migration_effect, max_migration_effect,
                                      length.out = granularity)) {

        K1 <- X * R_F ^ (T_D - T_X)
        K2 <- Y / R_B ^ (T_Y - T_D)
        P <- K1 - K2
        D_C <- P + true_birth_effect + true_migration_effect

        error_of_base <- D_C_base - D_C

        space3[[i]] <- cbind.data.frame(set = "Varying fertility and migration effects",
                                        true_birth_effect,
                                        true_migration_effect,
                                        D_C,
                                        error_of_base)

        i <- i + 1

    }

}

space3 <- bind_rows(space3)

# state space 4: vary all six of the variables in the previous simulations
granularity <- 5
space4 <- expand.grid(set = "Varying all six variables",
                      X_completeness = seq(min_completeness_1911,
                                           max_completeness_1911,
                                           length.out = granularity),
                      completeness_change = seq(min_completeness_change,
                                                max_completeness_change,
                                                length.out = granularity),
                      rel_R_F = seq(min_rel_R,
                                    max_rel_R,
                                    length.out = granularity),
                      rel_R_B = seq(min_rel_R,
                                    max_rel_R,
                                    length.out = granularity),
                      true_birth_effect = seq(min_birth_effect,
                                              max_birth_effect,
                                              length.out = granularity),
                      true_migration_effect = seq(min_migration_effect,
                                                  max_migration_effect,
                                                  length.out = granularity))

i <- 1

for (i in 1:nrow(space4)) {

    X_completeness <- space4$X_completeness[i]
    completeness_change <- space4$completeness_change[i]
    Y_completeness <- X_completeness + completeness_change

    rel_R_F <- space4$rel_R_F[i]
    rel_R_B <- space4$rel_R_B[i]
    R_F_unobserved <- (R_F - 1) * rel_R_F + 1
    R_B_unobserved <- (R_B - 1) * rel_R_B + 1

    birth_effect <- space4$true_birth_effect[i]
    migration_effect <- space4$true_migration_effect[i]

    K1 <- (X / X_completeness) * R_F_unobserved ^ (T_D - T_X)
    K2 <- (Y / Y_completeness) / R_B_unobserved ^ (T_Y - T_D)

    P <- K1 - K2
    D_C <- P + birth_effect + migration_effect

    space4$D_C[i] <- D_C

    i <- i + 1

}

# add base case diagram
segment1 <- tibble(time = seq(T_X, T_D, by = 0.1),
                   pop = X * R_F ^ (time - T_X),
                   set = "Base case")
segment2 <- tibble(time = seq(T_D, T_Y, by = 0.1),
                   pop = Y / R_B ^ (T_Y - time))

base_diagram <- ggplot() +
    geom_line(data = segment1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D, y = segment1$pop[segment1$time == T_D],
                     xend = T_D, yend = segment2$pop[segment2$time == T_D]),
                 linetype = "solid",
                 col = "red",
                 linewidth = 0.5) +
    geom_point(aes(x = T_X, y = X),
               size = 2) +
    geom_point(aes(x = T_Y, y = Y),
               size = 2) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = 'Population of British\nIndia (millions)') +
    theme(text = element_text(size = 9)) +
    theme_classic() +
    facet_grid(~set)

# visualise state spaces
high_value <- max(c(space1$D_C, space2$D_C, space3$D_C))
low_value <- min(c(space1$D_C, space2$D_C, space3$D_C))
mid_value <- D_C_base

high_colour <- met.brewer("Ingres", 4)[4]
low_colour <- met.brewer("Ingres", 4)[1]

f1 <- space1 %>%
    ggplot(aes(X_completeness, completeness_change, fill = D_C)) +
    geom_raster() +
    geom_contour(aes(z = D_C),
                 breaks = D_C_base, col = "white", linewidth = 0.5) +
    geom_segment(aes(x = 0.9, xend = 0.95,
                     y = -0.005965472, yend = -0.003158882,
                     col = "20.7"), linewidth = 0.5) +
    scale_x_continuous(name = "1911 census completeness",
                       labels = scales::percent) +
    scale_y_continuous(name = "Completeness\nchange, 1911\u201321",
                       labels = function(x) scales::percent(x, accuracy = 1)) +
    scale_color_manual(name = "Base case death toll (millions)",
                       values = "red") +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(name = "Range of death toll estimates (millions)",
                         limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.box = "vertical",
          text = element_text(size = 9),
          legend.margin = margin(-5, 0, 0, 0)) +
    guides(colour = guide_legend(order = 1)) +
    facet_wrap(~set)

f2 <- space2 %>%
    ggplot(aes(R_F_unobserved - 1, R_B_unobserved - 1, fill = D_C)) +
    geom_raster() +
    geom_contour(aes(z = D_C),
                 breaks = D_C_base, col = "red", linewidth = 0.5) +
    scale_x_continuous(name = "Unobserved R, 1911\u201318",
                       labels = scales::percent) +
    scale_y_continuous(name = "Unobserved R,\n1918\u201321",
                       labels = scales::percent,
                       breaks = c(0.008, 0.01, 0.012)) +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 9)) +
    facet_wrap(~set)

f3 <- space3 %>%
    ggplot(aes(true_birth_effect, true_migration_effect, fill = D_C)) +
    geom_raster() +
    geom_contour(aes(z = D_C),
                 breaks = D_C_base, col = "red", linewidth = 0.5) +
    scale_x_continuous(name = "Net effect on births (millions)") +
    scale_y_continuous(name = "Net effect on\nmigration (millions)") +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 9)) +
    facet_wrap(~set)

# add the census incompleteness diagram
jitter_value <- 0.1

segment1_1 <- tibble(time = seq(T_X, T_D, by = 0.1),
                     pop = X * R_F ^ (time - T_X),
                     set = "Varying census completeness")
segment2_1 <- tibble(time = seq(T_D, T_Y, by = 0.1),
                     pop = Y / R_B ^ (T_Y - time))

segment1_max1 <- tibble(time = seq(T_X, T_D + jitter_value, by = 0.1),
                        pop = (X / 0.90) * R_F ^ (time - T_X))
segment2_max1 <- tibble(time = seq(T_D + jitter_value, T_Y, by = 0.1),
                        pop = (Y / 0.92) / R_B ^ (T_Y - time))

segment1_min1 <- tibble(time = seq(T_X, T_D - jitter_value, by = 0.1),
                        pop = (X / 0.95) * R_F ^ (time - T_X))
segment2_min1 <- tibble(time = seq(T_D - jitter_value, T_Y, by = 0.1),
                        pop = (Y / 0.93) / R_B ^ (T_Y - time))

census_min_color <- colorRamp(c("white", low_colour)) %>%
    {.((min(space1$D_C) - D_C_base) / (low_value - D_C_base))} %>%
    rgb(maxColorValue = 255)

census_max_color <- colorRamp(c("white", high_colour)) %>%
    {.((max(space1$D_C) - D_C_base) / (high_value - D_C_base))} %>%
    rgb(maxColorValue = 255)

census_diagram <- ggplot() +
    #base case
    geom_line(data = segment1_1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D, y = segment1$pop[segment1$time == T_D],
                     xend = T_D, yend = segment2$pop[segment2$time == T_D]),
                 linetype = "solid",
                 col = "red",
                 linewidth = 0.5) +
    geom_point(aes(x = T_X, y = X),
               size = 2) +
    geom_point(aes(x = T_Y, y = Y),
               size = 2) +
    #max loss case
    geom_line(data = segment1_max1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_max1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D + jitter_value,
                     y = segment1_max1$pop[round(segment1_max1$time, 1) == round(T_D + jitter_value, 1)],
                     xend = T_D + jitter_value,
                     yend = segment2_max1$pop[round(segment2_max1$time, 1) == round(T_D + jitter_value, 1)]),
                 linetype = "solid",
                 col = census_max_color,
                 linewidth = 0.5) +
    geom_point(aes(x = T_X, y = (X / 0.9)),
               size = 2) +
    geom_point(aes(x = T_Y, y = (Y / 0.92)),
               size = 2) +
    #min loss case
    geom_line(data = segment1_min1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_min1,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D - jitter_value,
                     y = segment1_min1$pop[round(segment1_min1$time, 1) == round(T_D - jitter_value, 1)],
                     xend = T_D - jitter_value,
                     yend = segment2_min1$pop[round(segment2_min1$time, 1) == round(T_D - jitter_value, 1)]),
                 linetype = "solid",
                 col = census_min_color,
                 linewidth = 0.5) +
    geom_point(aes(x = T_X, y = (X / 0.95)),
               size = 2) +
    geom_point(aes(x = T_Y, y = (Y / 0.93)),
               size = 2) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = NULL,
                       breaks = c(310, 340, 370)) +
    theme_classic() +
    theme(text = element_text(size = 9)) +
    facet_wrap(~set)


# add the growth rate uncertainty diagram
jitter_value <- 0.1

segment1_2 <- tibble(time = seq(T_X, T_D, by = 0.1),
                     pop = X * R_F ^ (time - T_X),
                     set = "Varying true growth rate")
segment2_2 <- tibble(time = seq(T_D, T_Y, by = 0.1),
                     pop = Y / R_B ^ (T_Y - time))

segment1_max2 <- tibble(time = seq(T_X, T_D + jitter_value, by = 0.1),
                        pop = X * ((R_F - 1) * 1.25 + 1) ^ (time - T_X))
segment2_max2 <- tibble(time = seq(T_D + jitter_value, T_Y, by = 0.1),
                        pop = Y / ((R_B - 1) * 1.25 + 1) ^ (T_Y - time))

segment1_min2 <- tibble(time = seq(T_X, T_D - jitter_value, by = 0.1),
                        pop = X * ((R_F - 1) * 0.75 + 1) ^ (time - T_X))
segment2_min2 <- tibble(time = seq(T_D - jitter_value, T_Y, by = 0.1),
                        pop = Y / ((R_F - 1) * 0.75 + 1) ^ (T_Y - time))

growth_min_color <- colorRamp(c("white", low_colour)) %>%
    {.((min(space2$D_C) - D_C_base) / (low_value - D_C_base))} %>%
    rgb(maxColorValue = 255)

growth_max_color <- colorRamp(c("white", high_colour)) %>%
    {.((max(space2$D_C) - D_C_base) / (high_value - D_C_base))} %>%
    rgb(maxColorValue = 255)

growth_diagram <- ggplot() +
    #base case
    geom_line(data = segment1_2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D, y = segment1$pop[segment1$time == T_D],
                     xend = T_D, yend = segment2$pop[segment2$time == T_D]),
                 linetype = "solid",
                 col = "red",
                 linewidth = 0.5) +
    geom_point(aes(x = T_X, y = X),
               size = 2) +
    geom_point(aes(x = T_Y, y = Y),
               size = 2) +
    #max loss case
    geom_line(data = segment1_max2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_max2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D + jitter_value,
                     y = segment1_max2$pop[round(segment1_max2$time, 1) == round(T_D + jitter_value, 1)],
                     xend = T_D + jitter_value,
                     yend = segment2_max2$pop[round(segment2_max2$time, 1) == round(T_D + jitter_value, 1)]),
                 linetype = "solid",
                 col = growth_max_color,
                 linewidth = 0.5) +
    #min loss case
    geom_line(data = segment1_min2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_min2,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D - jitter_value,
                     y = segment1_min2$pop[round(segment1_min2$time, 1) == round(T_D - jitter_value, 1)],
                     xend = T_D - jitter_value,
                     yend = segment2_min2$pop[round(segment2_min2$time, 1) == round(T_D - jitter_value, 1)]),
                 linetype = "solid",
                 col = growth_min_color,
                 linewidth = 0.5) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = 'Population of British\nIndia (millions)') +
    theme_classic() +
    facet_wrap(~set) +
    theme(text = element_text(size = 9))


# add fertility and migration effect diagram
segment1_3 <- tibble(time = seq(T_X, T_D, by = 0.1),
                     pop = X * R_F ^ (time - T_X),
                     set = "Varying fertility and migration effects")
segment2_3 <- tibble(time = seq(T_D, T_Y, by = 0.1),
                     pop = Y / R_B ^ (T_Y - time))

fertility_migration_min_color <- colorRamp(c("white", low_colour)) %>%
    {.((min(space3$D_C) - D_C_base) / (low_value - D_C_base))} %>%
    rgb(maxColorValue = 255)

fertility_migration_diagram <- ggplot() +
    geom_line(data = segment1_3,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_line(data = segment2_3,
              aes(y = pop, x = time),
              linewidth = 1) +
    geom_segment(aes(x = T_D, y = segment1$pop[segment1$time == T_D],
                     xend = T_D, yend = segment2$pop[segment2$time == T_D] + 9),
                 linetype = "solid",
                 col = fertility_migration_min_color,
                 linewidth = 0.5) +
    geom_segment(aes(x = T_D, y = segment2$pop[segment2$time == T_D] + 9,
                     xend = T_D, yend = segment2$pop[segment2$time == T_D]),
                 linetype = "dotted",
                 col = "red",
                 linewidth = 0.5) +
    geom_segment(aes(x = T_D - 0.3, y = segment2$pop[segment2$time == T_D] + 9,
                     xend = T_D + 0.3, yend = segment2$pop[segment2$time == T_D] + 9),
                 linewidth = 1) +
    annotate("text", x = 1916.8, y = segment2$pop[segment2$time == T_D] + 4.5,
             lineheight = 0.8,
             label = "Max. fertility plus\nmigration effect", size = 2) +
    annotate("text", x = 1920.5, y = segment1$pop[segment1$time == T_D] - min(space3$D_C) / 2,
             lineheight = 0.8,
             label = "Min. death\ntoll estimate", size = 2) +
    geom_point(aes(x = T_X, y = X),
               size = 2) +
    geom_point(aes(x = T_Y, y = Y),
               size = 2) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(name = NULL,
                       breaks = c(310, 320, 330)) +
    theme_classic() +
    theme(text = element_text(size = 9)) +
    facet_grid(~set)


# get the full figure

f_legend <- get_legend(f1)

f1 <- f1 +
    theme(legend.position = "none")

full_figure <- plot_grid(plot_grid(NULL, base_diagram, NULL,
                                   ncol = 3, rel_widths = c(0.15, 0.7, 0.15)),
                         plot_grid(census_diagram, f1,
                                   growth_diagram, f2,
                                   fertility_migration_diagram, f3,
                                   ncol = 2,
                                   align = "vh"),
                         f_legend,
                         ncol = 1, nrow = 3, rel_heights = c(0.25, 0.6, 0.1),
                         scale = 0.99)

ggsave(args$output, full_figure,
       width = 4.9, height = 5.7, dpi = 720)

# done.
