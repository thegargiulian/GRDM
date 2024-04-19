#
# Authors:     HG
# Maintainers: HG, MG
# =========================================
# GRDM/code/simulation/src/simulation.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, ggplot2, MetBrewer, cowplot)

args <- list(output = "output/grdm-section3-figure.png")

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

min_rel_R <- 0.8
max_rel_R <- 1.2

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

min_birth_effect <- -10
max_birth_effect <- 2
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

diagram <- ggplot() +
  geom_line(data = segment1,
            aes(y = pop, x = time),
            linewidth = 1) +
  geom_line(data = segment2,
            aes(y = pop, x = time),
            linewidth = 1) +
  geom_segment(aes(x = T_D, y = segment1$pop[segment1$time == T_D],
                   xend = T_D, yend = segment2$pop[segment2$time == T_D]),
               linetype = "dashed",
               linewidth = 1) +
  geom_point(aes(x = T_X, y = X),
             size = 3) +
  geom_point(aes(x = T_Y, y = Y),
             size = 3) +
  scale_x_continuous(name = NULL) +
  scale_y_continuous(name = 'Population of British India\n(millions)') +
  theme(text = element_text(size = 11)) +
  theme_classic() +
  facet_grid(~set)

# visualise state spaces
high_value <- max(c(space1$D_C, space2$D_C,
                    space3$D_C, space4$D_C))
low_value <- min(c(space1$D_C, space2$D_C,
                   space3$D_C, space4$D_C))
mid_value <- D_C_base

high_colour <- met.brewer("Ingres", 4)[4]
low_colour <- met.brewer("Ingres", 4)[1]

f1 <- space1 %>%
    ggplot(aes(X_completeness, completeness_change, fill = D_C)) +
    geom_tile() +
    scale_x_continuous(name = "1911 census completeness",
                       labels = scales::percent) +
    scale_y_continuous(name = "Change in completeness,\n1911-21",
                       labels = function(x) scales::percent(x, accuracy = 1)) +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(name = "Death toll estimate (millions)",
                         limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "bottom",
          text = element_text(size = 11)) +
    facet_wrap(~set)

f2 <- space2 %>%
    ggplot(aes(R_F_unobserved - 1, R_B_unobserved - 1, fill = D_C)) +
    geom_tile() +
    scale_x_continuous(name = "Unobserved R, 1911-18",
                       labels = scales::percent) +
    scale_y_continuous(name = "Unobserved R, 1918-21",
                       labels = scales::percent) +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 11)) +
    facet_wrap(~set)

f3 <- space3 %>%
    ggplot(aes(true_birth_effect, true_migration_effect, fill = D_C)) +
    geom_tile() +
    scale_x_continuous(name = "Net effect on births (millions)") +
    scale_y_continuous(name = "Net effect on\nmigration (millions)") +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 11)) +
    facet_wrap(~set)

space4_density <- density(space4$D_C)
space4_density <- data.frame(x = space4_density$x,
                             y = space4_density$y) %>%
  filter(x >= low_value, x <= high_value) %>%
  mutate(set = "Varying all six variables")

f4 <- space4_density %>%
    ggplot(aes(x, y)) +
    geom_segment(aes(xend = x, yend = 0, col = x)) +
    theme_classic() +
    theme(legend.position = "none",
          text = element_text(size = 11)) +
    scale_x_continuous(name = "Death toll estimate (millions)") +
    scale_y_continuous(name = "Density") +
    scale_color_gradient2(limits = c(low_value, high_value),
                        midpoint = mid_value,
                        low = low_colour,
                        high = high_colour) +
    facet_wrap(~set)

f_legend <- get_legend(f1)

f1 <- f1 +
    theme(legend.position = "none")

full_figure <- plot_grid(plot_grid(NULL, diagram, NULL,
                                   ncol = 3, rel_widths = c(0.15, 0.7, 0.15)),
                         plot_grid(f1, f2, f3, f4,
                                   ncol = 2,
                                   align = "hv",
                                   axis = "tblr"),
                         f_legend,
                         ncol = 1, nrow = 3, rel_heights = c(0.3, 0.6, 0.05))

ggsave(args$output, full_figure,
       width = 6.5, height = 8, dpi = 320)

# done.
