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
T_X <- 1911
X <- 315.156

T_Y <- 1921
Y <- 318.942

T_D <- 1919

R_F <- 1.005
R_B <- 1.012

birth_effect <- 0
migration_effect <- 0

# base case
K1 <- X * R_F ^ (T_D - T_X)
K2 <- Y / R_B ^ (T_Y - T_D)
P <- K1 - K2
D_C_base <- P + birth_effect + migration_effect

# state space 1: vary the census completeness
space1 <- NULL

min_completeness <- 0.92
granularity <- 100

i <- 1

for (X_completeness in seq(min_completeness, 1,
                           length.out = granularity)) {

    for (Y_completeness in seq(min_completeness, 1,
                               length.out = granularity)) {

        K1 <- (X / X_completeness) * R_F ^ (T_D - T_X)
        K2 <- (Y / Y_completeness) / R_B ^ (T_Y - T_D)
        P <- K1 - K2
        D_C <- P + birth_effect + migration_effect

        error_of_base <- D_C_base - D_C

        space1[[i]] <- cbind.data.frame(set = "Varying census completeness",
                                        X_completeness,
                                        Y_completeness,
                                        D_C,
                                        error_of_base)

        i <- i + 1

    }

}

space1 <- bind_rows(space1)

# state space 2: vary the unobserved growth rates
space2 <- NULL

min_R <- 1.0025
max_R <- 1.0150

i <- 1

for (R_F_unobserved in seq(min_R, max_R,
                           length.out = granularity)) {

    for (R_B_unobserved in seq(min_R, max_R,
                               length.out = granularity)) {

        K1 <- X * R_F_unobserved ^ (T_D - T_X)
        K2 <- Y / R_B_unobserved ^ (T_Y - T_D)
        P <- K1 - K2
        D_C <- P + birth_effect + migration_effect

        error_of_base <- D_C_base - D_C

        space2[[i]] <- cbind.data.frame(set = "Varying true growth rates",
                                        R_F_unobserved,
                                        R_B_unobserved,
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

# state space 4: apply model 3 and vary the end time
space4 <- data.frame(end_time = seq(1921, 1931, length.out = granularity))

pop1921 <- Y
pop1931 <- 352.838
growth1921to31 <- (pop1931 / pop1921) ^ (1 / 10)

space4 <- space4 %>%
    mutate(estimated_pop = pop1921 * growth1921to31 ^ (end_time - T_Y),
           K = X * R_F ^ (end_time - T_X),
           P = K - estimated_pop,
           D_C = P + birth_effect + migration_effect,
           error_of_base = D_C_base - D_C,
           set = "Varying counterfactual end point")

# visualise
high_value <- max(c(space1$D_C, space2$D_C,
                    space3$D_C, space4$D_C))
low_value <- min(c(space1$D_C, space2$D_C,
                   space3$D_C, space4$D_C))
mid_value <- D_C_base

high_colour <- met.brewer("Tam", 4)[4]
low_colour <- met.brewer("Tam", 4)[1]

f1 <- space1 %>%
    ggplot(aes(X_completeness, Y_completeness, fill = D_C)) +
    geom_tile() +
    scale_x_continuous(name = "1911 census completeness",
                       labels = scales::percent,
                       breaks = seq(min_completeness, 1, length.out = 3)) +
    scale_y_continuous(name = "1921 census completeness",
                       labels = scales::percent,
                       breaks = seq(min_completeness, 1, length.out = 3)) +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(name = "Death toll (millions)",
                         limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "bottom") +
    facet_wrap(~set)

f2 <- space2 %>%
    ggplot(aes(R_F_unobserved - 1, R_B_unobserved - 1, fill = D_C)) +
    geom_tile() +
    scale_x_continuous(name = expression("True "*R[F]),
                       labels = scales::percent) +
    scale_y_continuous(name = expression("True "*R[B]),
                       labels = scales::percent) +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "none") +
    facet_wrap(~set)

f3 <- space3 %>%
    ggplot(aes(true_birth_effect, true_migration_effect, fill = D_C)) +
    geom_tile() +
    scale_x_continuous(name = "Net effect on births (millions)") +
    scale_y_continuous(name = "Net effect on migration (millions)") +
    coord_cartesian(expand = FALSE) +
    scale_fill_gradient2(limits = c(low_value, high_value),
                         midpoint = mid_value,
                         low = low_colour,
                         high = high_colour) +
    theme_classic() +
    theme(legend.position = "none") +
    facet_wrap(~set)

f4 <- space4 %>%
    ggplot(aes(end_time, D_C, col = D_C)) +
    geom_line(linewidth = 1) +
    scale_x_continuous(name = "Counterfactual end point",
                       breaks = seq(1921, 1931, length.out = 3)) +
    scale_y_continuous(name = "Death toll (millions)") +
    scale_colour_gradient2(limits = c(low_value, high_value),
                           midpoint = mid_value,
                           low = low_colour,
                           high = high_colour) +
    theme_classic() +
    theme(legend.position = "none") +
    facet_wrap(~set)

f_legend <- get_legend(f1)

f1 <- f1 +
    theme(legend.position = "none")

full_figure <- plot_grid(plot_grid(f1, f2, f3, f4,
                                   ncol = 2,
                                   align = "hv",
                                   axis = "tblr"),
                         f_legend,
                         ncol = 1, nrow = 2, rel_heights = c(1, 0.2))

ggsave(args$output, full_figure, scale = 1.8, dpi = 500)

# done.
