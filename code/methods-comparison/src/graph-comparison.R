#
# Authors:     HG
# Maintainers: MG
# =========================================
# GRDM/code/method-types/src/graph-comparison.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(here, dplyr, tidyr, ggplot2, MetBrewer, stringr)

args <- list(plot_absolute = here::here("code/methods-comparison/output/Figure-S3-Comparison-Absolute.png"),
             plot_relative = here::here("code/methods-comparison/output/Figure-3-Comparison-Relative.png"))

# ----- data

comparison_data <- read.csv(here::here("data/gold-standard-comparison/comparisons.csv"))
denominator_data <- read.csv(here::here("data/gold-standard-comparison/denominators.csv"))

# ----- functions

absolute_comparison <- function() {

    crisis_order <- comparison_data %>%
        arrange(value) %>%
        pull(crisis) %>%
        unique()

    comparison_points <- comparison_data %>%
        filter(type == "point_estimate")

    comparison_lines <- comparison_data %>%
        filter(type == "bound") %>%
        pivot_wider(names_from = bound)

    #these are fake (and omitted in the output) but are needed to format the plot properly
    comparison_lines <- comparison_lines %>%
        bind_rows(expand.grid(crisis = c("1975-79, Cambodia",
                                         "1975-79, Timor-Leste",
                                         "1992-95, Bosnia"),
                              source = "GRDM",
                              specificity = "Rough range",
                              lower = 9000,
                              upper = 11000))

    source_order <- c("GRDM", "MSE", "Excess", "Cohort component")

    comparison_points$source <- factor(comparison_points$source,
                                       levels = source_order)

    comparison_lines$source <- factor(comparison_lines$source,
                                      levels = source_order)

    comparison_lines$specificity <- factor(comparison_lines$specificity,
                                           levels = c("95% uncertainty",
                                                      "Rough range"))


    ggplot() +
        geom_point(data = comparison_points,
                   aes(x = crisis, y = value, col = source,
                       shape = source, group = quality),
                   position = position_dodge(width = 0.4)) +
        geom_errorbar(data = comparison_lines,
                      aes(x = crisis, ymin = lower, ymax = upper,
                          col = source, group = quality, linetype = specificity),
                      position = position_dodge(width = 0.4), width = 0.4) +
        geom_vline(xintercept = 1:8, col = "gray80", linetype = "dotted") +
        theme_classic() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              legend.justification = c(0.95, 1)) +
        coord_flip() +
        scale_x_discrete(name = NULL, limits = crisis_order) +
        scale_y_continuous(name = "Death toll estimates",
                           limits = c(30000, NA),
                           label = scales::comma,
                           trans = "log10") +
        scale_color_manual(name = "Source of\nestimate",
                           values = met.brewer("Java", 4),
                           labels = function(x) str_wrap(x, width = 8)) +
        scale_shape_manual(name = "Source of\nestimate",
                           values = c(17, 16, 16, 16),
                           labels = function(x) str_wrap(x, width = 8)) +
        scale_linetype_manual(name = "Type of\ninterval",
                              values = c("solid", "11")) +
        guides(color = guide_legend(nrow = 2, order = 1),
               shape = guide_legend(nrow = 2, order = 1),
               linetype = guide_legend(nrow = 2, order = 2))

}

relative_comparison <- function() {

    relative_comparison_data <- comparison_data %>%
        full_join(denominator_data, by = 'crisis') %>%
        mutate(prop = value / denominator)

    crisis_order <- relative_comparison_data %>%
        arrange(prop) %>%
        pull(crisis) %>%
        unique()

    comparison_points <- relative_comparison_data %>%
        filter(type == "point_estimate")

    comparison_lines <- relative_comparison_data %>%
        filter(type == "bound") %>%
        pivot_wider(id_cols = c("crisis", "reference.x", "quality", "source", "specificity"),
                    names_from = bound, values_from = prop)

    #these are fake (and omitted in the output) but are needed to format the plot properly
    comparison_lines <- comparison_lines %>%
        bind_rows(expand.grid(crisis = c("1975-79, Cambodia",
                                         "1975-79, Timor-Leste",
                                         "1992-95, Bosnia"),
                              source = "GRDM",
                              specificity = "Rough range",
                              lower = 0.001,
                              upper = 0.002))

    source_order <- c("GRDM", "MSE", "Excess", "Cohort component")

    comparison_points$source <- factor(comparison_points$source,
                                       levels = source_order)

    comparison_lines$source <- factor(comparison_lines$source,
                                      levels = source_order)

    comparison_lines$specificity <- factor(comparison_lines$specificity,
                                           levels = c("95% uncertainty",
                                                      "Rough range"))


    ggplot() +
        geom_point(data = comparison_points,
                   aes(x = crisis, y = prop, col = source,
                       shape = source, group = quality),
                   position = position_dodge(width = 0.4)) +
        geom_errorbar(data = comparison_lines,
                      aes(x = crisis, ymin = lower, ymax = upper,
                          col = source, group = quality, linetype = specificity),
                      position = position_dodge(width = 0.4), width = 0.4) +
        geom_vline(xintercept = 1:8, col = "gray80", linetype = "dotted") +
        theme_classic() +
        theme(text = element_text(size = 11),
              legend.position = "bottom",
              legend.justification = c(0.95, 1)) +
        coord_flip() +
        scale_x_discrete(name = NULL, limits = crisis_order) +
        scale_y_continuous(name = "Death toll estimates (% of pre-crisis population)",
                           limits = c(0.004, NA),
                           breaks = c(0.005, 0.01, 0.03, 0.1, 0.3, 0.5),
                           labels = c("0.5%", "1%", "3%", "10%", "30%", "50%"),
                           trans = "log10") +
        scale_color_manual(name = "Source of\nestimate",
                           values = met.brewer("Java", 4),
                           labels = function(x) str_wrap(x, width = 8)) +
        scale_shape_manual(name = "Source of\nestimate",
                           values = c(17, 16, 16, 16),
                           labels = function(x) str_wrap(x, width = 8)) +
        scale_linetype_manual(name = "Type of\ninterval",
                              values = c("solid", "11")) +
        guides(color = guide_legend(nrow = 2, order = 1),
               shape = guide_legend(nrow = 2, order = 1),
               linetype = guide_legend(nrow = 2, order = 2))

}

# ----- main

ggsave(args$plot_absolute,
       plot = absolute_comparison(),
       width = 4.9, height = 4, units = "in",
       dpi = 720)

ggsave(args$plot_relative,
       plot = relative_comparison(),
       width = 4.9, height = 4, units = "in",
       dpi = 720)

# done.
