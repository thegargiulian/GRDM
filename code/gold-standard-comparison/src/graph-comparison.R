#
# Authors:     HG
# Maintainers: MG
# =========================================
# GRDM/code/method-types/src/graph-comparison.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, tidyr, ggplot2, MetBrewer)

args <- list(plot = "code/gold-standard-comparison/output/Figure-Gold.png")

# ----- data

comparison_data <- read.csv("data/gold-standard-comparison/comparisons.csv")

# ----- functions

comparison_plot <- function() {
  
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
    bind_rows(expand.grid(crisis = c("1952-60, Kenya",
                                     "1975-79, Cambodia",
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
    theme_classic() +
    theme(text = element_text(size = 11),
          legend.position = "bottom") +
    coord_flip() +
    scale_x_discrete(name = NULL, limits = crisis_order) +
    scale_y_continuous(name = "Death toll estimates",
                       limits = c(30000, NA),
                       label = scales::comma,
                       trans = "log10") +
    scale_color_manual(name = "Source of\nestimate",
                       values = met.brewer("Java", 4)) +
    scale_shape_manual(name = "Source of\nestimate",
                       values = c(17, 16, 16, 16)) +
    scale_linetype_manual(name = "Type of\ninterval",
                            values = c("solid", "11")) +
    guides(color = guide_legend(nrow = 2, order = 1),
           shape = guide_legend(nrow = 2, order = 1),
           linetype = guide_legend(nrow = 2, order = 2))
  
}

# ----- main

ggsave(args$plot,
       plot = comparison_plot(),
       width = 6.5, height = 5, units = "in",
       dpi = 320)

# done.
