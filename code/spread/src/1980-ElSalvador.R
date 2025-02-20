#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/1980-ElSalvador.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2)

# ----- main

#load denominator file
pops <- read.csv(here::here("data/spread/1980_ElSalvador/elsalvador_denominators.csv"))

#load death data
deaths <- read.csv(here::here("data/spread/1980_ElSalvador/elsalvador_MSE.csv"))

#merge
data <- deaths %>%
    inner_join(pops, by = "department")

#estimate proportion of population killed in conflict
data <- data %>%
    mutate(prop_killed = deaths_MSE / pop1981)

#estimate GRDM population loss
data <- data %>%
    mutate(R_F = (pop1971 / pop1961) ^ (1 / 10),
           K1 = pop1971 * R_F ^ (1980 - 1971),
           P2 = pop1992,
           GRDM = (K1 - P2) / K1)

#get R^2
r2 <- data %>%
    {lm(.$GRDM ~ .$prop_killed)} %>%
    summary() %>%
    .$r.squared %>%
    signif(2)

n <- data %>%
    {lm(.$GRDM ~ .$prop_killed)} %>%
    summary() %>%
    .$df %>%
    .[2] + 2

#plot
plot_1980 <- data %>%
    ggplot(aes(GRDM, prop_killed)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                col = "red") +
    scale_x_continuous(name = NULL,
                       labels = scales::percent) +
    scale_y_continuous(name = NULL,
                       labels = scales::percent) +
    ggtitle("1980\u201392, El Sal.") +
    labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
    theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))

# done.
