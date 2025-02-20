#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/1918-Spain.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2)

# ----- main

#load denominator file
pops <- read.csv(here::here("data/spread/1918_Spain/spain_denominators.csv"))

#load death data
deaths <- read.csv(here::here("data/spread/1918_Spain/spain_excess.csv"))

#merge
data <- deaths %>%
    inner_join(pops, by = "province")

#estimate GRDM population loss
data <- data %>%
    mutate(R_F = (pop1910 / pop1900) ^ (1 / 10),
           R_B = (pop1930 / pop1920) ^ (1 / 10),
           K1 = pop1910 * R_F ^ (1919 - 1910),
           K2 = pop1920 / R_B ^ (1920 - 1919),
           GRDM = (K1 - K2) / K1)

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
plot_1918_2 <- data %>%
    ggplot(aes(GRDM, prop_killed)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                col = "red") +
    scale_x_continuous(name = NULL,
                       labels = scales::percent) +
    scale_y_continuous(name = NULL,
                       labels = scales::percent) +
    ggtitle("1918\u201319, Spain") +
    labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
    theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))
