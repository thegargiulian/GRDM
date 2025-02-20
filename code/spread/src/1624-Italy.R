#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/1624-Italy.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2, readxl)

#load combined denominator and death file
data <- read_xlsx(here::here("data/spread/1624_Italy/Database_AlfaniAndPercoco.xlsx"),
                  sheet = "Complete(N=56,Tab1)",
                  skip = 2)

# ----- main

#clean data
data <- data %>%
    rename(prop1624 = '1624-25 (mortality rate, per thousand)',
           prop1629 = '1629-30 (mortality rate, per thousand)',
           prop1656 = '1656-57 (mortality rate, per thousand)') %>%
    mutate(across(prop1624:prop1656, ~ ifelse(.x == 'Not Affected', 0, .x)),
           across(prop1624:prop1656, ~ ifelse(.x == '722 (including siege)', 722, .x)),
           across(prop1624:prop1656, as.numeric),
           across(prop1624:prop1656, ~ .x / 1000)) %>%
    rowwise() %>%
    mutate(prop_killed = sum(prop1624, prop1629, prop1656))

#estimate GRDM population loss
data <- data %>%
    mutate(GRDM = (pop1600 - pop1700) / pop1600) %>%
    filter(!is.na(GRDM))

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
plot_1624 <- data %>%
    ggplot(aes(GRDM, prop_killed)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                col = "red") +
    scale_x_continuous(name = NULL,
                       labels = scales::percent) +
    scale_y_continuous(name = NULL,
                       labels = scales::percent) +
    ggtitle("1624\u201357, Italy") +
    labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
    theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))

# done.
