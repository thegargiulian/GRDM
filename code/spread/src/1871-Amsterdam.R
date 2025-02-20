#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/1871-Amsterdam.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, here, ggplot2, readxl, tidyr)

# ----- main

#load denominator file
pops_file <- here::here("data/spread/1871_Amsterdam/population_by_neighborhood.xlsx")
pops_1 <- read_xlsx(pops_file, sheet = "1859") %>%
    select(neighborhood, sum) %>%
    mutate(year = 1859)
pops_2 <- read_xlsx(pops_file, sheet = "1869") %>%
    select(neighborhood, sum) %>%
    mutate(year = 1869)
pops_3 <- read_xlsx(pops_file, sheet = "1879") %>%
    select(neighborhood, sum) %>%
    mutate(year = 1879)
pops_4 <- read_xlsx(pops_file, sheet = "1889") %>%
    select(neighborhood, sum) %>%
    mutate(year = 1889)

pops <- rbind.data.frame(pops_1, pops_2, pops_3, pops_4)
pops <- pops %>% pivot_wider(names_from = year,
                             names_prefix = "pop",
                             values_from = sum)

#load death data
deaths <- read_xlsx(here::here("data/spread/1871_Amsterdam/Smallpox_1871_Amsterdam_neighborhood.xlsx"))

#merge
data <- pops %>%
    inner_join(deaths, by = c("neighborhood" = "buurt_thuis")) %>%
    rename("deaths" = "CountOftblPersoonID")

#get smallpox deaths as proportion of 1869 census population
data <- data %>%
    mutate(prop_killed = deaths / pop1869)

#estimate GRDM population loss
data <- data %>%
    mutate(R_F = (pop1869 / pop1859) ^ (1 / 10),
           R_B = (pop1889 / pop1879) ^ (1 / 10),
           K1 = pop1869 * R_F ^ (1871 - 1869),
           K2 = pop1879 / R_B ^ (1879 - 1871),
           GRDM = (K1 - K2) / K1)

#omit neighbourhood YY (De Pijp)
#because it grew from 1401 in 1869 to 16165 in 1879
data <- data %>%
    filter(neighborhood != "YY")

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
plot_1871 <- data %>%
    ggplot(aes(GRDM, prop_killed)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                col = "red") +
    scale_x_continuous(name = NULL,
                       labels = scales::percent,
                       breaks = c(-0.5, 0, 0.5)) +
    scale_y_continuous(name = NULL,
                       labels = scales::percent) +
    ggtitle("1870\u201372, Ams.") +
    labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
    theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))

# done.

