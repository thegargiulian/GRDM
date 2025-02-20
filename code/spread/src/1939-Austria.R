#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/1939-Austria.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2, haven)

# ----- main

#load death data
deaths <- read_dta(here::here("data/spread/1939_Austria/01_tote_ww2.dta"))

#load denominator data
pops <- read_dta(here::here("data/spread/1939_Austria/gem_hist_einwohner.dta"))

#load municipality merger mapping
map <- read_dta(here::here("data/spread/1939_Austria/01_gem_timeline_1939-2011.dta"))

#get municipalities without mergers
ids_2011_without_mergers <- map %>%
    group_by(gem_id_2013) %>%
    summarise(count = n()) %>%
    filter(count == 1) %>%
    pull(gem_id_2013)

pops <- pops %>%
    filter(gem_id_2011 %in% ids_2011_without_mergers)

#merge
data <- deaths %>%
    inner_join(pops, by = c("gem_id_2013" = "gem_id_2011"))

#estimate GRDM population loss
data <- data %>%
    select(gem_id_2013, gem_name_2013, einwohner_1939,
           einwohner_1951, einwohner_1961, tote_ww2) %>%
    rename(pop1939 = einwohner_1939,
           pop1951 = einwohner_1951,
           pop1961 = einwohner_1961) %>%
    mutate(R_B = (pop1961 / pop1951) ^ (1 / 10),
           K2 = pop1951 / R_B ^ (1951 - 1945),
           GRDM = (pop1939 - K2) / pop1939)

#omit Kematen in Tirol
#because it grew from 708 in 1939 to 2180 in 1951
data <- data %>%
    filter(gem_id_2013 != 70320)

#get casualties as proportion of 1939 census population
data <- data %>%
    mutate(prop_killed = tote_ww2 / pop1939)

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
plot_1939 <- data %>%
    ggplot(aes(GRDM, prop_killed)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                col = "red") +
    scale_x_continuous(name = NULL,
                       labels = scales::percent) +
    scale_y_continuous(name = NULL,
                       labels = scales::percent) +
    ggtitle("1939\u201345, Austria") +
    labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
    theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))

# done.
