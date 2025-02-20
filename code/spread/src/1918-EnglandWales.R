#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/1918-EnglandWales.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, tidyr, stringr, ggplot2, readxl)

# ----- main

#load denominator file
pops <- read.csv(here::here("data/spread/1918_England+Wales/1921_cr03_values.csv"))

#clean denominator file
pops <- pops %>%
    rename(pop1911 = X2c3_0002,
           pop1921 = X2c3_0003) %>%
    select(area, area_type, area_type_id, pop1911, pop1921) %>%
    mutate(area = gsub(", City of", "", area),
           area = gsub(", City and County of", "", area))

#add county to each population estimate, to deal with duplicate names
pops <- pops %>%
    mutate(county = ifelse(area_type_id %in% c(10, 11),
                           area,
                           NA)) %>%
    fill(county,
         .direction = "down")

#restrict to MB, UD, and London boroughs
pops <- pops %>%
    filter(area_type %in% c("Municipal Borough",
                            "Urban District",
                            "Metropolitan Borough",
                            "County Borough")) %>%
    mutate(area = ifelse(area_type == "Municipal Borough",
                         paste(area, "M.B."),
                         area),
           area = ifelse(area_type == "Urban District",
                         paste(area, "U.D."),
                         area),
           area = ifelse(area_type == "Metropolitan Borough",
                         paste(area, "London"),
                         area),
           area = ifelse(area_type == "County Borough",
                         paste(area, "C.B."),
                         area))

#load death data
deaths <- read_xls(here::here("data/spread/1918_England+Wales/RGdata.xls"),
                   sheet = "Raw data",
                   skip = 1,
                   col_names = TRUE)

#clean death data
deaths <- deaths %>%
    filter(!is.na(Total)) %>%
    rename(area = `Administrative unit`,
           county = `Administrative County`,
           count = Total) %>%
    mutate(area = ifelse(county == "London",
                         paste(area, "London"),
                         area),
           area = gsub(" CB", "", area),
           area = ifelse(row_number() >= 253,
                         paste(area, "C.B."),
                         area)) %>%
    select(area, county, count)

#harmonise county names
deaths <- deaths %>%
    mutate(county = word(county),
           county = gsub(",", "", county))

pops <- pops %>%
    mutate(county = word(county),
           county = gsub(",", "", county))

#merge
data <- deaths %>%
    inner_join(pops, by = c("area", "county"))

#estimate proportion of population killed in pandemic
data <- data %>%
    mutate(pop1918 = (pop1921 - pop1911) * 0.7 + pop1911,
           prop_killed = count / pop1918)

#estimate GRDM population loss
data <- data %>%
    mutate(GRDM = (pop1911 - pop1921) / pop1911)

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
plot_1918_1 <- data %>%
    ggplot(aes(GRDM, prop_killed)) +
    geom_point(alpha = 0.5, size = 1) +
    geom_smooth(method = "lm",
                se = FALSE,
                col = "red") +
    scale_x_continuous(name = NULL,
                       labels = scales::percent) +
    scale_y_continuous(name = NULL,
                       labels = scales::percent,
                       limits = c(0, NA),
                       breaks = c(0, 0.005, 0.01),) +
    ggtitle("1918\u201319, E. & W.") +
    labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
    theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))

# done.
