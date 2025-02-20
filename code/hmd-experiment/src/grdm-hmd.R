#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/hmd-experiment/grdm-hmd.R

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(dplyr, HMDHFDplus, ggplot2)

# NOTE: you need to set your own HMD credentials here for the code below to run
us <- ""
pw <- ""

# ----- main

#get countries
pops <- getHMDcountries() %>%
  select(-link)

#exclude populations with major definition changes
pops <- pops %>%
  filter(!CNTRY %in% c("BEL", "DNK", "FRATNP", "ITA", "NZL_MA", "GBR_SCO"))

#exclude all versions of Germany, all-UK, England and Wales civilian, non-Maori NZ
pops <- pops %>%
  filter(!CNTRY %in% c("DEUTE", "DEUTW", "DEUT_NP", "GBR_NP", "GBRCENW", "NZL_NM"))

#rename GBRTENW
pops <- pops %>%
  mutate(Country = ifelse(CNTRY == "GBRTENW", "England and Wales", Country))

#add code for reconstructed all-Germany
pops <- pops %>%
  bind_rows(data.frame(Country = "Germany", CNTRY = "DEUT"))

#test predictive power of the growth rates in the decade before
#common when applying Method 2
results <- list()
i <- 1

for (pop in pops$CNTRY) {

  #load data and sum by age and sex
  if (pop != "DEUT") {

    data <- readHMDweb(CNTRY = pop,
                       item = "Population",
                       username = us,
                       password = pw) %>%
      select(Year, Age, Total1, Total2) %>%
      group_by(Year) %>%
      summarise(Total1 = sum(Total1),
                Total2 = sum(Total2))

  }

  #treat Germany differently
  if (pop == "DEUT") {

    data <- readHMDweb(CNTRY = "DEUTE",
                       item = "Population",
                       username = us,
                       password = pw) %>%
      bind_rows(readHMDweb(CNTRY = "DEUTW",
                           item = "Population",
                           username = us,
                           password = pw)) %>%
      select(Year, Age, Total1, Total2) %>%
      group_by(Year) %>%
      summarise(Total1 = sum(Total1),
                Total2 = sum(Total2))

  }

  #add the final January 1 population count
  final_year <- data.frame(Year = max(data$Year) + 1,
                           Total1 = tail(data$Total2, 1))

  data <- data %>%
    bind_rows(final_year) %>%
    select(-Total2) %>%
    rename(Pop = Total1)

  #skip if fewer than 15 years of data available
  if (max(data$Year) - min(data$Year) < 15) {next}

  #get growth rates
  for (start in min(data$Year):(max(data$Year - 15))) {

    K1 <- data %>%
      filter(Year == start) %>%
      pull(Pop)

    K2 <- data %>%
      filter(Year == start + 10) %>%
      pull(Pop)

    K3 <- data %>%
      filter(Year == start + 15) %>%
      pull(Pop)

    R_predicted <- ((K2 / K1) ^ (1/10) - 1) * 100

    R_actual <- ((K3/K2) ^ (1/5) - 1) * 100

    #save
    results[[i]] <- data.frame(pop, start, R_predicted, R_actual)
    i <- i + 1

  }

}

results <- bind_rows(results)

#get coverage years
coverage <- results %>%
  group_by(pop) %>%
  summarise(min = min(start),
            max = max(start) + 15) %>%
  mutate(range = paste0("(", min, "–", max, ")")) %>%
  select(-c("min", "max"))

#plot error by population
results <- results %>%
  mutate(abs_error = abs(R_predicted - R_actual)) %>%
  inner_join(pops, by = c('pop' = 'CNTRY')) %>%
  inner_join(coverage, by = 'pop') %>%
  mutate(pop = paste(Country, range))

summary(results$abs_error)
quantile(results$abs_error, c(0.75, 0.95))

error_order <- results %>%
  group_by(pop) %>%
  summarise(median_error = median(abs_error)) %>%
  arrange(median_error) %>%
  pull(pop)

fs1 <- results %>%
  ggplot(aes(pop, abs_error / 100)) +
  geom_boxplot() +
  theme_classic() +
  scale_x_discrete(name = 'Population and years of data used',
                   limits = error_order) +
  scale_y_continuous(name = "Absolute error of predicted vs observed\nannualised 5-year growth rate",
                     labels = scales::percent) +
  coord_flip()

ggsave(here::here("code/hmd-experiment/output/Figure-S1-hmd-growth-rates.png"), fs1,
       width = 4.9, height = 5.7, dpi = 720)

# done.
