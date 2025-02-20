
#libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2, arrow, stringr, tidyr)

#load denominator files
pop_2000 <- read.table(here::here("data/spread/2020_US/nhgis0012_ds146_2000_county.csv"),
                       header = TRUE, sep = ',', skip = 1)
pop_2010 <- read.table(here::here("data/spread/2020_US/nhgis0012_ds172_2010_county.csv"),
                       header = TRUE, sep = ',', skip = 1)
pop_2022 <- read.csv(here::here("data/spread/2020_US/co-est2022-alldata.csv"))

#load death data
deaths <- read_feather(here::here("data/spread/2020_US/COVIDUCDMonthlyData.feather"))

#extract March 2020 denominators
pop_2020 <- deaths %>%
  filter(year == 2020, month == 3) %>%
  rename(pop2020 = pop) %>%
  select(FIPSCode, pop2020)

#merge the denominator files together
pops <- pop_2000 %>%
  bind_rows(pop_2010) %>%
  rename(TotalPop = Total) %>%
  mutate(state_pad = str_pad(State.Code, width = 2, pad = '0'),
         county_pad = str_pad(County.Code, width = 3, pad = '0'),
         FIPSCode = paste0(state_pad, county_pad)) %>%
  rename(Year = Data.File.Year) %>%
  select(Year, FIPSCode, TotalPop)

pops <- pop_2022 %>%
  filter(COUNTY != 0) %>%
  mutate(state_pad = str_pad(STATE, width = 2, pad = '0'),
         county_pad = str_pad(COUNTY, width = 3, pad = '0'),
         FIPSCode = paste0(state_pad, county_pad)) %>%
  select(FIPSCode, POPESTIMATE2022) %>%
  rename(TotalPop = POPESTIMATE2022) %>%
  mutate(Year = 2022) %>%
  bind_rows(pops) %>%
  pivot_wider(names_from = Year,
              names_prefix = 'pop',
              values_from = TotalPop) %>%
  inner_join(pop_2020, by = "FIPSCode")


#sum excess deaths for the Covid period
deaths <- deaths %>%
  mutate(pad_month = str_pad(month, width = 2, pad = '0')) %>%
  mutate(time = as.integer(paste0(year, pad_month))) %>%
  filter(time >= 202003, time <= 202202) %>%
  group_by(FIPSCode) %>%
  summarise(covid_deaths = sum(imputedCOVIDDeaths))

#merge
data <- deaths %>%
  inner_join(pops, by = "FIPSCode")

#estimate proportion of population killed in pandemic
data <- data %>%
  mutate(prop_killed = covid_deaths / pop2020)

#limit to counties with at least 1 Covid death estimated
#some counties are too small to even have any deaths imputed
data <- data %>%
  filter(prop_killed != 0)

#estimate GRDM population loss
data <- data %>%
  mutate(R_F = (pop2010 / pop2000) ^ (1 / 10),
         K1 = pop2010 * R_F ^ (2020-2010),
         P2 = pop2022,
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
plot_2020_2 <- data %>%
  ggplot(aes(GRDM, prop_killed)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm",
              se = FALSE,
              col = "red") +
  scale_x_continuous(name = NULL,
                     labels = scales::percent) +
  scale_y_continuous(name = NULL,
                     labels = scales::percent,
                     breaks = c(0, 0.005, 0.01),
                     limits = c(0, 0.01)) +
  ggtitle("2020-22, USA") +
  labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
  theme_classic() +
    theme(text = element_text(size = 9),
          plot.caption = element_text(size = 9),
          plot.title = element_text(size = 10))

# done.
