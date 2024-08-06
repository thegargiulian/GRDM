
#libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2)

#load denominator file
pops <- read.csv("data/spread/2020_Brazil/brazil_denominators.csv")

#load death data
deaths <- read.csv("data/spread/2020_Brazil/brazil_excess.csv")

#merge
data <- deaths %>%
  inner_join(pops, by = "state")

#estimate proportion of population killed in pandemic
data <- data %>%
  mutate(prop_killed = (excess2020 + excess2021) / pop2019)

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
plot_2020_1 <- data %>%
  ggplot(aes(GRDM, prop_killed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              col = "red") +
  scale_x_continuous(name = NULL,
                     labels = scales::percent) +
  scale_y_continuous(name = NULL,
                     labels = scales::percent,
                     breaks = c(0.002, 0.003, 0.004),) +
  ggtitle("2020-21, Brazil") +
  labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
  theme_classic() +
  theme(plot.caption = element_text(size = 11))
