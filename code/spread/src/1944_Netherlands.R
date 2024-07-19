
#libraries
if (!require(pacman)) {install.packages("pacman")}
pacman::p_load(dplyr, ggplot2, readxl)

#load combined denominator and death file
data <- read_xlsx("data/spread/1944_Netherlands/NL-excess_mortality-1944-1945-municipalities.xlsx")

#estimate proportion of population killed in pandemic
data <- data %>%
  mutate(prop_killed = exc_deaths / pop1944)

#estimate GRDM population loss
data <- data %>%
  mutate(GRDM = (pop1944 - pop1946) / pop1944)

#limit to counties with at least 1 excess death in the famine period estimated
#some counties are too small for excess to be estimated
data <- data %>%
  filter(prop_killed != 0)

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
plot_1944 <- data %>%
  ggplot(aes(GRDM, prop_killed)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm",
              se = FALSE,
              col = "red") +
  scale_x_continuous(name = NULL,
                     labels = scales::percent) +
  scale_y_continuous(name = NULL,
                     labels = scales::percent,
                     limits = c(0, NA)) +
  ggtitle("1944-45, Netherl.") +
  labs(caption = bquote(n*' = '*.(n)*', '*R^2*' = '*.(r2))) +
  theme_classic() +
  theme(plot.caption = element_text(size = 11))
