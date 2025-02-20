#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/trail-of-tears/src/cherokee.src

# ----- setup

if (!require(pacman)) {install.packages("pacman")}

pacman::p_load(ggplot2, dplyr, ggpubr)

# ----- data

# from Thornton 1984; assuming single-year estimates as mid-year estimates
crisis <- 1840
pre <- cbind.data.frame(time = c(1809, 1826.5, 1828.5, 1835.5),
                        population = c(13395, 17713, 18722, 21542))
post <- cbind.data.frame(time = c(1852, 1866.5, 1875.5, 1880.5),
                         population = c(15802, 15566, 19717, 21920))

# ----- main

# derivation for panel a
t1809.to.crisis <- data.frame(time = seq(pre$time[1], crisis, by = 0.1))
crisis.to.t1880 <- data.frame(time = seq(crisis, post$time[4], by = 0.1))

f <- lm(log(population) ~ time, data = pre)
b <- lm(log(population) ~ time, data = post)

f.fit <- data.frame(time = t1809.to.crisis,
                    population = exp(predict(object = f,
                                             newdata = t1809.to.crisis)))
b.fit <- data.frame(time = crisis.to.t1880,
                    population = exp(predict(object = b,
                                             newdata = crisis.to.t1880)))

# derivation for panel b
crisis.to.CW <- data.frame(time = seq(crisis, 1863, by = 0.1))
CW.to.t1880 <- data.frame(time = seq(1863, post$time[4], by = 0.1))

b.postCW <- lm(log(population) ~ time, data = post[2:4,])

b.fit.postCW <- data.frame(time = CW.to.t1880,
                           population = exp(predict(object = b.postCW,
                                                    newdata = CW.to.t1880)))

post.CW.pop <- b.fit.postCW$population[1]
pre.CW.pop <- post.CW.pop + 7000

post_pre.CW <- cbind.data.frame(time = c(1852, 1863),
                                population = c(15802, pre.CW.pop))

b.preCW <- lm(log(population) ~ time, data = post_pre.CW)

b.fit.preCW <- data.frame(time = crisis.to.CW,
                          population = exp(predict(object = b.preCW,
                                                   newdata = crisis.to.CW)))

# plots
panelA <- ggplot() +
    geom_smooth(data = f.fit, aes(time, population),
                method = "loess", linetype = "dashed", se = FALSE) +
    geom_smooth(data = b.fit, aes(time, population),
                method = "loess", linetype = "dashed", se = FALSE) +
    geom_segment(aes(x = crisis, y = b.fit$population[1],
                     xend = crisis, yend = tail(f.fit$population, 1)),
                 col = "red") +
    geom_point(data = pre, aes(time, population)) +
    geom_point(data = post, aes(time, population)) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0),
                       name = "Population") +
    theme(axis.title.x = element_blank()) +
    theme_classic()

panelB <- ggplot() +
    geom_smooth(data = f.fit, aes(time, population),
                method = "loess", linetype = "dashed", se = FALSE) +
    geom_smooth(data = b.fit.preCW, aes(time, population),
                method = "loess", linetype = "dashed", se = FALSE) +
    geom_smooth(data = b.fit.postCW, aes(time, population),
                method = "loess", linetype = "dashed", se = FALSE) +
    geom_segment(aes(x = crisis, y = b.fit.preCW$population[1],
                     xend = crisis, yend = tail(f.fit$population, 1)),
                 col = "red") +
    geom_segment(aes(x = 1863, y = b.fit.postCW$population[1],
                     xend = 1863, yend = tail(b.fit.preCW$population, 1)),
                 col = "purple") +
    geom_point(data = pre, aes(time, population)) +
    geom_point(data = post, aes(time, population)) +
    scale_x_continuous(name = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0),
                       name = "Population") +
    theme(axis.title.x = element_blank()) +
    theme_classic()

ggarrange(panelA, panelB,
          labels = c("a", "b"),
          nrow = 1)

ggsave(here::here("code/trail-of-tears/output/Figure-S2-Cherokee-deaths.png"),
       width = 4.9, height = 4, dpi = 720)

# death toll estimate
thornton <- tail(f.fit$population, 1) - b.fit$population[1]
update <- tail(f.fit$population, 1) - b.fit.preCW$population[1]

thornton
update

# done.
