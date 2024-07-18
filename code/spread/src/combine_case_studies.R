
#run the case studies
source("code/spread/src/1624_Italy.R")
source("code/spread/src/1918_England+Wales.R")
source("code/spread/src/1918_Japan.R")
source("code/spread/src/1918_Spain.R")
source("code/spread/src/1918_US.R")
source("code/spread/src/1980_ElSalvador.R")
source("code/spread/src/2020_Brazil.R")
source("code/spread/src/2020_US.R")

#additional libraries
pacman::p_load(ggpubr, grid)

#make overall axis labels
x <- text_grob("GRDM population loss estimate",
               face = "bold")
y <- text_grob("Well-validated death toll estimate (% of population)",
               rot = 90, face = "bold")

#plot
figure <- ggarrange(plot_1624,
                    plot_1918_1,
                    plot_1918_2,
                    plot_1918_3,
                    plot_1918_4,
                    plot_1980,
                    plot_2020_1,
                    plot_2020_2)

annotate_figure(figure,
                left = y,
                bottom = x)

ggsave("code/spread/output/spread_figure.png",
       dpi = 320, width = 6.5, height = 8)
