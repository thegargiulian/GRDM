#
# Authors:     HG
# Maintainers: MG, HG
# =========================================
# GRDM/code/spread/src/combine-case-studies.R

pacman::p_load(here, ggpubr, grid)

#run the case studies
source(here::here("code/spread/src/1624-Italy.R"))
source(here::here("code/spread/src/1871-Amsterdam.R"))
source(here::here("code/spread/src/1918-EnglandWales.R"))
source(here::here("code/spread/src/1918-Japan.R"))
source(here::here("code/spread/src/1918-Spain.R"))
source(here::here("code/spread/src/1918-US.R"))
source(here::here("code/spread/src/1939-Austria.R"))
source(here::here("code/spread/src/1944-Netherlands.R"))
source(here::here("code/spread/src/1980-ElSalvador.R"))
source(here::here("code/spread/src/2020-Brazil.R"))
source(here::here("code/spread/src/2020-US.R"))

#make overall axis labels
x <- text_grob("GRDM population loss estimate")
y <- text_grob("Better-validated death toll estimate (% of population)",
               rot = 90)

#plot
figure <- ggarrange(plot_1624,
                    plot_1871,
                    plot_1918_1,
                    plot_1918_2,
                    plot_1918_3,
                    plot_1918_4,
                    plot_1939,
                    plot_1944,
                    plot_1980,
                    plot_2020_1,
                    plot_2020_2,
                    ncol = 3, nrow = 4,
                    align = "hv")

annotate_figure(figure,
                left = y,
                bottom = x)

ggsave(here::here("code/spread/output/Figure-4-spread.png"),
       dpi = 720, width = 4.9, height = 5.7)

# done.