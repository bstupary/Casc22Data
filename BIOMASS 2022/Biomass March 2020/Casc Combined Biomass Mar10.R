##### Install / Load Packages #####
# install.packages("vegan")
# install.packages("ggplot2")
# install.packages("ggalt")
# install.packages("ggforce")
# install.packages("ggrepel")
# install.packages("ggpmisc")

library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)

#Set Working Directory
setwd("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020")

#Import Workspace from NMDS
load("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/R February/NMDS Master Feb24.RData")

######## Load in your datasets ###############
Big.Bio20 <- read.csv("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020/Bighorn Biomass.csv")
View(Big.Bio20)

Har.Bio20 <- read.csv("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020/Harrison Biomass.csv")
View(Har.Bio20)

Pip.Bio20 <- read.csv("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020/Pipit Biomass.csv")
View(Pip.Bio20)

Snw.Bio20 <- read.csv("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020/Snowflake Biomass.csv")
View(Snw.Bio20)

Casc.Cmbo.Bio <- read.csv("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020/Casc Combined Biomass.csv")
View(Casc.Cmbo.Bio)

Cntrls.Cmbo.Bio <- read.csv("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/Biomass March 2020/Cntrls Combined Biomass.csv")
View(Cntrls.Cmbo.Bio)


########### Remove 1970's datapoints so we focus on recent data ############




############ Visualize Plot ##################

Casc.Bio20.Fig <- ggplot(Casc.Cmbo.Bio,
                           aes(x = Year,
                               y = Mean.total)
                         ) +
  theme_bw()+
  theme(plot.tag = element_text(size=30),
        legend.title = element_text(size = 30, color = "black", face = "bold"),
        legend.justification = c(0, 1),
        legend.position = c(0.03, 0.96),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.key.size = unit(10, 'lines'),
        legend.key.height = unit(2, 'lines'),
        legend.key.width = unit(3,"line"),
        legend.text = element_text(size = 15),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15, color="#000000"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")))+
  geom_path(aes(colour=Lake),
            size=1.5)+#pretty lines
  labs(colour = "Cascade Lakes",
       tag = "f)")+
  stat_smooth(color = "black",
              fill = "#1b9e77",
              linetype = 5,
              alpha = 0.15)+
  geom_hline(yintercept = 0)+
  geom_point(pch=16, size=3)+
  ylab("Mean Annual Biomass (µg/L)") +
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(limits = c(1966,2015),
                     labels = waiver(),
                     breaks = c(1965,
                                1970,
                                1975,
                                1980,
                                1985,
                                1990,
                                1995,
                                2000,
                                2005,
                                2010,
                                2015))+
  scale_y_continuous(breaks = c(-50,
                                0,
                                50,
                                100,
                                150,
                                200,
                                250))

Casc.Bio20.Fig
