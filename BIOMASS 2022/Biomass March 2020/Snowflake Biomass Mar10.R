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

########### Subset fish changes so points connect ############
#Important to note this subseting isnt "nessicary" but makes the code pretier since we arnt using subset brackets
Snw.PrestoDec <- Snw.Bio20[13:14,1:14]
Snw.DectoAbs <- Snw.Bio20[14:15,1:14]


############ Visualize Plot ##################

Snw.Bio20.Fig <- ggplot(Snw.Bio20, aes(x=Year, y=Mean.Total)) +
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
  geom_path(aes(colour=Group), size=1.5)+#pretty lines
  labs(colour = "Snowflake Lake", tag = "d)")+
  geom_line(data = Snw.PrestoDec,
            aes(x=Year,
                y=Mean.Total),
            size=1.5,
            colour="blue")+#lines connecting fish pres to kill
  geom_line(data = Snw.DectoAbs,
            aes(x=Year,
                y=Mean.Total),
            size=1.5,
            colour="green")+#lines connecting fish kill to absent
  geom_point(aes(x=1992,y=17.369257),#Put symbol on hespero intro
             col= "black",
             pch = 24, #Upward triangle shape
             bg = "purple",
             size = 7,
             stroke = 1.4)+
  geom_point(aes(x=1992,y=17.369257), #Put symbol on hespero intro
             col= "black",
             pch = 25, #Downward triangle shape
             bg = "purple",
             size = 7,
             alpha=0.05,
             stroke = 1.4)+
  geom_text_repel(data = Snw.Bio20[15,],
                    aes(x = 1992,
                      y = 17.369257,
                      label = "H.arcticus\ninnoculation"
                      ),
                  direction = "both",
                  xlim = c(1993,NA),
                  ylim = c(100,NA),
                  point.padding = 0.5,
                  min.segment.length = 1,
                  force = 10,
                  size = 6
                  ) +
  scale_color_manual(values = c("red",
                                "blue",
                                "green"),
                     labels = c("Fish Present",
                                "Fish Decline",
                                "Fish Absent"))+
  scale_fill_manual(values = c("red",
                               "blue",
                               "green"),
                    labels = c("Fish Present",
                               "Fish Decline",
                               "Fish Absent"))+
  geom_point(pch=16, size=3)+
  geom_vline(xintercept = 1979, size=1.2, linetype=5)+
  geom_vline(xintercept = 1991, size=1.2, linetype=5)+
  ylab("Mean Annual Biomass (µg/L)")+
  scale_x_continuous(limits = c(1966,2015),
                     labels = waiver(),
                     breaks = c(1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015))+
  scale_y_continuous(limits = c(0,275),
                     breaks = c(0,50,100,150,200,250))

Snw.Bio20.Fig
