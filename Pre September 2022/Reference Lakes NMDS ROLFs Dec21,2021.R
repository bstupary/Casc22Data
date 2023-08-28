##### Install / Load Packages #####
library(vegan)
library(ggplot2)
library(ggalt)
library(ggforce)
library(ggrepel)
library(ggpmisc)

#Set Working Directory
setwd("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/R February")

#Import Workspace from NMDS
load("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/R February/NMDS Master Feb24.RData")

####### Subset to make control lakes coords dataframe(s) ########
Cntrl20 <- BC.Sites20[99:136,]

#Get the average
Cntrl20.X.Mean <- mean(Cntrl20[,"NMDS1"])
Cntrl20.Y.Mean <- mean(Cntrl20[,"NMDS2"])

Opabin <- Cntrl20[1:8,]
Oesa <- Cntrl20[9:16,]
Eiffel <- Cntrl20[17:24,]
Hungabee <- Cntrl20[25:31,]
Sentinel <- Cntrl20[32:38,]

BC.Spp20$Labels <- c("Acan.vern", "Alon.gutt", "Chyd.spha", "Cyclopoids", "Daph.midd", "Daph.pule", "Diac.thom", "Eucyclops", "Harpacticoids", "Hesp.arct", "Lept.tyrr", "Macr.albi")


################### ROLF's Symmetrical Scaled Plots (Dec 21,2021) ##################

Cntrl.gg.dec21 <- ggplot() +
  theme_bw() +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(
      size = 40,
      face = "bold",
      hjust = 0.5,
      margin = margin(0, 0, 12, 0, unit = "pt")
    ),
    plot.subtitle = element_text(
      size = 20,
      face = "italic",
      hjust = 0.5,
      margin = margin(0, 0, 0, 0, unit = "pt")
    ),
    axis.title = element_text(size = 20),
    plot.background = element_rect(fill = "transparent", colour = NA),
    axis.text = element_text(size = 15, color = "#000000", face = "bold"),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.title = element_blank(),
    legend.key.size = unit(2, 'lines'),
    legend.key.width = unit(4, "line"),
    legend.position = "blank",
    #legend.box.margin = margin(t=0.5,unit = "cm"),
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 1.2
    ),
    legend.text = element_text(size = 15),
    plot.tag = element_text(size = 30)
  ) +
  #legend.justification = c(0.5,0.5))+
  xlab("NMDS 1") +
  ylab("NMDS 2") +
  #labs(tag = "a)") +
  geom_path(data = Cntrl20,
            aes(x = NMDS1,
                y = NMDS2,
                group = Lake,
                color = Lake
            ),
            size = 1.5,
            arrow = arrow()
  ) +
  stat_ellipse(data = Cntrl20,
               aes(x = NMDS1,
                   y = NMDS2
               ),
               linetype = 2,
               size = 1.5
  ) +
  stat_ellipse(data = Cntrl20,
               aes(x = NMDS1,
                   y = NMDS2
               ),
               geom = "polygon",
               fill = "darkorchid2",
               alpha = 0.3
  ) +
  geom_point(data = Cntrl20, #Makes circles at year points
             aes(x = NMDS1,
                 y = NMDS2,
                 shape = Lake
             ),
             col = "black",
             pch = 21,
             size = 3,
             stroke = 1.4
  ) +
  geom_text(data = BC.Spp20, #Place Species names
            aes(x = NMDS1,
                y = NMDS2,
                label = Labels
            ),
            fontface = "italic",
            color = "black",
            size = 6,
            hjust = "outward"
  ) +
  # geom_text_repel(data = Cntrl20, #Place Site names
  #           aes(x = NMDS1,
  #               y = NMDS2,
  #               label = row.names(Cntrl20)
  #           ),
  #           fontface = "italic",
  #           color = "black",
  #           size = 4,
  #           min.segment.length = 0,
  #           xlim = c(-0.5,NA),
  #           max.overlaps = Inf,
  #           hjust = "outward" +
  # guides(fill = guide_legend(nrow = 1,
  #                            ncol = 1,
  #                            fill = "transparent",
  #                            alpha = 0.7,
  #                            override.aes = list(size = 10),
  #                            color = guide_legend("title")
  #                            )
  #  ) +#Makes Legend points size 10
  # scale_x_continuous(limits = c(-1.7, 2.5),
  #                    labels = waiver(),
  #                    breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
  # ) +
  # scale_y_continuous(limits = c(-1.55, 2.5), #recaling Y per Rolf's request
  #                    labels = waiver(),
  #                    breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
scale_x_continuous(limits = c(-1.7,2.7),
                   labels = waiver(),
                   breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
                   ) +
  scale_y_continuous(limits = c(-1.55,2),
                     labels = waiver(),
                     breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)
                     )+
  scale_colour_brewer(palette = "Set1") # Recolor groups

Cntrl.gg.dec21


############# For candidacy (Nov4,2021), removing legend and stealing it from old plot in original folder path