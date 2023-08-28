##### Install / Load Packages #####
library(vegan)
library(ggplot2)
library(ggalt)
library(ggrepel)
library(ggpmisc)

#Set Working Directory
setwd("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/R February")

#Import Workspace from NMDS
load("C:/Users/bstup/Desktop/New Control Tinkering Jan 17, 2020/R February/NMDS Master Feb24.RData")

####### Subset to make Bighorn coords dataframe ########
Big20 <- BC.Sites20[1:22,]

BigPresent <- Big20[1:10,]
BigKill <- Big20[10:16,]
BigAbsent <- Big20[16:22,]


################### Time to Visualize ##################

Big.gg.20 <- ggplot() +
  theme_classic() +
  theme(aspect.ratio = 1,
        plot.title = element_text(size = 40,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(0,0,12,0,unit = "pt")),
        plot.subtitle = element_text(size = 20,
                                     face = "italic",
                                     hjust = 0.5,
                                     margin = margin(0,0,0,0,unit = "pt")),
        axis.title = element_text(size = 20),
        plot.background = element_rect(fill = "transparent", colour = NA),
        axis.text = element_text(size = 15, color = "#000000"),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_rect(colour = "black", fill = NA, size = 1.7),
        #axis.line.x = element_line(size=1.2),
        #axis.line.y = element_line(size=1.2),
        #legend.position = c(0.88,0.9),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.title = element_blank(),
        legend.key.size = unit(2, 'lines'),
        legend.key.width = unit(4,"line"),
        #legend.box.margin = margin(t=0.5,unit = "cm"),
        legend.box.background = element_rect(fill = "transparent",
                                             colour = "transparent",
                                             size = 1.2),
        legend.text = element_text(size = 15),
        plot.tag = element_text(size=30)) +
  #legend.justification = c(0.5,0.5))+
  labs(tag = "c)")+
  xlab("NMDS 1") +
  ylab("NMDS 2") +
  geom_path(data = BigPresent,
            aes(x = NMDS1,
                y = NMDS2),
            col = "red",
            show.legend = FALSE,
            size = 1.5,
            arrow = arrow()) +
  geom_path(data = BigKill, #Makes line connecting groups at 1996 to 1997
            aes(x = NMDS1,
                y = NMDS2),
            color = "blue",
            show.legend = FALSE,
            size = 1.5,
            arrow = arrow()) +
  geom_path(data = BigAbsent, #Makes line connecting groups at 2002 to 2003
            aes(x = NMDS1,
                y = NMDS2),
            col = "green",
            size = 1.5,
            show.legend = FALSE,
            arrow = arrow()) +
  geom_point(data = Big20, #Makes circles at year points
             aes(x = NMDS1,
                 y = NMDS2),
             col = "black",
             pch = 21,
             size = 3,
             stroke = 1.4) +
  geom_text(data = BC.Spp20, #Place Species names
            aes(x = NMDS1,
                y = NMDS2,
                label = row.names(BC.Spp20)),
            fontface = "italic",
            color = "black",
            size = 6,
            hjust = "outward") +
  geom_text_repel(data = Big20[2:21,], #Label everything but start and finish
                  aes(x = NMDS1,
                      y = NMDS2,
                      label = Year),
                  box.padding = 0.75,
                  point.padding = 0.5,
                  force = 5,
                  size = 5) +
  geom_label_repel(data = Big20[c(1),], #Label the starting point
                   aes(x = NMDS1,
                       y = NMDS2,
                       label = Year),
                   fontface = "bold",
                   point.padding = 0.4,
                   box.padding = 0.95,
                   direction = "y",
                   label.r = 0.2,
                   force = 10,
                   size = 5,
                   show.legend = FALSE,
                   vjust = 0) + #force the label to go below the point, and away from overlaping species
  geom_label_repel(data = Big20[c(22),], #Label the end point
                   aes(x = NMDS1,
                       y = NMDS2,
                       label = Year),
                   fontface = "bold",
                   point.padding = 0.4,
                   box.padding = 0.95,
                   direction = "y",
                   label.r = 0.2,
                   force = 10,
                   size = 5,
                   show.legend = FALSE,
                   vjust = 1,#get the label to go above the point
                   ylim = 1) + #get the label to get about the y = 1 line, forcing it to be visble away from 2005
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
  scale_color_manual(values = c("red",
                                "blue",
                                "green"),
                     labels = c("Fish Present",
                                "Pop Crash ~1984",
                                "Fish Absent")) +
  scale_fill_manual(values = c("red",
                               "blue",
                               "green"),
                    labels = c("Fish Present",
                               "Pop Crash ~1984",
                               "Fish Absent")) +
  guides(fill = guide_legend(nrow = 1,
                            ncol = 1,
                            fill = "transparent",
                            alpha = 0.7,
                            override.aes = list(size = 10))) +#Makes Legend points size 10
  scale_x_continuous(limits = c(-1.7,2.7),
                     labels = waiver(),
                     breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
                     ) +
  scale_y_continuous(limits = c(-1.55,2),
                     labels = waiver(),
                     breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)
                     )

Big.gg.20
## PLEASE figure out the legend not showing up future me
# Tue Feb 25 17:22:54 2020 ------------------------------
