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

####### Subset to make Snwit coords dataframe ########
Snw20.b <- BC.Sites20[66:98,]
which(Snw20.b$Year == "1978")
Snw20.b <- Snw20.b[-12,]

SnwStock.b <- Snw20.b[1:12,]
SnwKill.b <- Snw20.b[12:13,]
SnwAbsent.b <- Snw20.b[13:32,]

################### Time to Visualize ##################
set.seed(42)


Snw.gg.dec21 <- ggplot() +
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
    legend.box.background = element_rect(
      fill = "transparent",
      colour = "transparent",
      size = 1.2
    )
    ) +
  #legend.justification = c(0.5,0.5))+
  #labs(tag = "d)")+
  xlab("NMDS 1") +
  ylab("NMDS 2") +
  geom_path(data = SnwStock.b,
            aes(x = NMDS1,
                y = NMDS2),
            col = "red",
            show.legend = FALSE,
            size = 1.5,
            arrow = arrow()) +
  geom_path(data = SnwKill.b,
            aes(x = NMDS1,
                y = NMDS2),
            col = "blue",
            show.legend = FALSE,
            size = 1.5,
            arrow = arrow()) +
  geom_path(data = SnwAbsent.b,
            aes(x = NMDS1,
                y = NMDS2),
            col = "green",
            show.legend = FALSE,
            size = 1.5,
            arrow = arrow()) +
  geom_point(data=Snw20.b[14,], #Makes mark at Hespero innoculation
             aes(x=NMDS1,
                 y=NMDS2),
             col= "black",
             pch = 24, #Upward triangle shape
             bg = "purple",
             size = 6,
             stroke = 1.4,
             show.legend = FALSE)+
  geom_point(data=Snw20.b[14,], #Makes mark at Hespero innoculation
             aes(x=NMDS1,
                 y=NMDS2),
             col= "black",
             pch = 25, #Downward triangle shape
             bg = "purple",
             size = 6,
             stroke = 1.4,
             show.legend = FALSE)+
  geom_point(data = Snw20.b, #Makes circles at year points
             aes(x = NMDS1,
                 y = NMDS2),
             col = "black",
             pch = 21,
             size = 3,
             stroke = 1.4) +
  geom_text(data = BC.Spp20, #Place Species names
            aes(x = NMDS1,
                y = NMDS2,
                label = Labels
            ),
            fontface = "italic",
            color = "black",
            size = 6,
            hjust = "outward") +
  geom_text_repel(data = SnwStock.b[12,], #Label everything but start and finish
                  aes(x = NMDS1,
                      y = NMDS2,
                      label = Year),
                  box.padding = 0.75,
                  point.padding = 0.5,
                  force = 5,
                  size = 5) +
  geom_text_repel(data = SnwAbsent.b[c(1,2),], #Label everything but start and finish
                  aes(x = NMDS1,
                      y = NMDS2,
                      label = Year),
                  box.padding = 0.75,
                  point.padding = 0.5,
                  force = 5,
                  size = 5) +
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
  geom_label_repel(data = Snw20.b[c(1,32),], #Start Label unique position
                   aes(x = NMDS1,
                       y = NMDS2,
                       label = Year),
                   fontface = "bold",
                   point.padding = 0.4,
                   box.padding = 0.95,
                   direction = "both",
                   label.r = 0.2,
                   force = 1,
                   size = 5,
                   show.legend = FALSE,
                   hjust = 0) + #Align on the left
  scale_color_manual(values = c("red",
                                "blue",
                                "green"),
                     labels = c("Fish Present",
                                "Fish Decline",
                                "Fish Absent")) +
  scale_fill_manual(values = c("red",
                               "blue",
                               "green"),
                    labels = c("Fish Present",
                               "Fish Decline",
                               "Fish Absent")) +
  # guides(fill = guide_legend(nrow = 1,
  #                            ncol = 1,
  #                            fill = "transparent",
  #                            alpha = 0.7,
  #                            override.aes = list(size = 10))) +#Makes Legend points size 10
  # scale_x_continuous(limits = c(-1.7,2.5),
  #                    labels = waiver(),
  #                    breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)) +
  # scale_y_continuous(limits = c(-1.55,2.5),
  #                    labels = waiver(),
  #                    breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5))
scale_x_continuous(limits = c(-1.7,2.7),
                   labels = waiver(),
                   breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2, 2.5)
) +
  scale_y_continuous(limits = c(-1.55,2),
                     labels = waiver(),
                     breaks = c(-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2)
  )

Snw.gg.dec21
## PLEASE figure out the legend not showing up future me
# Tue Feb 25 16:45:40 2020 ------------------------------
