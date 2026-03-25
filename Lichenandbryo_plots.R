library(tidyverse)
library(cowplot)

final_data <- read.csv("final_data.csv", stringsAsFactors = TRUE)

levels(final_data$GENUS_NAME) <- c("Maple", "Hornbeam", "Beech", "Cherry")

#Plot of lichen abundance for each tree genus, paired bars for arterial vs residential
#create data for *** annotations
plot_annotations <- data.frame(label = c("***","**","*","***"),
                               NEIGHBOURHOOD_NAME = c("KITSILANO", "KITSILANO", 
                                                      "KITSILANO", "RENFREW-COLLINGWOOD"),
                               GENUS_NAME = c("Beech", "Hornbeam", "Maple", "Cherry"), 
                               Road = c("Residential", "Residential", "Residential", "Residential"),
                               height = c(35,35,35,63))

road_genus_neighbourhood_plot <- ggplot(final_data, 
                                        aes(x=GENUS_NAME, y = mean_lichen_cover/25*100, fill = Road)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.9), width = 0.2) +
  labs(y = "Percent cover of lichen (%)",
       x = "Tree genus", 
       fill = "Road traffic level") +
  facet_wrap(~NEIGHBOURHOOD_NAME) +
  geom_text(data = plot_annotations, mapping = aes(x=GENUS_NAME, y = height, label = label),
            size = 6) +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.11,0.88),
        legend.background = element_rect(colour = "black"))
road_genus_neighbourhood_plot
ggsave("Figures/lichen_plot1.png", road_genus_neighbourhood_plot, units = "mm", width = 240, height = 140)


#Or with viollin and data behind - don't like
ggplot(final_data, aes(x=GENUS_NAME, y = mean_lichen_cover, fill = Road, colour = Road)) +
  geom_point(position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.8), 
             alpha = 0.35, size = 1.5) +
  geom_violin(alpha = 0.45, position = position_dodge(width = 0.8)) +
  stat_summary(geom = "point", fun = "mean", size = 2, 
               position = position_dodge(width =0.8), pch = 21) +
  annotate(geom = "text", x=3, y=18, label = "*", size = 7) +
  theme_bw()

#by neighbourhood
neighbourhood_lichen_plot <- ggplot(final_data, aes(x=NEIGHBOURHOOD_NAME, y=mean_lichen_cover/25*100, fill = NEIGHBOURHOOD_NAME)) +
  geom_violin(alpha = 0.45, colour = "black") +
  geom_point(position = position_jitter(width = 0.15), alpha =0.25)+
  stat_summary(fun = "mean", geom = "point", size = 3, colour = "black")+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  labs(x="Neighbourhood", 
       y = "Percent cover of lichen (%)") +
  annotate(geom = "text", x=1,y=75, label = "P = 5.674e-11 ***", size = 5)+
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
neighbourhood_lichen_plot
ggsave("Figures/lichen_plot2.png", neighbourhood_lichen_plot, units = "mm", width = 90, height = 120)

#With opposite facet wrap
road_genus_neighbourhood_plot_2 <- ggplot(final_data, 
                                        aes(x=GENUS_NAME, y = mean_lichen_cover/25*100, fill = Road)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.9), width = 0.2) +
  labs(y = "Percent cover of lichen (%)",
       x = "Tree genus", 
       fill = "Road traffic level") +
  facet_wrap(~NEIGHBOURHOOD_NAME, ncol = 1) +
  geom_text(data = plot_annotations, mapping = aes(x=GENUS_NAME, y = height, label = label),
            size = 6) +
  theme_bw() +
  coord_cartesian(ylim = c(0,66)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75,0.91),
        legend.background = element_rect(colour = "black"))
road_genus_neighbourhood_plot_2

#Lets try to combine these plots into one
neighbourhood_spaced_plot <- plot_grid(neighbourhood_lichen_plot, NULL, ncol = 1)
neighbourhood_spaced_plot
combined_lichen_plot <- plot_grid(neighbourhood_spaced_plot, road_genus_neighbourhood_plot_2,
                                  labels = c("A","B"))
combined_lichen_plot

ggsave("Figures/combined_lichen_plot.png", combined_lichen_plot, units = "cm", width = 18, height = 18)



########For bryos

road_genus_neighbourhood_plot_bryos <- ggplot(final_data, 
                                              aes(x=GENUS_NAME, y = mean_bryo_cover/25*100, fill = Road)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.9), width = 0.2) +
  labs(y = "Percent cover of bryophytes (%)",
       x = "Tree genus", 
       fill = "Road traffic level") +
  facet_wrap(~NEIGHBOURHOOD_NAME, ncol = 1) +
  #geom_text(data = plot_annotations, mapping = aes(x=GENUS_NAME, y = height, label = label),
   #         size = 6) +
  theme_bw() +
  #coord_cartesian(ylim = c(0,66)) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.77,0.89),
        legend.background = element_rect(colour = "black"))
road_genus_neighbourhood_plot_bryos


neighbourhood_bryo_plot <- ggplot(final_data, aes(x=NEIGHBOURHOOD_NAME, y=mean_bryo_cover/25*100, fill = NEIGHBOURHOOD_NAME)) +
  geom_violin(alpha = 0.45, colour = "black") +
  geom_point(position = position_jitter(width = 0.15), alpha =0.25)+
  stat_summary(fun = "mean", geom = "point", size = 3, colour = "black")+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  labs(x="Neighbourhood", 
       y = "Percent cover of bryophytes (%)") +
  annotate(geom = "text", x=1,y=85, label = "P = 0.048362 *", size = 5)+
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
neighbourhood_bryo_plot

neighbourhood_plot <- plot_grid(neighbourhood_bryo_plot, neighbourhood_lichen_plot, ncol = 2)
neighbourhood_plot

ggsave("Figures/neighbourhood_plot.png", neighbourhood_plot, units = "cm", width = 25, height = 12.5)


neighbourhood_spaced_plot_bryo <- plot_grid(neighbourhood_bryo_plot, NULL, ncol = 1)
neighbourhood_spaced_plot_bryo
combined_bryo_plot <- plot_grid(neighbourhood_spaced_plot_bryo, road_genus_neighbourhood_plot_bryos,
                                  labels = c("A","B"))
combined_bryo_plot
ggsave("Figures/combined_bryo_plot.png", combined_bryo_plot, units = "cm", width = 18, height = 18)




##Road type only plots
road_bryo_plot <- ggplot(final_data, aes(x=Road, y=mean_bryo_cover/25*100, fill = Road)) + 
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  labs(x="Road Type", y = "Bryophyte percent cover (%)") +
  annotate(geom = "text", x=1,y=20, label = "P = 1.162e-06 ***", size = 5)+
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
road_bryo_plot

ggsave("Figures/road_bryo_plot.png", road_bryo_plot, units = "cm", width = 10, height = 10)


road_lichen_plot <- ggplot(final_data, aes(x=Road, y=mean_lichen_cover/25*100, fill = Road)) + 
  geom_violin(alpha = 0.4) +
  geom_point(position = position_jitter(width = 0.15), alpha =0.25)+
  stat_summary(fun = "mean", geom = "point", size = 3, colour = "black")
road_lichen_plot

##Tree genus only plots
genus_bryo_plot <- ggplot(final_data, aes(x=GENUS_NAME, y=mean_bryo_cover/25*100, fill = GENUS_NAME)) + 
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  labs(x="Tree genus", y= "Bryophyte percent cover (%)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
genus_bryo_plot

genus_lichen_plot <- ggplot(final_data, aes(x=GENUS_NAME, y=mean_lichen_cover/25*100, fill = GENUS_NAME)) + 
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  labs(x="Tree genus", y= "Lichen percent cover (%)") +
  theme_bw()+
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
genus_lichen_plot


ggsave("Figures/genus_lichen_plot.png", genus_lichen_plot, units = "cm", width = 11, height = 11)
ggsave("Figures/genus_bryo_plot.png", genus_bryo_plot, units = "cm", width = 11, height = 11)
