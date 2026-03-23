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
neighbourhood_lichen_plot <- ggplot(final_data, aes(x=NEIGHBOURHOOD_NAME, y=mean_lichen_cover, fill = NEIGHBOURHOOD_NAME)) +
  geom_violin(alpha = 0.45, colour = "black") +
  geom_point(position = position_jitter(width = 0.15), alpha =0.25)+
  stat_summary(fun = "mean", geom = "point", size = 3, colour = "black")+
  scale_fill_manual(values = c("darkgreen", "lightgreen"))+
  labs(x="Neighbourhood", 
       y = "Percent cover of lichen (%)") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("Figures/lichen_plot2.png", neighbourhood_lichen_plot, units = "mm", width = 90, height = 120)



#by road
ggplot(final_data, aes(x=Road, y=mean_lichen_cover)) +
  stat_summary(fun = "mean", geom = "bar")

#by genus
ggplot(final_data, aes(x=GENUS_NAME, y=mean_lichen_cover)) +
  stat_summary(fun = "mean", geom = "bar")


final_data %>% 
  group_by(NEIGHBOURHOOD_NAME) %>% 
  summarize(mean = mean(mean_lichen_cover, na.rm = TRUE))
