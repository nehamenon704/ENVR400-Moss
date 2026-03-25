library(tidyverse)
data <- read.csv("final_data.csv")
#Histogram - roughly normal
ggplot(data, aes(x=mean_species_count)) +
  geom_histogram()
qqnorm(data$mean_species_count)
qqline(data$mean_species_count)

#factors
data$neighbourhood_name <- as.factor(data$neighbourhood_name)
data$GENUS_NAME <- as.factor(data$GENUS_NAME)
data$Road <- as.factor(data$Road)

#Let's just do an additive fixed-effect anova - variables are Road, Genus_name, and neighbourhood_name
species_linear_model_add <- lm(mean_species_count~neighbourhood_name+GENUS_NAME+Road, data = data)
anova(species_linear_model_add)
summary(species_linear_model_add)

par(mfrow=c(2,2))
plot(species_linear_model_add)
par(mfrow=c(1,1))
qqnorm(residuals(species_linear_model_add), main = "Residual Q-Q Plot for Visually Distinct Species Model")
qqline(residuals(species_linear_model_add))
#let's do an interaction model - better model
species_linear_model_inter <- lm(mean_species_count~neighbourhood_name+GENUS_NAME+Road+Road:neighbourhood_name+Road:GENUS_NAME+neighbourhood_name:GENUS_NAME, data = data)
anova(species_linear_model_inter)
par(mfrow=c(2,2))
plot(species_linear_model_inter)
anova(species_linear_model_add, species_linear_model_inter)
par(mfrow=c(1,1))
qqnorm(residuals(species_linear_model_inter), main = "Residual Q-Q Plot for Visually Distinct Species Model")
qqline(residuals(species_linear_model_inter))
summary(species_linear_model_inter)
#Final model
species_lm_full_inter <- lm(mean_species_count~neighbourhood_name*GENUS_NAME*Road, data = data)
anova(species_lm_full_inter)
summary(species_lm_full_inter)
par(mfrow=c(1,2))
plot(species_lm_full_inter)
par(mfrow=c(1,1))
qqnorm(residuals(species_lm_full_inter), main = "Residual Q-Q Plot for Visually Distinct Species Model")
qqline(residuals(species_lm_full_inter))

#Pairwise comparisons
comps_full_species <- emmeans(species_lm_full_inter, specs = ~ GENUS_NAME:Road:neighbourhood_name, adjust = "Tukey")
contrast(comps_full_species, method = "pairwise")

comps_genus_only <- emmeans(species_lm_full_inter, specs = ~ GENUS_NAME, adjust = "Tukey")
contrast(comps_genus_only, method = "pairwise")

## Plots

genus_species_plot <- ggplot(final_data, aes(x=GENUS_NAME, y=mean_species_count, fill = GENUS_NAME)) + 
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  labs(x="Tree genus", y= "Mean 'visually distinct taxa' per quadrat") +
  theme_bw()+
  annotate(geom = "text", x=1,y=1.9,label = "a", size = 5)+
  annotate(geom = "text", x=2,y=1.87,label = "a", size = 5)+
  annotate(geom = "text", x=3,y=1.5,label = "b", size = 5)+
  annotate(geom = "text", x=4,y=1.7,label = "ab", size = 5)+
  theme(legend.position = "none",
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))
genus_species_plot

ggsave("Figures/genus_species_plot.png", genus_species_plot, units = "cm", width = 12, height = 12)



###### NEED TO UPDATE COLOURS
road_neighbourhood_plot <- ggplot(data, aes(x=Road, y=mean_species_count, fill = Road)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width = 0.2) +
  facet_wrap(~NEIGHBOURHOOD_NAME) +
  labs(x="Road Type",
       y="Mean Species Count Per Quadrat",
       fill = "Road Traffic Level") +
  theme_bw() +
  theme(legend.position = c(0.2,0.85),
        legend.background = element_rect(colour = "black", fill = "white", size = 0.5))
road_neighbourhood_plot
ggsave("Figures/road_neighbourhood_diversity.jpg", road_neighbourhood_plot, width = 5.5,
       height = 5.5)


### Want a paired bar graph (road and genus)
#Fagus - kits P<0.0001
#Prunus - renfrew 0.008
#Data frame for significance annotations
plot_annotations_species <- data.frame(label = c("***","***"),
                                       NEIGHBOURHOOD_NAME = c("KITSILANO", "RENFREW-COLLINGWOOD"),
                                       GENUS_NAME = c("Beech", "Cherry"), 
                                       Road = c("Residential", "Residential"),
                                       height = c(2,2.7))

road_genus_plot_species <- ggplot(final_data, 
                          aes(x=GENUS_NAME, y = mean_species_count, fill = Road)) +
  stat_summary(geom = "bar", fun = "mean", position = "dodge") +
  stat_summary(geom = "errorbar", fun.data = "mean_se", position = position_dodge(width = 0.9), width = 0.2) +
  labs(y = "'Visually disnctint taxa' per quadrat",
       x = "Tree genus", 
       fill = "Road traffic level") +
  facet_wrap(~NEIGHBOURHOOD_NAME, ncol = 1) +
  geom_text(data = plot_annotations_species, mapping = aes(x=GENUS_NAME, y = height, label = label),
            size = 6) +
  coord_cartesian(ylim = c(0,2.85))+
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.3,0.915),
        legend.background = element_rect(colour = "black"))
road_genus_plot_species

ggsave("Figures/road_genus_diversity.jpg", road_genus_plot, width = 5.5,
       height = 8)

#Neighbourhood plot
neighbourhood_species_plot <- ggplot(final_data, aes(x=NEIGHBOURHOOD_NAME, y = mean_species_count, fill = NEIGHBOURHOOD_NAME)) +
  geom_violin(alpha = 0.45, colour = "black") +
  geom_point(position = position_jitter(width = 0.15), alpha =0.25)+
  stat_summary(fun = "mean", geom = "point", size = 3, colour = "black")+
  scale_fill_manual(values = c("darkgreen", "lightgreen")) +
  labs(x="Neighbourhood", 
       y = "'Visually distinct taxa' per quadrat") +
  theme_bw() +
  annotate(geom = "text", label = "P = 9.138e-08 ***", x=0.9, y=3.6) +
  theme(legend.position = "none")

#Lets try to combine these plots into one
neighbourhood_spaced_plot_species <- plot_grid(neighbourhood_species_plot, NULL, ncol = 1)
neighbourhood_spaced_plot_species
combined_species_plot <- plot_grid(neighbourhood_spaced_plot_species, road_genus_plot_species,
                                  labels = c("A","B"))
combined_species_plot

ggsave("Figures/combined_species_plot.png", combined_species_plot, units = "cm", width = 18, height = 18)

#### Mean differences
data %>% 
  group_by(neighbourhood_name) %>% 
  summarize(mean = mean(mean_species_count, na.rm = TRUE),
            mean_lichen = mean(mean_lichen_cover, na.rm = TRUE))

data %>% 
  group_by(GENUS_NAME) %>% 
  summarize(mean = mean(mean_species_count, na.rm = TRUE))

####Inspect sample sizes
data %>% 
  filter(road_side != "removal") %>% 
  group_by(NEIGHBOURHOOD_NAME, GENUS_NAME, Road) %>% 
  summarize(count = n())



