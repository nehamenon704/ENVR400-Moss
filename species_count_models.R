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
par(mfrow=c(1,2))
plot(species_lm_full_inter)
par(mfrow=c(1,1))
qqnorm(residuals(species_lm_full_inter), main = "Residual Q-Q Plot for Visually Distinct Species Model")
qqline(residuals(species_lm_full_inter))
summary(species_lm_full_inter)


## Plots
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
road_genus_plot <- ggplot(data, aes(x=GENUS_NAME, y=mean_species_count, fill = Road)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", position = position_dodge(width =0.9), width = 0.2) +
  labs(x="Tree Genus",
       y="Mean Species Count Per Quadrat",
       fill = "Road Traffic Level") +
  theme_bw()
road_genus_plot
ggsave("Figures/road_genus_diversity.jpg", road_genus_plot, width = 8,
       height = 5.5)

## Now i want to combine the two above plots
final_species_plot <- ggplot(data, aes(x=GENUS_NAME, y=mean_species_count, fill = Road)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  stat_summary(fun.data = "mean_se", geom = "errorbar", 
               position = position_dodge(width =0.9), width = 0.2) +
  facet_wrap(~NEIGHBOURHOOD_NAME) +
  labs(x="Tree Genus",
       y="Mean Species Count Per Quadrat",
       fill = "Road Traffic Level") +
  theme_bw()
final_species_plot

#### Mean differences
data %>% 
  group_by(neighbourhood_name) %>% 
  summarize(mean = mean(mean_species_count, na.rm = TRUE))

####Inspect sample sizes
data %>% 
  filter(road_side != "removal") %>% 
  group_by(NEIGHBOURHOOD_NAME, GENUS_NAME, Road) %>% 
  summarize(count = n())

