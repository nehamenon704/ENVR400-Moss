### This script is to test for data normality
library(tidyverse)
library(cowplot)
### each person set their own working directory - setwd("/Users/sean/Desktop/Moss_analysis/ENVR400-Moss")
kits_data <- read.csv("DATA COLLECTION - Cleaned Kits Values.csv", stringsAsFactors = TRUE) %>% 
  mutate(neighbourhood = "kits")
renfrew_data <- read.csv("DATA COLLECTION - Cleaned Renfrew Values.csv", stringsAsFactors = TRUE) %>% 
  mutate(neighbourhood = "renfrew")
#combine data
all_data <- rbind(kits_data, renfrew_data) %>% 
  pivot_longer(4:12, names_to = "data_type", values_to = "values") %>% 
  separate(col = data_type, into = c("height", "data_type"), sep = "_")
all_data$data_type[all_data$data_type == "bryo"] <- "bryo_cover"
all_data$data_type[all_data$data_type == "byro"] <- "bryo_cover"
all_data$data_type[all_data$data_type == "lichen"] <- "lichen_cover"
all_data$data_type <- as.factor(all_data$data_type)
levels(all_data$data_type)
all_data <- all_data %>% 
  pivot_wider(names_from = data_type, values_from = values)
#put it in the folder as a csv for future use
write_csv(all_data, "combined_cleaned_data.csv")

summarized_data <- all_data %>% 
  group_by(tree_id) %>% 
  summarize(neighbourhood_name = first(neighbourhood),
            road_side = first(road_side),
            mean_bryo_cover = mean(bryo_cover, na.rm = TRUE),
            mean_lichen_cover = mean(lichen_cover, na.rm = TRUE),
            mean_species_count = mean(species, na.rm = TRUE))
write_csv(summarized_data, "combined_summarized_data.csv")


##### Histograms!!!!!
bryo_cover_histogram <- ggplot(summarized_data, aes(x=mean_bryo_cover)) +
  geom_histogram()+ #STRONG RIGHT SKEW
  labs(x = "Mean Bryophyte Percent Cover",
       y = "Count") +
  theme_classic()
lichen_cover_histogram <- ggplot(summarized_data, aes(x = mean_lichen_cover)) +
  geom_histogram() + #SLIGHT RIGHT SKEW
  labs(x = "Mean Lichen Percent Cover",
       y = "Count") +
  theme_classic()
species_histogram <- ggplot(summarized_data, aes(x = mean_species_count)) +
  geom_histogram() + #NORMAL
  labs(x = "Mean Species Count",
       y = "Count") +
  theme_classic()

initial_histogram_plot <- plot_grid(bryo_cover_histogram, lichen_cover_histogram, species_histogram, ncol = 1)
initial_histogram_plot
ggsave("Figures/initial_histograms.png", plot = initial_histogram_plot, height = 8, width = 5)

### Try to Log Transform Bryo Cover
log_transformed_Bryo_histogram <- ggplot(summarized_data, aes(x=log(mean_bryo_cover +1))) +
  geom_histogram() + # wait but this won;t work with all the 0 values, so I added a plus one - need to make sure this is scientifically legit
# So this doesn't work enough - too many 0 values
  labs(x = "Log(Mean Bryophyte Cover +1)",
       y = "Count") +
  theme_classic()

## Try to root transform lichen cover
root_transformed_lichen_hist <- ggplot(summarized_data, aes(x = (mean_lichen_cover)^(1/2))) +
  geom_histogram() + #### Square root transformation works well on lichen!!!!
  labs(x = "Srqt(Mean Lichen Cover)",
       y = "Count") +
  theme_classic()

final_histogram_plot <- plot_grid(log_transformed_Bryo_histogram, root_transformed_lichen_hist, species_histogram, ncol = 1)
final_histogram_plot
ggsave("Figures/final_histograms.png", plot = initial_histogram_plot, height = 8, width = 5)
