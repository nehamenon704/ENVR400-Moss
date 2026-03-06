
library(broom)
library(MASS)
library(readr)   # for read_csv
library(dplyr)   # for sample_n
#library(cowplot)
library(tidyverse)

dataset<-read_csv('combined_summarized_data.csv')
head(dataset)

kits_trees<-read.csv("kitsilano_trees_updated.csv", sep = ",", stringsAsFactors = FALSE)
rc_trees<-read.csv("renfrewcollingwood_trees_updated.csv", sep = ",", stringsAsFactors = FALSE)


#trees <- read.csv("public-trees.csv", sep = ",", stringsAsFactors = FALSE)
kits_trees <- kits_trees %>%
  rename(tree_id = TREE_ID)
rc_trees <- rc_trees %>%
  rename(tree_id = TREE_ID)

joined_df <- inner_join(dataset, kits_trees, by = "tree_id")

joined_df2 <- inner_join(dataset, rc_trees, by = "tree_id")

final_data<-rbind(joined_df,joined_df2)
write.csv(final_data, 'final_data.csv')

#Creating a summary data set
cleaned_data<-final_data%>%
  group_by(GENUS_NAME,Road, neighbourhood_name)%>%
  summarise(mean_bryo=mean(mean_bryo_cover,na.rm = TRUE),
            mean_lichen = mean(mean_lichen_cover, na.rm = TRUE),
            mean_species = mean(mean_species_count, na.rm = TRUE))
## Below is for bootstrapping
lm_boot <- slice_sample(cleaned_data, n = 16, replace = TRUE)
head(lm_boot)

slope_sampling_dist_boot <-
  ggplot(final_data, aes(x = mean_bryo_cover)) +
  geom_histogram() +
  xlab("Slope Estimations of mean") +
  ggtitle("Bootstrap sampling distribution for the estimator of the slope")

slope_sampling_dist_boot
#### Neha Work using bootstrapped data
mlr<-lm(mean_bryo~GENUS_NAME+Road+neighbourhood_name,lm_boot) %>%
  tidy()

lm_boot2<-lm_boot%>%
  mutate(mean_bryo=round(mean_bryo))

poisson_model<- glm.nb(mean_bryo_cover ~ neighbourhood_name+GENUS_NAME+ Road,
                      data = final_data,
                      family = poisson) %>%
  tidy(conf.int=TRUE)
poisson_model


neg_bin<-glm.nb(mean_bryo_cover ~ neighbourhood_name+GENUS_NAME+ Road,
                       data = final_data) %>%
  tidy(conf.int=TRUE)

neg_bin

library(MASS)

# Fit Negative Binomial model
NegBinom_Mod <- MASS::glm.nb(mean_bryo_cover ~ neighbourhood_name+GENUS_NAME+ Road,
                             data = final_data)

# Model summary
summary(NegBinom_Mod)

