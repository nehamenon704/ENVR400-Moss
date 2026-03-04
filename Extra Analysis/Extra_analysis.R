#setwd("/Users/sean/Desktop/Moss_analysis/ENVR400-Moss")
library(tidyverse)
library(lme4)

### This is a script to for variations in abundnance with height
data_raw <- read.csv("combined_cleaned_data.csv", stringsAsFactors = TRUE)
data_raw$height <- as.character(data_raw$height)
data_raw$height[data_raw$height == "hig"] <- "high"
data_raw$height <- as.factor(data_raw$height)
data_raw$quadrat_side <- ""

#for west roads
data_raw$quadrat_side[data_raw$road_side == "WR" & data_raw$aspect == "West"] <- "Road Facing"
data_raw$quadrat_side[data_raw$road_side == "WR" & data_raw$aspect == "East"] <- "Sidewalk Facing"
data_raw$quadrat_side[data_raw$road_side == "WR" & data_raw$aspect == "North"] <- "Parallel to Road"
data_raw$quadrat_side[data_raw$road_side == "WR" & data_raw$aspect == "South"] <- "Parallel to Road"

#for east roads
data_raw$quadrat_side[data_raw$road_side == "ER" & data_raw$aspect == "East"] <- "Road Facing"
data_raw$quadrat_side[data_raw$road_side == "ER" & data_raw$aspect == "West"] <- "Sidewalk Facing"
data_raw$quadrat_side[data_raw$road_side == "ER" & data_raw$aspect == "North"] <- "Parallel to Road"
data_raw$quadrat_side[data_raw$road_side == "ER" & data_raw$aspect == "South"] <- "Parallel to Road"

#for north roads
data_raw$quadrat_side[data_raw$road_side == "NR" & data_raw$aspect == "East"] <- "Parallel to Road"
data_raw$quadrat_side[data_raw$road_side == "NR" & data_raw$aspect == "West"] <- "Parallel to Road"
data_raw$quadrat_side[data_raw$road_side == "NR" & data_raw$aspect == "North"] <- "Road Facing"
data_raw$quadrat_side[data_raw$road_side == "NR" & data_raw$aspect == "South"] <- "Sidewalk Facing"

#for south roads
data_raw$quadrat_side[data_raw$road_side == "SR" & data_raw$aspect == "East"] <- "Parallel to Road"
data_raw$quadrat_side[data_raw$road_side == "SR" & data_raw$aspect == "West"] <- "Parallel to Road"
data_raw$quadrat_side[data_raw$road_side == "SR" & data_raw$aspect == "South"] <- "Road Facing"
data_raw$quadrat_side[data_raw$road_side == "SR" & data_raw$aspect == "North"] <- "Sidewalk Facing"

#add NAs. instead of missing values
data_raw$quadrat_side[data_raw$quadrat_side == ""] <- NA
# Code quadrat_side factors to be in different order
data_raw$quadrat_side <- factor(data_raw$quadrat_side, levels = c("Road Facing", "Sidewalk Facing", "Parallel to Road"))
data_raw$height <- factor(data_raw$height, levels = c("high", "med", "low"))

# add percent cover columns for graphing
data_raw$bryo_cover_percent <- 100*data_raw$bryo_cover/25
data_raw$lichen_cover_percent <- 100*data_raw$lichen_cover/25

##### Simple summary data
summary_data_height <- data_raw %>% 
  group_by(height) %>% 
  summarize(lichen_mean_cover = mean(lichen_cover_percent, na.rm = TRUE),
            moss_mean_cover = mean(bryo_cover_percent, na.rm = TRUE),
            lichen_median_cover = median(lichen_cover_percent, na.rm = TRUE),
            moss_median_cover = median(bryo_cover_percent, na.rm = TRUE),
            count = n())





##Test model on bryophytes
data_raw$tree_id <- as.factor(data_raw$tree_id)

#do a histogram
ggplot(data_raw, aes(x=bryo_cover_percent)) +
  geom_histogram() +
  facet_wrap(~height, ncol=1)


test_model_bryo <- glmer.nb(bryo_cover~quadrat_side+height+ (1|tree_id), data = data_raw)
summary(test_model_bryo)

#now change factor order so low is first, the re run same model
data_raw$height <- factor(data_raw$height, levels = c("low", "med", "high"))
test_model_bryo <- glmer.nb(bryo_cover~quadrat_side+height+ (1|tree_id), data = data_raw) 
summary(test_model_bryo)

#Plots for bryo
ggplot(filter(data_raw, quadrat_side != is.na(quadrat_side)), aes(x=quadrat_side, y = bryo_cover_percent)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  stat_summary(fun.data = "mean_se", geom="errorbar", width = 0.15) +
  labs(x="Side of tree",
       y = "Percent cover of bryophytes") +
  theme_bw() +
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)))


ggplot(data_raw, aes(x=height, y = bryo_cover_percent)) +
  stat_summary(fun = "mean", geom = "bar", fill = "steelblue") +
  stat_summary(fun.data = "mean_se", geom="errorbar", width = 0.15) +
  labs(x="Quadrat height",
       y = "Percent cover of bryophytes") +
  coord_cartesian(ylim = c(0, 21)) +
  geom_segment(x=1,y=17,xend=2,yend=17) +
  geom_segment(x=1,y=19,xend=3,yend=19) +
  geom_text(x=2,y=20,label="P=0.0012") +
  geom_text(x=1.5,y=18,label="P=0.048") +
  theme_classic() +
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)))

#Plots for lichen
ggplot(filter(data_raw, quadrat_side != is.na(quadrat_side)), aes(x=quadrat_side, y = lichen_cover)) +
  stat_summary(fun = "mean", geom = "bar")

ggplot(data_raw, aes(x=height, y = lichen_cover)) +
  stat_summary(fun = "mean", geom = "bar")

summary(glmer.nb(lichen_cover~height + (1|tree_id), data = data_raw))
