#setwd("/Users/sean/Desktop/Moss_analysis/ENVR400-Moss")
library(tidyverse)
library(lme4)
library(car)
library(cowplot)

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

ggplot(data_raw, aes(x=lichen_cover_percent)) +
  geom_histogram() +
  facet_wrap(~height, ncol=1)


test_model_bryo <- glmer.nb(bryo_cover~quadrat_side+height+ (1|tree_id), data = data_raw)
Anova(test_model_bryo, Type = "II")

bryo_height_comps <- emmeans(test_model_bryo, specs = ~ height, adjust = "Tukey")
contrast(bryo_height_comps, method = "pairwise")
bryo_side_comps <- emmeans(test_model_bryo, specs = ~ quadrat_side, adjust = "Tukey")
contrast(bryo_side_comps, method = "pairwise")
##Lichen
test_model_lichen <- glmer.nb(lichen_cover_percent~quadrat_side+height+ (1|tree_id), data = data_raw) 
Anova(test_model_lichen, Type = "II")
lichen_height_comps <- emmeans(test_model_lichen, specs = ~ height, adjust = "Tukey")
contrast(lichen_height_comps, method = "pairwise")
lichen_side_comps <- emmeans(test_model_lichen, specs = ~ quadrat_side, adjust = "Tukey")
contrast(lichen_side_comps, method = "pairwise")

######Plots
# For the plots, I want to summarize by tree ID in advance
#First for height
#First bryo
## Jitter plot below
data_raw %>% 
  group_by(tree_id, height) %>% 
  summarize(mean_moss_percent = mean(bryo_cover_percent, na.rm = TRUE),
            mean_lichen_percent = mean(lichen_cover_percent, na.rm = TRUE),
            neighbourhood = first(neighbourhood)) %>% 
  ggplot(aes(x=height, y = mean_moss_percent)) +
  geom_jitter(width = 0.2, alpha = 0.15) +
  stat_summary(fun = "mean", geom = "point", colour = "red", size =2.5) +
  labs(x="Quadrat height",
       y = "Percent cover of bryophytes") +
  geom_segment(x=2,y=85,xend=3,yend=85) +
  geom_segment(x=1,y=95,xend=3,yend=95) +
  geom_segment(x=1, y=75, xend=2,yend=75) +
  geom_text(x=2,y=98,label="P=0.0034") + 
  geom_text(x=2.5,y=88,label="P=0.35") +
  geom_text(x=1.5,y=78, label = "P=0.10") +
  theme_classic() +
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)))

## Bar plot below
bryo_height_plot <- data_raw %>% 
  group_by(tree_id, height) %>% 
  summarize(mean_moss_percent = mean(bryo_cover_percent, na.rm = TRUE),
            mean_lichen_percent = mean(lichen_cover_percent, na.rm = TRUE),
            neighbourhood = first(neighbourhood)) %>% 
  ggplot(aes(x=height, y = mean_moss_percent, fill = height)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom="errorbar", width = 0.15) +
  labs(x="Quadrat height",
       y = "Percent cover of bryophytes (%)") +
  coord_cartesian(ylim = c(0, 21)) +
  #geom_segment(x=1,y=17,xend=2,yend=17) +
  #geom_segment(x=1,y=20,xend=3,yend=20) +
  #geom_segment(x=2, y=18.5, xend=3,yend=18.5) +
  #annotate(geom = "text", x=2,y=20.7,label="P = 0.0034 ***") +
  #annotate(geom = "text", x=1.5,y=17.7,label="P = 0.10") +
  #annotate(geom = "text", x=2.5,y=19.2,label="P = 0.35") +
  annotate(geom = "text", x=1,y=16.7, label = "a")+
  annotate(geom = "text", x=2,y=15.7, label = "ab")+
  annotate(geom = "text", x=3,y=13.1, label = "b")+
  scale_fill_manual(values = c("#08308a", "#4292c6", "#c6dbef")) +
  theme_classic() +
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)),
        legend.position = "none")
bryo_height_plot

#Same bar plot for lichen
lichen_height_plot <- data_raw %>% 
  group_by(tree_id, height) %>% 
  summarize(mean_moss_percent = mean(bryo_cover_percent, na.rm = TRUE),
            mean_lichen_percent = mean(lichen_cover_percent, na.rm = TRUE),
            neighbourhood = first(neighbourhood)) %>% 
  ggplot(aes(x=height, y = mean_lichen_percent, fill = height)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom="errorbar", width = 0.15) +
  labs(x="Quadrat height",
       y = "Percent cover of lichens (%)") +
  #coord_cartesian(ylim = c(0, 41)) +
  #geom_segment(x=1,y=32,xend=2,yend=32) +
  #geom_segment(x=1,y=36,xend=3,yend=36) +
  #geom_segment(x=2, y=34, xend=3,yend=34) +
  #annotate(geom = "text", x=2,y=37,label="P = 0.0004 ***") +
  #annotate(geom = "text", x=1.5,y=33,label="P = 0.17") +
  #annotate(geom = "text", x=2.5,y=35,label="P = 0.10") +
  annotate(geom = "text", x=1,y=32.2, label = "a")+
  annotate(geom = "text", x=2,y=30.9, label = "ab")+
  annotate(geom = "text", x=3,y=27.1, label = "b")+
  scale_fill_manual(values = c("#08308a", "#4292c6", "#c6dbef")) +
  theme_classic() +
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)),
        legend.position = "none")
lichen_height_plot


## Lichen Jitter plot below
data_raw %>% 
  group_by(tree_id, height) %>% 
  summarize(mean_moss_percent = mean(bryo_cover_percent, na.rm = TRUE),
            mean_lichen_percent = mean(lichen_cover_percent, na.rm = TRUE),
            neighbourhood = first(neighbourhood)) %>% 
  ggplot(aes(x=height, y = mean_lichen_percent)) +
  geom_jitter(width = 0.2, alpha = 0.15) +
  stat_summary(fun = "mean", geom = "point", colour = "red", size =2.5) +
  labs(x="Quadrat height",
       y = "Percent cover of lichen") +
  geom_segment(x=1,y=15,xend=2,yend=15) +
  geom_segment(x=1,y=19,xend=3,yend=19) +
  geom_segment(x=2, y=17, xend=3,yend=17) +
  annotate(geom = "text", x=2,y=19.5,label="P = 1.00 (N.S)") +
  annotate(geom = "text", x=1.5,y=17.5,label="P = 0.62 (N.S)") +
  annotate(geom = "text", x=2.5,y=19.5,label="P = 0.98 (N.S)") +
  theme_classic() +
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)))

#now road side
bryo_road_plot <- ggplot(filter(data_raw, quadrat_side != is.na(quadrat_side)), 
                         aes(x=quadrat_side, y = bryo_cover_percent, fill = quadrat_side)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom="errorbar", width = 0.15) +
  labs(x="Side of tree",
       y = "Percent cover of bryophytes (%)") +
  theme_classic() +
  scale_fill_manual(values = c("#beaed4", "#fdc086", "#7fc97f"))+
  #geom_segment(x=1,y=15,xend=2,yend=15) +
  #geom_segment(x=1,y=18,xend=3,yend=18) +
  #geom_segment(x=2, y=16.5, xend=3,yend=16.5) +
  #annotate(geom = "text", x=2,y=18.8,label="P = 0.39") +
  #annotate(geom = "text", x=1.5,y=15.8,label="P = 0.62") +
  #annotate(geom = "text", x=2.5,y=17.3,label="P = 0.98 ") +
  annotate(geom = "text", x=1,y=12.5, label = "a")+
  annotate(geom = "text", x=2,y=14.6, label = "a")+
  annotate(geom = "text", x=3,y=15.2, label = "a")+
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)),
        legend.position = "none")
bryo_road_plot

#Road side for lichen

lichen_road_plot <- ggplot(filter(data_raw, quadrat_side != is.na(quadrat_side)), 
                         aes(x=quadrat_side, y = lichen_cover_percent, fill = quadrat_side)) +
  stat_summary(fun = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_se", geom="errorbar", width = 0.15) +
  labs(x="Side of tree",
       y = "Percent cover of lichens (%)") +
  theme_classic() +
  scale_fill_manual(values = c("#beaed4", "#fdc086", "#7fc97f"))+
  #coord_cartesian(ylim = c(0,36))+
  #geom_segment(x=1,y=31,xend=2,yend=31) +
  #geom_segment(x=1,y=36,xend=3,yend=36) +
  #geom_segment(x=2, y=33.5, xend=3,yend=33.5) +
  #annotate(geom = "text", x=2,y=36.7,label="P = 1.00") +
  #annotate(geom = "text", x=1.5,y=31.7,label="P = 0.015 *") +
  #annotate(geom = "text", x=2.5,y=34.2,label="P = 0.0027 ***") +
  annotate(geom = "text", x=1,y=30.5, label = "a")+
  annotate(geom = "text", x=2,y=26.2, label = "b")+
  annotate(geom = "text", x=3,y=30.5, label = "a")+
  theme(axis.text.x = element_text(margin = margin(t=2, b=5)),
        axis.text.y = element_text(margin = margin(l=5, r=2)),
        legend.position = "none")
lichen_road_plot

#Combine plots
final_plot <- plot_grid(bryo_height_plot, bryo_road_plot, lichen_height_plot, lichen_road_plot, labels = c("A", "B", "C", "D"))
final_plot
ggsave("Figures/height_and_roadside_plot.png", final_plot, units = "cm", height = 19, width =19)
