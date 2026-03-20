final_data <- read.csv("final_data.csv")

#Want basic summary statistics for moss and lichen
summary_table <- final_data %>% 
  group_by(NEIGHBOURHOOD_NAME, Road, GENUS_NAME) %>% 
  summarize(mean_lichen_percent = mean(mean_lichen_cover, na.rm = TRUE)/25*100,
            mean_bryophyte_percent = mean(mean_bryo_cover, na.rm = TRUE)/25*100,
            mean_visually_distinct_taxa = mean(mean_species_count, na.rm = TRUE)/25*100)
write.csv(summary_table, "Final Tables/summary_table.csv")

#Curious to know the number of tree species represented by each genus
tree_summary <- final_data %>% 
  filter(road_side != "removal") %>% 
  group_by(GENUS_NAME) %>% 
  summarize(n_species = n_distinct(SPECIES_NAME),
            n_cultivars = n_distinct(COMMON_NAME))

#Curious to see if certain species are associated with certain road types
# Potential confounding variable

#For carpinus
carpinus_treediversity_plot <- final_data %>% 
  filter(GENUS_NAME == "CARPINUS") %>% 
  ggplot(aes(x=SPECIES_NAME)) +
  geom_bar(stat = "count", fill = "darkgrey") +
  facet_wrap(~Road+NEIGHBOURHOOD_NAME) +
  labs(x="Carpinus species",
       y="Number of trees sampled")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
carpinus_treediversity_plot
ggsave("Figures/carpinus_treediversity_plot.png", carpinus_treediversity_plot, width = 6, height = 7)
#This does not suggest confoudning since the vast majority are one species on both residential and arterial roads

#For prunus
prunus_treediversity_plot <- final_data %>% 
  filter(GENUS_NAME == "PRUNUS") %>% 
  ggplot(aes(x=SPECIES_NAME)) +
  geom_bar(stat = "count", fill = "darkgrey") +
  facet_wrap(~Road+NEIGHBOURHOOD_NAME) +
  labs(x="Prunus species",
       y="Number of trees sampled")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
prunus_treediversity_plot
ggsave("Figures/prunus_treediversity_plot.png", prunus_treediversity_plot, width = 6, height = 7)
#Much more cerasifera on residential roads and much more X. Yedoensis and Sargentii on arterial

#For Acer
acer_treediversity_plot <- final_data %>% 
  filter(GENUS_NAME == "ACER") %>% 
  ggplot(aes(x=SPECIES_NAME)) +
  geom_bar(stat = "count", fill = "darkgrey") +
  facet_wrap(~Road+NEIGHBOURHOOD_NAME) +
  labs(x="Acer species",
       y="Number of trees sampled")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
acer_treediversity_plot
ggsave("Figures/acer_treediversity_plot.png", acer_treediversity_plot, width = 6, height = 7)
#Rubrum on residential and platanoides on arterial. Generally more diversity on residential

###Now to do the same thing for neighbourhood

