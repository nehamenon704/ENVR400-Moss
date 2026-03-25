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

###Want to compare lichen and bryocover at a tree level
ggplot(final_data, aes(x=mean_lichen_cover, y=mean_bryo_cover)) +
  geom_point() #Huh a bit of a negative relationship here

#compare species counts to percent covers
final_data <- mutate(final_data, total_cover = mean_lichen_cover+mean_bryo_cover)
long_final_data <- final_data %>% 
  pivot_longer(5:6, names_to = "taxa", values_to = "cover")
long_final_data$taxa <- as.factor(long_final_data$taxa)
levels(long_final_data$taxa) <- c("Bryophytes", "Lichens")
  
richness_abundance_plot <- ggplot(final_data, aes(y=mean_species_count, x=total_cover)) +
  geom_point() +
  labs(x= "Epiphyte percent cover (%)",
       y="Mean count of 'visually distinct taxa per quadrat") +
  annotate(geom="text", x=6.5,y=3.5, label = "Pearson's r = 0.81")+
  annotate(geom="text", x=6.5,y=3.3, label = "P < 2e-16 ***")+
  theme_classic()
richness_abundance_plot
ggsave("Figures/richness_abundance_plot.png", richness_abundance_plot, height = 5.5, width = 5.5)

cor.test(final_data$mean_species_count, final_data$total_cover)
#r=0.808, P<2e-16

#separate bryos and lichens
ggplot(long_final_data, aes(x=cover,y=mean_species_count, colour = taxa)) +
  geom_point() +
  theme_classic() +
  labs(x="Percent cover (%)",
       y="Count of 'visually distinct taxa",
       colour = "Epiphyte type")+
  scale_colour_manual(values = c("chartreuse4", "cadetblue3")) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75,0.13),
        legend.background = element_rect(fill = "white", colour = "black"))
