ggplot(all_trees_total, aes(x = GENUS_NAME, y = total_abundance, fill = GENUS_NAME)) +
  geom_boxplot(color = "black") +
  theme_minimal() +
  labs(title = "Total Epiphyte Abundance by Genus",
       x = "Genus Name",
       y = "Total Abundance (%)") +
  theme(legend.position = "none")
  