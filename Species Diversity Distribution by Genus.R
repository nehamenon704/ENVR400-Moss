library(ggplot2)
library(dplyr)

# 2. Create the Violin + Jitter Plot
ggplot(all_trees_total, aes(x = GENUS_NAME, y = total_species, fill = GENUS_NAME)) +
  # The 'Violin' shows the distribution density
  geom_violin(alpha = 0.3, color = NA) +
  # 'Jitter' shows every individual tree point
  geom_jitter(aes(color = GENUS_NAME), width = 0.2, alpha = 0.5, size = 1.5) +
  facet_wrap(~NEIGHBOURHOOD_NAME) +
  labs(
    title = "Species Diversity Distribution by Genus",
    x = "Tree Genus",
    y = "Number of Distinct Species",
    fill = "Genus",
    color = "Genus"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none", # Hide legend since X-axis labels are clear
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )
