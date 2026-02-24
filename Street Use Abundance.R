street_use_plot_data <- all_trees_total %>%
  filter(!is.na(STREETUSE), !is.na(GENUS_NAME)) %>%
  mutate(STREET_TYPE_SIMPLE = case_when(
    STREETUSE %in% c("Arterial", "Secondary Arterial") ~ "Arterial",
    STREETUSE == "Residential" ~ "Residential",
    TRUE ~ STREETUSE # Keeps any other categories if they exist
  )) %>%
  # Set the order so Residential is on the left
  mutate(STREET_TYPE_SIMPLE = factor(STREET_TYPE_SIMPLE, levels = c("Residential", "Arterial")))

# 2. Create the plot
ggplot(street_use_plot_data, aes(x = STREET_TYPE_SIMPLE, y = total_abundance, fill = STREET_TYPE_SIMPLE)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.4) +
  # This creates one sub-plot per Genus
  facet_wrap(~GENUS_NAME, scales = "free_y") + 
  theme_minimal() +
  labs(
    title = "Epiphyte Abundance by Street Type and Genus",
    x = "Street Classification",
    y = "Total Abundance (% Cover)",
    fill = "Street Type"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
