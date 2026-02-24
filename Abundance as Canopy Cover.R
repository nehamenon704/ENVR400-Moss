ggplot(all_trees_total, aes(x = GENUS_NAME, y = total_abundance, fill = GENUS_NAME)) +
  geom_boxplot(outlier.shape = NA) + # Matches the clean look of your image
  facet_wrap(~Cover_Type) +         # This creates the two side-by-side panels
  theme_bw() +                      # Gives that clean white background with a border
  labs(
    y = "Total Epiphyte Abundance (% Cover)",
    x = NULL,                       # Removes the Genus label from bottom as it's in the legend
    fill = "Genus",
    title = "Total Epiphyte Abundance at Neighbourhood-Level Canopy Cover"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) + # Adds % sign to Y axis
  theme(
    strip.background = element_blank(),     # Removes gray boxes from titles
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_blank()           # Removes x-axis text since legend identifies colors
  )
