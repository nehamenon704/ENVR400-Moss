plot_data_fig6 <- all_trees_total %>%
  filter(grepl("Kitsilano", NEIGHBOURHOOD_NAME, ignore.case = TRUE) | 
           grepl("Renfrew-Collingwood", NEIGHBOURHOOD_NAME, ignore.case = TRUE))

# 2. Create the Stacked Histogram
ggplot(plot_data_fig6, aes(x = total_species, fill = NEIGHBOURHOOD_NAME)) +
  # geom_histogram with position = "stack" (default)
  # bins = 30 is standard, but you can adjust this based on your data range
  geom_histogram(position = "stack", binwidth = 1, color = "white", alpha = 0.9) +
  # Automatic professional color palette
  scale_fill_manual(values = c("RENFREW-COLLINGWOOD" = "#2c7fb8", "KITSILANO" = "#feb24c")) +
  labs(
    title = "Distribution of Species Counts by Neighbourhood",
    x = "Number of Distinct Species",
    y = "Count of Trees",
    fill = "Neighbourhood"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
