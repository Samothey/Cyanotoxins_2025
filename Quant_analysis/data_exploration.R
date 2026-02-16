
source("C:/Users/spena5/Desktop/Project/Brooks_lake_2025/CODE/cyanotoxin/Cyanotoxins_2025/class_scripts/load_packages.r")


grab_totals <- read_rds("C:/Users/spena5/Desktop/Project/Brooks_lake_2025/CODE/cyanotoxin/Cyanotoxins_2025/grab_class_totals.rds")

brooks_tox <- grab_totals %>%
  filter(lake == "brooks",
    toxin_class == "microcystin")

# toxin distribution in brooks lake 
ggplot(brooks_tox, aes(x = "", y = total)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, fill = "grey80") +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2, color = "grey30") +
  labs(
    title = "Brooks Microcystin Distribution",
    x = "",
    y = "Total Microcystin (µg/L)"
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank()
  )

#toxin distribution shore vs buoy 
ggplot(brooks_tox, aes(x = site_type, y = total)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.4, fill = "grey80") +
  geom_jitter(aes(color = site_type), width = 0.15, alpha = 0.7, size = 2) +
  scale_color_manual(
    values = c(
      shore = "salmon",
      buoy  = "green4"
    )
  ) +
  labs(
    x = "Site Type",
    y = "Total Microcystins (µg/L)",
    title = "Microcystin Distribution by Site Type in Brooks Lake"
  ) +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )



## distribution by site-ID
ggplot(brooks_tox, aes(x = site_id, y = total)) +
  geom_boxplot(outlier.shape = NA, fill = "grey80", alpha = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.7, size = 2, color = "grey30") +
  labs(
    x = "Site ID",
    y = "Total Microcystins (µg/L)",
    title = "Microcystin Distribution by Site — Brooks Lake"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



ggplot(brooks_tox, aes(x = total, fill = site_type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  labs(
    title = "Distribution of Microcystin by Site Type",
    x = "Total Microcystin (µg/L)",
    y = "Count"
  ) +
  theme_minimal()

  
ggplot(brooks_tox, aes(x = date, y = total)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_line(aes(group = site_id), alpha = 0.4) +
  labs(
    title = "Microcystin Concentrations Over Time — Brooks Lake",
    x = "Date",
    y = "Total Microcystin (µg/L)"
  ) +
  theme_classic()


ggplot(brooks_tox, aes(date, total, color = site_type)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(se = FALSE, method = "loess", span = 0.6) +
  labs(
    title = "Microcystin Trends by Site Type",
    x = "Date",
    y = "Total Microcystin (µg/L)",
    color = "Site Type"
  ) +
  theme_classic()

  
  
  
  
  
  