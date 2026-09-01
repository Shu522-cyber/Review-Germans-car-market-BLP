

df17 <- data_list[["2017"]]
elas17 <- elasticities_list[["2017"]]

target_idx <- which(
  (df17$brand == "Fiat"           & df17$model == "500C"      & df17$fueltype == "gasoline" & df17$kw == 5.1) |
    (df17$brand == "Fiat"           & df17$model == "Panda"     & df17$fueltype == "gasoline" & df17$kw == 5.1) |
    (df17$brand == "Abarth"         & df17$model == "595C/695C" & df17$fueltype == "gasoline" & df17$kw == 13.2) |
    (df17$brand == "Peugeot"        & df17$model == "208"       & df17$fueltype == "gasoline" & df17$kw == 6.0) |
    (df17$brand == "Citroen"        & df17$model == "C3"        & df17$fueltype == "gasoline" & df17$kw == 6.0) |
    (df17$brand == "DS Automobiles" & df17$model == "DS 3"      & df17$fueltype == "gasoline" & df17$kw == 8.1) |
    (df17$brand == "Alfa Romeo"     & df17$model == "Giulietta" & df17$fueltype == "gasoline" & df17$kw == 8.8) |
    (df17$brand == "Peugeot"        & df17$model == "308"       & df17$fueltype == "gasoline" & df17$kw == 9.6) |
    (df17$brand == "Citroen"        & df17$model == "C4"        & df17$fueltype == "gasoline" & df17$kw == 9.6) |
    (df17$brand == "Jeep"           & df17$model == "Renegade"  & df17$fueltype == "gasoline" & df17$kw == 10.3) |
    (df17$brand == "Peugeot"        & df17$model == "3008"      & df17$fueltype == "gasoline" & df17$kw == 12.1) |
    (df17$brand == "Fiat"           & df17$model == "500X"      & df17$fueltype == "gasoline" & df17$kw == 8.1)
)

elasticities_17 <- elas17[target_idx, target_idx]

# 5-4: Visualization 

# STEP 1:
car_class_lookup <- data_list[["2017"]] %>%
  filter(paste(brand, model, fueltype, kw, sep = "_") %in% rownames(elasticities_17)) %>%
  transmute(id_t = paste(brand, model, fueltype, kw, sep = "_"), class) %>%
  distinct()

# STEP 2: 
ordered_ids <- car_class_lookup %>%
  arrange(class, id_t) %>%
  pull(id_t)

# STEP 3:
elas_df <- as.data.frame(elasticities_17) %>%
  rownames_to_column("row_car") %>%
  pivot_longer(-row_car, names_to = "col_car", values_to = "elasticity") %>%
  mutate(
    row_car = factor(row_car, levels = ordered_ids),
    col_car = factor(col_car, levels = ordered_ids),
    
    elasticity_trans = sign(elasticity) * log1p(abs(elasticity))
  )

# STEP 4:
ggplot(elas_df, aes(x = col_car, y = row_car, fill = elasticity_trans)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.3f", elasticity)), size = 2.3, color = "black") +
  scale_fill_gradient2(
    low = "steelblue", mid = "white", high = "firebrick",
    midpoint = 0, name = "elasticity\n(log-scaled)"
  ) +
  scale_y_discrete(limits = rev) +   
  labs(
    title = "2017 FCA-PSA Case：Cross-Price Elasticity Matrix basing on nested classess ordering",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave(
  "output/Figures/elasticity_heatmap_2017.png",
  width = 10,
  height = 8,
  dpi = 300
)