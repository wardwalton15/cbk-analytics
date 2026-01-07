library(tidyverse)
data <- read_csv("torvik team tables - Sheet1.csv")
view(data)

style_features <- data %>%
  mutate(team = coalesce(`Team...42`, `Team...2`)) %>%
  select(
    team,
    Barthag,
    adjusted_tempo = `Adj. T`,
    efg = eFG,
    ft_rate = `FT Rate`,
    tov_pct = `TOV%`,
    oreb_pct = `O Reb%`,
    three_pa_rate = `3P Rate`,
    assist_rate = `Ast %`
  ) %>%
  arrange(desc(Barthag)) %>%
  slice_head(n = 100) %>%
  drop_na()

feature_cols <- c(
  "adjusted_tempo",
  "efg",
  "ft_rate",
  "tov_pct",
  "oreb_pct",
  "three_pa_rate",
  "assist_rate"
)

scaled_features <- style_features %>%
  select(all_of(feature_cols)) %>%
  scale()

set.seed(15)
k <- 8
kmeans_fit <- kmeans(scaled_features, centers = k, nstart = 25)
## elbow plot
wss <- map_dbl(1:15, function(k) {
  kmeans(scaled_features, centers = k, nstart = 10)$tot.withinss
})
elbow_data <- tibble(
  k = 1:15,
  wss = wss
)
ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Elbow Method for Optimal k",
    x = "Number of clusters (k)",
    y = "Total Within-Cluster Sum of Squares (WSS)"
  ) +
  theme_minimal()


clustered_teams <- style_features %>%
  mutate(cluster = kmeans_fit$cluster)

cluster_summary <- clustered_teams %>%
  group_by(cluster) %>%
  summarise(
    teams = n(),
    across(all_of(feature_cols), mean, .names = "{.col}_mean"),
    .groups = "drop"
  )

view(clustered_teams)
view(cluster_summary)

data <- cluster_summary %>%
  pivot_longer(
    cols = ends_with("_mean"),
    names_to = "feature",
    values_to = "mean_value"
  ) %>%
  mutate(feature = str_remove(feature, "_mean")) %>%
  # Crucial step: group by feature so scaling happens per variable
  group_by(feature) %>% 
  mutate(mean_value_scaled = as.numeric(scale(mean_value))) %>%
  ungroup()

ggplot(data, aes(x = feature, y = factor(cluster), fill = mean_value_scaled)) +
  geom_tile(color = "white") + # Added white borders for better separation
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0
  ) +
  theme_minimal() +
  labs(
    title = "Cluster Feature Means (Relative to Feature Average)",
    subtitle = "Red = Above Average | Blue = Below Average",
    x = "Feature",
    y = "Cluster",
    fill = "Z-Score"
  )



print(cluster_summary)

logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")

clustered_teams_logos <- clustered_teams %>%
  left_join(logos, by = c("team" = "team")) %>%
  filter(Barthag >= .85) %>%
  select(team, cluster, logo)
view(clustered_teams_logos)
library(gt)
head(clustered_teams_logos)

# 1. Define your cluster names
# Create a named vector or a lookup table to map numbers to names
cluster_map <- c(
  "1" = "Deliberate and Efficient",
  "2" = "Old School: Slow and Inside",
  "3" = "Dominate on the Interior",
  "4" = "Pace-and-Space NBA Style",
  "5" = "Offensive Glass Cleaners",
  "6" = "Isolation Specialists",
  "7" = "Aggressive Slashers",
  "8" = "Half-Court Set Shooters"
)

# 2. Prepare the data for the table
table_data <- clustered_teams_logos %>%
  mutate(cluster_name = cluster_map[as.character(cluster)]) %>%
  group_by(cluster_name) %>%
  summarize(
    # Create a string of HTML image tags for all logos in the cluster
    logos_html = paste0(
      '<img src="', logo, '" style="height:30px; margin-right:5px; vertical-align:middle;">', 
      collapse = ""
    )
  ) %>%
  # Optional: Arrange by cluster name or a specific order
  arrange(cluster_name)

# 3. Create the gt table
tbl <- table_data %>%
  gt() %>%
  # This tells gt to render the HTML strings as actual images
  fmt_markdown(columns = logos_html) %>%
  # Label the columns nicely
  cols_label(
    cluster_name = "Cluster",
    logos_html = "Teams"
  ) %>%
  # Optional: Table styling
  tab_header(title = "College Basketball Offensive Play Styles") %>%
  opt_align_table_header(align = "center") %>%
  tab_options(
    table.width = px(600),
    data_row.padding = px(10)
  )
tbl
gtsave(tbl, "team_clusters_table.png")

table_data_wide <- clustered_teams_logos %>%
  filter(cluster %in% c(1, 4)) %>%
  mutate(cluster_label = ifelse(cluster == 1, "Deliberate and Efficient", "Pace-and-Space NBA Style")) %>%
  group_by(cluster_label) %>%
  # Create a sequence number (1, 2, 3...) for each team in the cluster
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  # Keep only the columns we need for the pivot
  select(row_id, cluster_label, logo) %>%
  # Pivot so that each cluster becomes its own column
  pivot_wider(names_from = cluster_label, values_from = logo) %>%
  # Remove the row_id column as it's no longer needed for display
  select(-row_id)

# 3. Create the gt table
tbl2 <- table_data_wide %>% head(5) %>%
  gt() %>%
  # Transform the URLs in both columns into actual images
  # 'columns = everything()' applies it to both cluster name columns
  fmt_image(
    columns = everything(),
    height = 40 # Adjust the size of the logos here
  ) %>%
  tab_header(title = "Old School vs New School") %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "#D3D3D3", # Light grey color
      weight = px(2)    # Thickness of the line
    ),
    locations = list(
      cells_body(columns = 1),          # Adds line to the body cells of the 1st column
      cells_column_labels(columns = 1)  # Adds line to the header of the 1st column
    ) )%>%
  sub_missing(columns = everything(), missing_text = "") %>% # Hide NA if clusters are uneven
  tab_options(
    column_labels.font.weight = "bold",
    table.width = px(400),
    data_row.padding = px(5)
  ) %>%
  cols_align(align = "center")
tbl2
  gtsave(tbl2, "old_vs_new_school.png")
