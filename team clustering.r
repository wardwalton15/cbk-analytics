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
k <- 9
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
  "1" = "test",
  "2" = "High Octane",
  "3" = "Defensive Grinders",
  "4" = "Balanced Teams",
  "5" = "Three-Point Specialists",
  "6" = "Rebounding Focused",
  "7" = "Title Contenders",
  "8" = "Fast Paced Offense",
  "9" = "Playmakers"
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
table_data %>%
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
  opt_align_table_header(align = "left") %>%
  tab_options(
    table.width = px(600),
    data_row.padding = px(10)
  )
