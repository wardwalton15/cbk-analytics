library(tidyverse)
library(gt)

data <- read_csv("C:/Users/drawn/Documents/CBK-2025-26/cbk-analytics/ncsos - Sheet1.csv")
view(data)
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(logos)
data <- data %>%
  left_join(logos, by = c("Team" = "team")) %>%
    select(logo, Team, c(3)) %>% head(20)

tbl <- data %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = logo),
    fn = function(x) {
      web_image(url = x, height = 30)
    }
  ) %>%
  
  # --- 2. LABELS & HEADERS ---
  cols_label(
    logo = "", # Remove the column header for the logo
    Team = "Team",
    NETNCSOS = "Non-Conf SOS Rank"
  ) %>%
  tab_header(
    title = md("**NCAA Team Efficiency Metrics**"),
    subtitle = "Analysis of NET Non-Conference Strength of Schedule"
  )%>%
  data_color(
    columns = NETNCSOS,
    method = "numeric",
    palette = c("#69bf69", "#ce9c3f", "#c74c4c"),
    reverse = FALSE # Typically lower SOS rank (1) is 'harder', red usually implies 'hot/high'. 
                    # Adjust colors based on if you want low numbers to be red or green.
  ) %>%
  
  # Align the logo center, text left, numbers right
  cols_align(align = "center", columns = logo) %>% 
  
  # --- 4. THEME & STYLE ---
  # Apply a clean, thick-border style
  opt_table_lines(extent = "default") %>% 
  # Use Google Fonts for a presentation look
  opt_table_font(
    font = google_font(name = "Roboto Condensed")
  ) %>%
  # Tweak specific options for "Presentation" polish
  tab_options(
    heading.title.font.size = px(24),
    heading.subtitle.font.size = px(16),
    column_labels.font.weight = "bold",
    column_labels.background.color = "#f2f2f2",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    data_row.padding = px(6)
  )

gtsave(tbl, "ncsos_table.png")
