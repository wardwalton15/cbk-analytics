library(tidyverse)
library(gt)
library(gtExtras)

data <- read_csv("C:/Users/drawn/Documents/CBK-2025-26/cbk-analytics/jan risers and fallers - Sheet1.csv")
view(data)
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(logos)
data <- data %>%
  mutate(jan_net = janoe-jande,
   pre_net = preoe-prede,
   diff = jan_net - pre_net)

data <- data %>%
  left_join(logos, by = c("Team" = "team")) %>%
    select(logo, Team, c(8:10))

#top january risers table
top_risers <- data %>%
    filter(pre_net >= 10) %>%
  arrange(desc(diff)) %>%
  slice_head(n = 10)

head(top_risers)

top_risers_gt <- top_risers %>%
  gt() %>%
  
  # Add team logos
  gt_img_rows(columns = logo, height = 35) %>%
  
  # Rename columns for display
  cols_label(
    logo = "",
    Team = "Team",
    pre_net = "Thru Dec. 31st",
    jan_net = "Since January 1st",
    diff = "Change"
  ) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(pre_net, jan_net, diff),
    decimals = 1,
    use_seps = FALSE
  ) %>%
  
  # Add plus sign to positive diff values
  text_transform(
    locations = cells_body(columns = diff),
    fn = function(x) paste0("+", x)
  ) %>%
  
  # Color the diff column with a gradient
  data_color(
    columns = diff,
    palette = c("#e8f5e9", "#1b5e20"),
    domain = c(min(top_risers$diff), max(top_risers$diff))
  ) %>%
  
  # Style the table
  tab_header(
    title = md("**📈 Biggest Risers**"),
    subtitle = "Teams with the largest net rating improvement since Jan 1st"
  ) %>%
  
  # Add source note
  tab_source_note(
    source_note = md("* Chart: Ward Walton | Data: bartorvik.com*")
  ) %>%
  
  # Column alignment
  cols_align(
    align = "center",
    columns = c(pre_net, jan_net, diff)
  ) %>%
  cols_align(
    align = "left",
    columns = Team
  ) %>%
  
  # Column widths
  cols_width(
    logo ~ px(50),
    Team ~ px(140),
    pre_net ~ px(90),
    jan_net ~ px(90),
    diff ~ px(80)
  ) %>%
  
  # Table styling
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))
    ),
    locations = cells_body(columns = diff)
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = Team)
  ) %>%
  
  # Add borders
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "#d3d3d3",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  
  # Header styling
  tab_style(
    style = list(
      cell_fill(color = "#1a1a2e"),
      cell_text(color = "white", weight = "bold", size = px(18))
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#1a1a2e"),
      cell_text(color = "#f1eded", size = px(12))
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Column header styling
  tab_style(
    style = list(
      cell_fill(color = "#f5f5f5"),
      cell_text(color = "#333333")
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Overall table options

  tab_options(
    table.font.size = px(13),
    table.font.names = "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
    heading.align = "left",
    heading.padding = px(12),
    column_labels.padding = px(10),
    data_row.padding = px(8),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.border.top.color = "#d3d3d3",
    table_body.border.bottom.color = "#d3d3d3",
    source_notes.font.size = px(11),
    source_notes.padding = px(8)
  )

# Display the table
top_risers_gt

library(webshot2)

# Save as PNG (requires webshot2)
gtsave(top_risers_gt, "top_risers_table.png", vwidth = 550, vheight = 600)

top_fallers <- data %>%
    filter(pre_net >= 15) %>%
  arrange((diff)) %>%
  slice_head(n = 10)

head(top_fallers)

top_fallers_gt <- top_fallers %>%
  gt() %>%
  
  # Add team logos
  gt_img_rows(columns = logo, height = 35) %>%
  
  # Rename columns for display
  cols_label(
    logo = "",
    Team = "Team",
    pre_net = "Thru Dec. 31st",
    jan_net = "Since January 1st",
    diff = "Change"
  ) %>%
  
  # Format numeric columns
  fmt_number(
    columns = c(pre_net, jan_net, diff),
    decimals = 1,
    use_seps = FALSE
  ) %>%
  
  
  # Color the diff column with a gradient
  data_color(
    columns = diff,
    palette = c("#e8f5e9", "#ba2f1c"),
    domain = c(max(top_risers$diff), min(top_risers$diff))
  ) %>%
  
  # Style the table
  tab_header(
    title = md("**Biggest Fallers**"),
    subtitle = "Teams with the largest net rating decrease since Jan 1st"
  ) %>%
  
  # Add source note
  tab_source_note(
    source_note = md("* Chart: Ward Walton | Data: bartorvik.com*")
  ) %>%
  
  # Column alignment
  cols_align(
    align = "center",
    columns = c(pre_net, jan_net, diff)
  ) %>%
  cols_align(
    align = "left",
    columns = Team
  ) %>%
  
  # Column widths
  cols_width(
    logo ~ px(50),
    Team ~ px(140),
    pre_net ~ px(90),
    jan_net ~ px(90),
    diff ~ px(80)
  ) %>%
  
  # Table styling
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14))
    ),
    locations = cells_body(columns = diff)
  ) %>%
  
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = Team)
  ) %>%
  
  # Add borders
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "#d3d3d3",
      weight = px(1)
    ),
    locations = cells_body()
  ) %>%
  
  # Header styling
  tab_style(
    style = list(
      cell_fill(color = "#1a1a2e"),
      cell_text(color = "white", weight = "bold", size = px(18))
    ),
    locations = cells_title(groups = "title")
  ) %>%
  
  tab_style(
    style = list(
      cell_fill(color = "#1a1a2e"),
      cell_text(color = "#f1eded", size = px(12))
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  
  # Column header styling
  tab_style(
    style = list(
      cell_fill(color = "#f5f5f5"),
      cell_text(color = "#333333")
    ),
    locations = cells_column_labels()
  ) %>%
  
  # Overall table options

  tab_options(
    table.font.size = px(13),
    table.font.names = "system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif",
    heading.align = "left",
    heading.padding = px(12),
    column_labels.padding = px(10),
    data_row.padding = px(8),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.border.top.color = "#d3d3d3",
    table_body.border.bottom.color = "#d3d3d3",
    source_notes.font.size = px(11),
    source_notes.padding = px(8)
  )

# Display the table
top_fallers_gt

library(webshot2)

# Save as PNG (requires webshot2)
gtsave(top_risers_gt, "top_risers_table.png", vwidth = 550, vheight = 600)

