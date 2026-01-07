library(tidyverse)

data <- read_csv("efg% - Sheet1.csv")
view(data)
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(logos)
data <- data %>%
  left_join(logos, by = c("Team" = "team")) %>%
    select(Rank, logo, Team, c(6)) %>%
    arrange(desc(eFG))

head(data)
library(gt)

top_15_data <- head(data, 15)

# 2. Create the formatted table
table_plot <- top_15_data %>%
  gt() %>%
  # Render the 'logo' column as images instead of text URLs
  text_transform(
    locations = cells_body(columns = logo),
    fn = function(x) {
      web_image(
        url = x,
        height = 30 # Adjust size as needed
      )
    }
  ) %>%

  data_color(
    columns = Rank,
    fn = scales::col_numeric(
      palette = c("#34C759", "#FF3B30"), # Apple-style nice Green to Red
      domain = c(1, 365)
    )
  ) %>%
  # Make the column labels nice
  cols_label(
    logo = "", # Hide the header for the logo column
    eFG = "eFG%"
  ) %>%

  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(16), align = "center")
    ),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", size = px(18), align = "center"),
    locations = cells_column_labels()
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%

  tab_header(
    title = md("**Top 15 Teams by eFG%**"),
    subtitle = "2025-26 NCAA Rankings"
  ) %>%
  
  opt_table_lines(extent = "default") %>%
  tab_options(
    table.background.color = "white",
    heading.title.font.size = px(24),
    column_labels.background.color = "#f2f2f2"
  )


table_plot


gtsave(table_plot, "efg_rank_table.png", vwidth = 600)
