library(tidyverse)
library(gt)

# Preseason: copied KenPom table (first block = preseason ratings, second block =
# current). The header repeats partway down, so keep only rows with a numeric rank.
pre <- read_csv("preseason - Sheet1.csv", col_names = FALSE, skip = 1,
                col_types = cols(.default = "c")) %>%
  transmute(pre_rank = as.integer(X1), Team = X2, pre_adjem = as.numeric(X4)) %>%
  filter(!is.na(pre_rank))

# Current: KenPom summary export
cur <- read_csv("summary27.csv") %>%
  select(Team = TeamName, adjem = AdjEM, rank = RankAdjEM)

logos_path <- "logos.csv"

n_teams <- 15         # rows in the table
max_pre_rank <- 100   # only teams ranked this high preseason (Inf = everyone)
as_of <- "Nov. 16"    # date for the subtitle

ratings <- pre %>%
  inner_join(cur, by = "Team") %>%
  mutate(diff = adjem - pre_adjem,
         rank_diff = pre_rank - rank)

risers <- ratings %>%
  filter(pre_rank <= max_pre_rank) %>%
  arrange(desc(diff)) %>%
  slice_head(n = n_teams)

if (file.exists(logos_path)) {
  logos <- read_csv(logos_path)
  risers <- risers %>%
    left_join(logos %>% select(team, logo), by = c("Team" = "team"))
} else {
  risers <- risers %>% mutate(logo = NA_character_)
}

# AdjEM with the rank in small gray text next to it, e.g. "+24.1  #12"
rating_label <- function(em, rk) {
  sprintf("%+.1f <span style='color:#8a8a8a;font-size:14px;'>&nbsp;#%d</span>", em, rk)
}

tbl_data <- risers %>%
  mutate(
    pre = rating_label(pre_adjem, pre_rank),
    now = rating_label(adjem, rank),
    move = case_when(
      rank_diff > 0 ~ paste("▲", rank_diff),
      rank_diff < 0 ~ paste("▼", abs(rank_diff)),
      TRUE ~ "–"
    )
  ) %>%
  select(logo, Team, pre, now, diff, move)

subtitle <- paste0(
  "Biggest gain in KenPom AdjEM from preseason to ", as_of,
  if (is.finite(max_pre_rank)) paste0(" (preseason top ", max_pre_rank, ")") else ""
)

tbl <- tbl_data %>%
  gt() %>%
  cols_label(
    logo = "",
    Team = "Team",
    pre = "Preseason",
    now = "Now",
    diff = "Change",
    move = "Rank"
  ) %>%
  fmt_markdown(columns = c(pre, now)) %>%
  fmt_number(columns = diff, decimals = 1, force_sign = TRUE) %>%
  data_color(
    columns = diff,
    palette = c("#e8f5e9", "#1b5e20"),
    domain = range(tbl_data$diff)
  ) %>%
  tab_header(
    title = md("**\U0001F4C8 Hottest Starts**"),
    subtitle = subtitle
  ) %>%
  tab_source_note(
    source_note = md("AdjEM = adjusted efficiency margin (points per 100 possessions vs. an average D1 team). Rank = spots gained since preseason.<br>*Chart: Ward Walton | Data: kenpom.com*")
  ) %>%
  cols_align(align = "center", columns = c(pre, now, diff, move)) %>%
  cols_align(align = "left", columns = Team) %>%
  cols_width(
    logo ~ px(55),
    Team ~ px(170),
    pre ~ px(130),
    now ~ px(130),
    diff ~ px(95),
    move ~ px(80)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_column_labels(), cells_body(columns = c(Team, diff)))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "#1b5e20"),
    locations = cells_body(columns = move, rows = str_detect(tbl_data$move, "▲"))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "#b71c1c"),
    locations = cells_body(columns = move, rows = str_detect(tbl_data$move, "▼"))
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "#d3d3d3", weight = px(1)),
    locations = cells_body()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a1a2e"),
      cell_text(color = "white", weight = "bold", size = px(30))
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#1a1a2e"),
      cell_text(color = "#f1eded", size = px(16))
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f5f5f5"), cell_text(color = "#333333")),
    locations = cells_column_labels()
  ) %>%
  text_transform(
    locations = cells_body(columns = logo),
    fn = function(x) ifelse(is.na(x) | x == "NA", "", web_image(url = x, height = 36))
  ) %>%
  opt_table_font(font = list(google_font("Inter"), default_fonts())) %>%
  tab_options(
    table.font.size = px(18),
    heading.align = "center",
    heading.padding = px(14),
    column_labels.font.size = px(16),
    column_labels.padding = px(12),
    data_row.padding = px(8),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    source_notes.font.size = px(13),
    source_notes.padding = px(10)
  )

tbl

gtsave(tbl, "early_risers_table.png", vwidth = 730, vheight = 1100, zoom = 2, expand = 10)
