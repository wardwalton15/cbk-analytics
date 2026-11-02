library(tidyverse)
library(gt)

# Input: one row per team, rank from each source. Include every team that is
# top ~30 in ANY source so the consensus pool is complete. Leave AP blank if unranked.
data <- read_csv("consensus top 20 - Sheet1.csv")
logos_path <- "logos.csv"   # team (Torvik/KenPom names), logo (ESPN URL)

sources <- c("AP", "KenPom", "EvanMiya", "Torvik")
ap_unranked <- 26   # rank assigned to AP "NR" teams when averaging

# Skip any source column that hasn't been filled in yet
sources <- sources[map_lgl(sources, ~ any(!is.na(data[[.x]])))]

# A source "significantly" disagrees when its rank is off from the median of the
# other three sources by at least min_gap spots AND pct_gap of that median
# (so #2 vs #7 gets flagged, but #18 vs #23 doesn't). The gap must also be
# bigger than the spread among the other three, so only true outliers get
# flagged, not every source when the sources split 2-vs-2.
min_gap <- 4
pct_gap <- 0.35

ranks <- data %>%
  mutate(AP = replace_na(AP, ap_unranked)) %>%
  select(Team, all_of(sources))

consensus <- ranks %>%
  mutate(avg = rowMeans(across(all_of(sources)), na.rm = TRUE),
         best = do.call(pmin, c(across(all_of(sources)), na.rm = TRUE))) %>%
  arrange(avg, best) %>%
  mutate(rk = row_number()) %>%
  slice_head(n = 20)

# Leave-one-out comparison: each source vs. the median of the other three
flags <- consensus %>%
  select(Team, all_of(sources)) %>%
  pivot_longer(-Team, names_to = "source", values_to = "rank") %>%
  group_by(Team) %>%
  mutate(others = map_dbl(seq_along(rank), ~ median(rank[-.x], na.rm = TRUE)),
         spread = map_dbl(seq_along(rank), ~ diff(range(rank[-.x], na.rm = TRUE))),
         gap = others - rank,
         sig = abs(gap) >= pmax(min_gap, pct_gap * others) & abs(gap) > spread,
         flag = case_when(
           is.na(gap) | !sig ~ "none",
           gap > 0 ~ "high",
           TRUE ~ "low"
         )) %>%
  ungroup()

# Display labels: arrow on flagged cells, "NR" for unranked AP teams
display <- flags %>%
  mutate(label = case_when(
    source == "AP" & rank >= ap_unranked ~ "NR",
    is.na(rank) ~ "-",
    TRUE ~ as.character(rank)
  ),
  label = case_when(
    flag == "high" ~ paste(label, "\u25B2"),
    flag == "low" ~ paste(label, "\u25BC"),
    TRUE ~ label
  )) %>%
  select(Team, source, label) %>%
  pivot_wider(names_from = source, values_from = label)

tbl_data <- consensus %>%
  select(rk, Team, avg) %>%
  left_join(display, by = "Team") %>%
  select(rk, Team, all_of(sources), avg)

if (file.exists(logos_path)) {
  logos <- read_csv(logos_path)
  tbl_data <- tbl_data %>%
    left_join(logos %>% select(team, logo), by = c("Team" = "team")) %>%
    relocate(logo, .after = rk)
}

tbl <- tbl_data %>%
  gt() %>%
  cols_label(
    rk = "",
    Team = "Team",
    EvanMiya = "EvanMiya",
    avg = "Avg"
  ) %>%
  fmt_number(columns = avg, decimals = 1) %>%
  tab_header(
    title = md("**College Basketball Preseason Top 20**"),
    subtitle = paste("Average rank across", str_flatten_comma(sources, ", and "))
  ) %>%
  tab_source_note(
    source_note = md(paste0(
      "\u25B2 / \u25BC = source ranks the team significantly higher / lower than the other sources ",
      "(", min_gap, "+ spots and ", pct_gap * 100, "%+ off their median, and further off than they are from each other). AP unranked counted as ", ap_unranked, ".<br>",
      "*Chart: Ward Walton | Data: ", str_flatten_comma(sources), "*"
    ))
  ) %>%
  cols_align(align = "center", columns = c(rk, all_of(sources), avg)) %>%
  cols_align(align = "left", columns = Team) %>%
  cols_width(
    rk ~ px(40),
    Team ~ px(175),
    all_of(sources) ~ px(100),
    avg ~ px(75)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_column_labels(), cells_body(columns = c(rk, Team, avg)))
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
      cell_text(color = "#f1eded", size = px(17))
    ),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "#f5f5f5"), cell_text(color = "#333333")),
    locations = cells_column_labels()
  ) %>%
  opt_table_font(font = list(google_font("Inter"), default_fonts())) %>%
  tab_options(
    table.font.size = px(18),
    heading.align = "center",
    heading.padding = px(14),
    column_labels.font.size = px(17),
    column_labels.padding = px(12),
    data_row.padding = px(8),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    source_notes.font.size = px(13),
    source_notes.padding = px(10)
  )

# Shade flagged cells: green = source is higher on the team, red = lower
for (s in sources) {
  high_rows <- which(str_detect(tbl_data[[s]], "\u25B2"))
  low_rows <- which(str_detect(tbl_data[[s]], "\u25BC"))
  if (length(high_rows) > 0) {
    tbl <- tbl %>%
      tab_style(
        style = list(cell_fill(color = "#c8e6c9"), cell_text(color = "#1b5e20", weight = "bold")),
        locations = cells_body(columns = all_of(s), rows = high_rows)
      )
  }
  if (length(low_rows) > 0) {
    tbl <- tbl %>%
      tab_style(
        style = list(cell_fill(color = "#ffcdd2"), cell_text(color = "#b71c1c", weight = "bold")),
        locations = cells_body(columns = all_of(s), rows = low_rows)
      )
  }
}

if ("logo" %in% names(tbl_data)) {
  tbl <- tbl %>%
    text_transform(
      locations = cells_body(columns = logo),
      fn = function(x) web_image(url = x, height = 36)
    ) %>%
    cols_label(logo = "") %>%
    cols_align(align = "center", columns = logo) %>%
    cols_width(logo ~ px(55))
}

tbl

# zoom = 2 for a sharp vertical image on phone screens (TikTok)
gtsave(tbl, "consensus_top20_table.png", vwidth = 750, vheight = 1300, zoom = 2, expand = 10)
