library(tidyverse)
library(ggimage)
library(ggrepel)

# Load data ---------------------------------------------------------------
poss <- read_csv("poss_length - Sheet1.csv")
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")

# Keep top 10% in offense (higher is better) and defense (lower is better)
off_cut <- quantile(poss$off, 0.875, na.rm = TRUE)
def_cut <- quantile(poss$def, 0.125, na.rm = TRUE)
x_cut <- quantile(poss$off_poss_length, 0.4, na.rm = TRUE)
x_cut2 <- quantile(poss$off_poss_length, 0.6, na.rm = TRUE)

plot_data <- poss %>%
  filter(off >= off_cut, def <= def_cut) %>%
  left_join(logos %>% select(team, logo, color, alternate_color),
            by = c("Team" = "team")) %>%
  drop_na(logo)

# Plot --------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = off_poss_length, y = off)) +
  annotate("rect",
           xmin = 16, xmax = 18,
           ymin = -Inf, ymax = Inf,
           fill = "#d87c7c", alpha = 0.4) +
             annotate("rect",
           xmin = -Inf, xmax = 16,
           ymin = -Inf, ymax = Inf,
           fill = "#83BC87", alpha = 0.4) +
             annotate("rect",
           xmin = 18, xmax = Inf,
           ymin = -Inf, ymax = Inf,
           fill = "#83BC87", alpha = 0.4) +
  geom_hline(yintercept = off_cut, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = x_cut, linetype = "dashed", color = "gray50") +
  geom_image(aes(image = logo), size = 0.06, asp = 1.2) +
  labs(
    title = "Offensive Possession Length for Elite Teams",
    x = "Avg offensive possession length (sec, lower = faster)",
    y = "Offensive efficiency"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 35, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p)
# ggsave("off_vs_possession_top10.png", p, width = 10, height = 7, dpi = 320)
