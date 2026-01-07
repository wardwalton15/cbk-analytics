library(tidyverse)
library(ggimage)
library(ggrepel)

# Load data ---------------------------------------------------------------
data <- read_csv("mid majors - Sheet1.csv")
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(data)
# Keep top 10% in offense (higher is better) and defense (lower is better)


plot_data <- data %>%
  filter(Barthag >= .5) %>%
  left_join(logos %>% select(team, logo, color, alternate_color),
            by = c("Team" = "team")) 

# Plot --------------------------------------------------------------------
p <- ggplot(plot_data, aes(x = `Adj OE`, y = -`Adj DE`)) +
  # Adding Quadrant Backgrounds
  # Top Right (Green)
  annotate("rect", xmin = 108, xmax = Inf, ymin = -108, ymax = Inf, fill = "#c6efce", alpha = 0.5) +
  # Bottom Left (Red)
  annotate("rect", xmin = -Inf, xmax = 108, ymin = -Inf, ymax = -108, fill = "#ffc7ce", alpha = 0.5) +
  # Top Left (Yellow)
  annotate("rect", xmin = -Inf, xmax = 108, ymin = -108, ymax = Inf, fill = "#ffeb9c", alpha = 0.5) +
  # Bottom Right (Yellow)
  annotate("rect", xmin = 108, xmax = Inf, ymin = -Inf, ymax = -108, fill = "#ffeb9c", alpha = 0.5) +
  
  # Your original layers
  geom_hline(yintercept = -108, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 108, linetype = "dashed", color = "red") +
  geom_image(aes(image = logo), size = 0.06, asp = 1.2) +
  labs(
    title = "Mid-Major Check In",
    x = "Offensive Efficiency",
    y = "Defensive Efficiency"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 35, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p)

