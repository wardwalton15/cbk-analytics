library(tidyverse)
library(ggimage)
library(ggrepel)

# Load data ---------------------------------------------------------------
data <- read_csv("bubble1 - Sheet1.csv")
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(plot_data)


plot_data <- data %>%
  left_join(logos %>% select(team, logo, color, alternate_color),
            by = c("Team" = "team")) 
head(plot_data)

p <- ggplot(plot_data, aes(x = KenPom, y = SOR)) +
  # Quadrant Backgrounds (Using scale_y_reverse logic: low SOR is "top")
  # Top Right: High KenPom, Low SOR Rank (Green - Elite Resume)
  annotate("rect", xmin = 18, xmax = Inf, ymin = -Inf, ymax = 40, fill = "#c6efce", alpha = 0.5) +
  # Bottom Left: Low KenPom, High SOR Rank (Red - Weak Resume)
  annotate("rect", xmin = -Inf, xmax = 18, ymin = 40, ymax = Inf, fill = "#ffc7ce", alpha = 0.5) +
  # Top Left: Low KenPom, Low SOR Rank (Yellow - Overachieving/Good Record)
  annotate("rect", xmin = -Inf, xmax = 18, ymin = -Inf, ymax = 40, fill = "#ffeb9c", alpha = 0.5) +
  # Bottom Right: High KenPom, High SOR Rank (Yellow - Underachieving/Analytical Darling)
  annotate("rect", xmin = 18, xmax = Inf, ymin = 40, ymax = Inf, fill = "#ffeb9c", alpha = 0.5) +
  
  # Reference Lines
  geom_hline(yintercept = 40, linetype = "dashed", color = "black", alpha = 0.4) +
  geom_vline(xintercept = 18, linetype = "dashed", color = "black", alpha = 0.4) +
  
  # Team Logos
  geom_image(aes(image = logo), size = 0.06, asp = 1.2) +
  
  # Reverse Y-Axis so 1 is at the top
  scale_y_reverse(breaks = seq(0, 100, by = 10)) +
  
  labs(
    title = "NCAA Bubble Resume Check-In",
    subtitle = "Better Resumes are Top-Right",
    x = "KenPom Rating (Quality)",
    y = "Strength of Record Rank (Results)"
  ) +
  theme_minimal(base_family = "Helvetica") +
  theme(
    plot.title = element_text(face = "bold", size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.minor = element_blank()
  )
print(p)

p2 <- p + 
  annotate("text", x = 21, y = 18, label = "Safe (For Now)", 
           color = "#1b5e20", fontface = "bold", size = 12) +
  
  annotate("text", x = 13, y = 16, label = "Overachievers", 
           color = "#827717", fontface = "bold", size = 12) +
  
  annotate("text", x = 21, y = 60, label = "Analytical Darlings", 
           color = "#827717", fontface = "bold", size = 12) +
  
  annotate("text", x = 13, y = 60, label = "Work to Do", 
           color = "#b71c1c", fontface = "bold", size = 12)
print(p2)
