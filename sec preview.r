library(tidyverse)
data <- read_csv("C:/Users/drawn/Documents/CBK-2025-26/cbk-analytics/sec preview - Sheet1.csv")
view(data)
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(logos)
data <- data %>%
  left_join(logos, by = c("Team" = "team")) %>%
    select(logo, Team, c(2,3))


library(ggimage) # This package handles plotting images from URLs
library(ggrepel) # Highly recommended for cleaner team labels

table(is.na(data$logo))

# 2. Define your data (assuming you have already loaded your data frame 'data')
# You may need to rename columns for clarity, especially the logo column
cbb_data <- data %>%
  rename(Logo_URL = logo,
         Offensive_Rating = `Adj OE`,
         Defensive_Rating = `Adj DE`)

# 3. Calculate League Averages for Quadrant Lines
# Replace these with actual league averages if you have all teams, 
# otherwise use the average of your subset for a relative comparison.
avg_def_rating <- mean(cbb_data$Defensive_Rating)
avg_off_rtg <- mean(cbb_data$Offensive_Rating)


# 4. Create the Plot
cbb_plot <- ggplot(cbb_data, 
                   aes(x = Offensive_Rating, y = -Defensive_Rating)) +
  
  # --- Visualization Layers ---
  
  # 1. Quadrant Lines (The core of the analysis)
  geom_vline(xintercept = avg_off_rtg, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = -avg_def_rating, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  


  # 2. Team Logos (The data points)
geom_image(aes(image = Logo_URL),
                size = .08,
             alpha = 0.8) +
  
  
  # Add titles and axis labels
  labs(title = "SEC Preview",
       subtitle = "Offensive vs Defensive Ratings",
       x = "Offensive Rating",
       y = "Defensive Rating") +
  
  # A clean, minimal theme works best for video backgrounds
  theme_minimal(base_size = 14) + 
  theme(
    # Use a dark background theme for contrast on a screen/video
    panel.background = element_rect(fill = "#8f8f8f", color = NA),
    plot.background = element_rect(fill = "#8f8f8f", color = NA),
    text = element_text(color = "black"),
    
    # Customize the grid lines (optional, can be removed)
    panel.grid.major = element_line(color = "grey30", linewidth = 0.2),
    panel.grid.minor = element_line(color = "grey30", linewidth = 0.1),
    
    # Hide the legend if space is tight on TikTok, or move it.
    legend.position = "bottom",
    
    # Customize titles
    plot.title = element_text(face = "bold", size = 50, color = "#000000", hjust = 0.5),
    plot.subtitle = element_text(color = "black", size = 40, hjust = 0.5),
    axis.title = element_text(size = 25, color = "black")
  )

# 5. Display the plot
cbb_plot


