library(tidyverse)
data <- read_csv("C:/Users/drawn/Documents/CBK-2025-26/cbk-analytics/style.csv")
view(data)
logos <- read_csv("C:/Users/drawn/Documents/KenPomR/logos.csv")
view(logos)
data <- data %>%
  left_join(logos, by = c("Team" = "team")) %>%
    select(logo, Team, c(2:4))

head(data)

# 1. Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggimage) # This package handles plotting images from URLs
library(ggrepel) # Highly recommended for cleaner team labels

table(is.na(cbb_data$Logo_URL))

# 2. Define your data (assuming you have already loaded your data frame 'data')
# You may need to rename columns for clarity, especially the logo column
cbb_data <- data %>%
  rename(Logo_URL = logo,
         Pace = tempo,
         Offensive_Rating = off_rtg,
         Three_Point_Rate = three_pt_rate)

# 3. Calculate League Averages for Quadrant Lines
# Replace these with actual league averages if you have all teams, 
# otherwise use the average of your subset for a relative comparison.
avg_pace <- mean(cbb_data$Pace)
avg_off_rtg <- mean(cbb_data$Offensive_Rating)

cbb_data <- cbb_data %>%
  mutate(Scaled_Size = Three_Point_Rate / 100 * 0.15)

# 4. Create the Plot
cbb_plot <- ggplot(cbb_data, 
                   aes(x = Pace, y = Offensive_Rating)) +
  
  # --- Visualization Layers ---
  
  # 1. Quadrant Lines (The core of the analysis)
  geom_vline(xintercept = avg_pace, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_hline(yintercept = avg_off_rtg, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  
  # 2. Team Logos (The data points)
  # Scale 'size' based on the 3PAr. Adjust 'size' and 'by' until the logos look right.
geom_image(aes(image = Logo_URL, size = I(Scaled_Size)), 
             alpha = 0.8) +
  
  # --- Custom Scales and Labels ---
  
  # Define the range of logo sizes
  scale_size_continuous(name = "3-Point Attempt Rate (%)", 
                        range = c(0.04, 0.1), # Adjust this range to make logos bigger/smaller
                        guide = "legend") + 
  
  # Add titles and axis labels
  labs(title = "College Basketball Offensive Styles",
       subtitle = "Logo size reflects 3-Point Attempt Rate",
       x = "Tempo (Pace)",
       y = "Offensive Rating") +
  
  # --- Styling for Social Media ---
  
  # A clean, minimal theme works best for video backgrounds
  theme_minimal(base_size = 14) + 
  theme(
    # Use a dark background theme for contrast on a screen/video
    panel.background = element_rect(fill = "#1A1A1A", color = NA),
    plot.background = element_rect(fill = "#1A1A1A", color = NA),
    text = element_text(color = "white"),
    
    # Customize the grid lines (optional, can be removed)
    panel.grid.major = element_line(color = "grey30", linewidth = 0.2),
    panel.grid.minor = element_line(color = "grey30", linewidth = 0.1),
    
    # Hide the legend if space is tight on TikTok, or move it.
    legend.position = "bottom",
    
    # Customize titles
    plot.title = element_text(face = "bold", size = 40, color = "white", hjust = 0.5),
    plot.subtitle = element_text(color = "grey80", size = 25, hjust = 0.5),
    axis.title = element_text(size = 25, color = "white")
  )

# 5. Display the plot
cbb_plot

# 6. Save the plot for high-quality video background (adjust dimensions for TikTok aspect ratio)
# TikTok uses a 9:16 aspect ratio (e.g., 1080x1920 pixels).
# ggsave("cbb_style_chart.png", cbb_plot, width = 6, height = 10, units = "in", dpi = 300)

ggplot(cbb_data, aes(x = Pace, y = Offensive_Rating)) +
  geom_point(aes(size = Three_Point_Rate), color = "blue") +
  theme_minimal() + geom_image(aes(image = Logo_URL, size = Three_Point_Rate), 
           alpha = 0.8, 
           by = "width")
