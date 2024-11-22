library(dcData)
library(dplyr)
library(ggplot2)

# Data Wrangling: Filter and summarize data for selected names
selected_names <- BabyNames %>%
  filter(name %in% c("Michael", "Jennifer", "David")) %>%  # Filter for chosen names
  group_by(year, name) %>%                                 # Group by year and name
  summarize(total_count = sum(count, na.rm = TRUE)) %>%    # Summarize yearly counts
  ungroup()

# Visualization: Line plot of name popularity over time
ggplot(selected_names, aes(x = year, y = total_count, color = name, group = name)) +
  geom_line(size = 1) +  # Add lines with thickness for visibility
  labs(
    title = "Popularity of Selected Baby Names Over Time",             # Main title
    subtitle = "Tracking the popularity of Michael, Jennifer, and David from 1880 onwards",  # Subtitle for context
    x = "Year",                                                        # Label for x-axis
    y = "Total Number of Babies",                                      # Label for y-axis
    color = "Name"                                                     # Legend title
  ) +
  theme_minimal() +  # Apply minimal theme for a clean look
  theme(
    plot.title = element_text(hjust = 0.5),  # Center-align title
    plot.subtitle = element_text(hjust = 0.5),  # Center-align subtitle
    legend.position = "bottom"  # Place legend at the bottom
  )