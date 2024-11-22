library(dplyr)
library(readr) # For writing CSV files
data("BabyNames", package = "dplyr")
head(BabyNames)
write_csv(BabyNames, "BabyNames.csv")
write.csv(BabyNames, "BabyNames.csv", row.names = FALSE)
getwd()
write_csv(BabyNames, "/Users/sarahkhan/Desktop/BabyNames.csv")

library(dcData)
library(ggplot2)
library(dplyr)

selected_names <- BabyNames %>%
  filter(name %in% c("Michael", "Jennifer", "David")) %>%  # Select chosen names
  group_by(year, name) %>%                                # Group by year and name
  summarize(total_count = sum(count, na.rm = TRUE)) %>%   # Summarize counts by year and name
  ungroup()

ggplot(selected_names, aes(x = year, y = total_count, color = name, group = name)) +
  geom_line(size = 1) +  # Line thickness for visibility
  labs(
    title = "Popularity of Selected Baby Names Over Time",
    x = "Year",
    y = "Total Number of Babies",
    color = "Name"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

View(BabyNames)

