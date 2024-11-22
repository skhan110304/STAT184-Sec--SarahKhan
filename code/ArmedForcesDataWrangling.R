library(tidyverse)
library(rvest)
library(readr)
library(janitor)
library(knitr)
library(kableExtra)


ArmedForcesList <- read_html("https://docs.google.com/spreadsheets/d/1cn4i0-ymB1ZytWXCwsJiq6fZ9PhGLUvbMBHlzqG4bwo/edit?gid=597536282#gid=597536282") %>%
  html_element(css = "body") %>%
  html_table()

ArmedForces <- ArmedForcesList[[1]]

ArmedForcesByGroup <- ArmedForces %>%
  slice(c(42, 14, 18, 29)) %>%
  pivot_longer(
    cols = c("Army.Male", "Army.Female", "Navy.Male", "Navy.Female", "MarineCorps.Male", "MarineCorps.Female", "AirForce.Male", "AirForce.Female", "SpaceForce.Male", "SpaceForce.Female"),
    names_to = "Branch",
    values_to = "Total"
  ) %>%
  separate_wider_delim(
    cols = Branch,
    delim = ".",
    names = c("Branch", "Gender")
  )

ArmedForcesByIndividual <- ArmedForcesByGroup %>%
  mutate(
    Total = readr::parse_number(Total, na = c("N/A", "NA"))
  ) %>%
  filter(!is.na(Total))

ArmedForcesByIndividual <- uncount(ArmedForcesByIndividual, Total)

View(ArmedForcesByIndividual)


# Load required packages
library(tidyverse)
library(janitor)
library(kableExtra)

# Step 1: Filter data by gender
males_data <- ArmedForcesByIndividual %>%
  filter(Gender == "Male")

females_data <- ArmedForcesByIndividual %>%
  filter(Gender == "Female")

# Step 2: Group and summarize data by Branch and Pay Grade
males_summary <- males_data %>%
  group_by(Branch, `Pay Grade`) %>%
  summarize(count = n(), .groups = "drop")

females_summary <- females_data %>%
  group_by(Branch, `Pay Grade`) %>%
  summarize(count = n(), .groups = "drop")

# Step 3: Reshape data to create frequency table layout
males_table <- males_summary %>%
  pivot_wider(names_from = Branch, values_from = count, values_fill = 0)

females_table <- females_summary %>%
  pivot_wider(names_from = Branch, values_from = count, values_fill = 0)

# Step 4: Format and display tables
males_table %>%
  kable("html", caption = "Frequency Table for Males by Rank and Branch") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

females_table %>%
  kable("html", caption = "Frequency Table for Females by Rank and Branch") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

