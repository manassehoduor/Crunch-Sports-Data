# Libraries
library(rio)
library(tidyverse)
library(gt)
library(gtExtras)

# Cleaning data
source_data = import("source_data.xlsx", sheet = "source_data")

# Add 2022 Christies 7s Winners
source_data <- source_data %>% 
  add_row(tibble_row(Year = 2022, Team = "Strathmore Leos"))

# Remove seasons that were not played due to Covid-19 
source_data <- source_data %>%
  filter(!str_detect(Team, 'Covid'))

df <- source_data %>% 
  select(Team) %>% 
  count(Team, sort = TRUE) %>% 
  rename(No..of.times.won = n)

# Import processed data
df = read.csv(file = "roll.csv", header = TRUE, sep = ",")

# Table
p1 <- df %>%
  select(Team, No..of.times.won) %>% 
  gt() %>%
  gt_fa_repeats(No..of.times.won, name = "trophy") %>% 
  tab_header(
    title = md("Who has won Christie's 7s the most?"),
    subtitle = add_text_img(
      "Table created with",
      url = "https://www.r-project.org/logo/Rlogo.png"
    )
  ) %>% 
  tab_source_note(md("**Table Credits:**: Manasseh | **Source**: @OfficialKRU ~ Christie's Roll of Honor <br> **Inspiration**: Thomas Mock | **Date Created:** 7th-June-2022")) %>% 
  opt_table_font(
    font = list(
      google_font("Orienta")
    )) %>% 
  tab_options(
    heading.title.font.weight = "bold",
    heading.title.font.size = 26,
    heading.subtitle.font.size = 14,
    table.border.bottom.color = "#450bb3",
    table.border.top.color = "#450bb3",
    heading.align = "left",
    column_labels.border.bottom.color = "#450bb3",
    column_labels.border.lr.color = "#450bb3",
    column_labels.border.bottom.width= px(2),
    footnotes.border.lr.color = "#450bb3",
    footnotes.border.bottom.width = px(2),
    table.width = px(550),
    table.font.size = px(14L),
    heading.padding = px(2),
    data_row.padding = px(2)
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "#f7fcfc",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>% 
  cols_label(
    Team = html(
      "<span style='color:#171c13;'>TEAM</span>"
    ), 
    No..of.times.won = html(
      "<span style='color:#171c13;'>CHAMPIONS</span>"
    ))

# Save
gtsave(p1, "christies.png")

