# Load necessary libraries
pacman::p_load(tidyverse, readxl, gt, gtExtras, ggtext, showtext, webshot2)

# Font
font_add_google(name = "Roboto Condensed")
font_add_google(name = "Ubuntu Condensed")
font_add_google(name = "Aladin")
font_add_google(name = "Raleway")
font_add_google(name = "Rosario")
font_add(family = "fb", regular = "Font Awesome 5 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

# Socials
cap <-  paste0("<span style='font-family:fb;'>&#xf09b;</span>",
               "<span style='font-family:sans;color:white;'></span>",
               "<span style='font-family:Rosario;'> Manasseh Oduor   </span>",
               "<span style='font-family:fb;'>&#xf099; </span>  Manasseh_6 | #RugbyKE")

# Load data
rugby7s_christie_2024 <- read_excel("SportpPesa7s_Stats.xlsx", sheet = "Christie7s")

# Data preparation
# No. of games played by each team
games_played <- rugby7s_christie_2024 |>
  select(Team_1, Team_2) |>
  gather(key = "Type", value = "Team") |>
  group_by(Team) |>
  summarise(Games_Played = n())

# Metrics Summation for each team
team_metrics <- rugby7s_christie_2024 |>
  gather(key = "Type", value = "Team", Team_1, Team_2) |>
  gather(key = "Metric", value = "Value", -Type, -Team) |>
  mutate(Metric = gsub("_1", "", Metric),
         Metric = gsub("_2", "", Metric)) |>
  summarise(Total = sum(Value), .by = c(Team, Metric)) |>
  spread(key = Metric, value = Total) 

# Merge
team_stats <- left_join(team_metrics, games_played, by = "Team")

# Compute Metrics 
team_stats <- team_stats |>
  mutate(
    `Contestable Restarts (%)` = round((`Contestable Restarts` / Restarts)*100, 0),
    `Contestable Restarts Regained (%)` = round((`Contestable Restarts Regained` / `Contestable Restarts`)*100, 0),
    `Contestable Receipts Retained (%)` = round((`Contestable Receipts Secured` / `Contestable Restarts`)*100, 0),
    `Non-Contestable Restarts (%)` = round((`Non-Contestable Restarts` / Restarts)*100, 0),
    `Restart Error Rate %` = round((`Restart Errors` / Restarts)*100, 0),
    `Average Restart Errors per Game` = round(`Restart Errors` / `Games_Played`, 1),
    `Scrums (average per game)` = round(`Scrums` / `Games_Played`, 1),
    `Total Scrums Faced` = `Scrums` + `Opposition Scrum Won`,
    `Opposition Scrum Won (%)` = round((`Opposition Scrum Won` / `Total Scrums Faced`)*100, 0),
    `Scrums Retained (%)` = round((`Scrums Retained` / `Scrums`)*100, 0),
    `Lineouts (average per game)` = round(`Lineouts` / `Games_Played`,1),
    `Lineouts Retained (%)` = round((`Lineouts Retained` / `Lineouts`)*100, 0),
    `Total Lineouts Faced` = `Lineouts` + `Opposition Lineouts Won`,
    `Opposition Lineouts Won (%)` = round((`Opposition Lineouts Won` / `Total Lineouts Faced`)*100, 0)
  ) |>
  select(Team, `Games_Played`, everything())

team_stats <- team_stats |>
  select(-Row_ID)

# Table
T1 <- team_stats |>
  select(Team, `Contestable Restarts (%)`, `Non-Contestable Restarts (%)`, `Restart Error Rate %`, `Average Restart Errors per Game`) |>
  arrange(desc(`Average Restart Errors per Game`)) |>  
  gt() |>
  gtExtras::gt_theme_538() |>
  data_color(
    columns = `Contestable Restarts (%)`,
    colors = scales::col_numeric(
      palette = c("lightgreen", "darkgreen"),
      domain = NULL
    )
  ) |>
  data_color(
    columns = `Non-Contestable Restarts (%)`,
    colors = scales::col_numeric(
      palette = c("lightgreen", "darkgreen"),
      domain = NULL
    )
  ) |>
  data_color(
    columns = `Restart Error Rate %`,
    colors = scales::col_numeric(
      palette = c("lightgreen", "darkgreen"),
      domain = NULL
    )
  ) |>
  data_color(
    columns = `Average Restart Errors per Game`,
    colors = scales::col_numeric(
      palette = c("lightgreen", "darkgreen"),
      domain = NULL
    )
  ) |>
  tab_header(title = "2024 SportPesa Christie 7s: Kick-Off Set Piece Analysis") |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_column_labels()
    )
  ) |> 
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body()
  ) |>
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ), align = "left",v_align = "middle")),
    locations = cells_column_labels(
      columns = c(Team, `Contestable Restarts (%)`, `Non-Contestable Restarts (%)`, `Restart Error Rate %`, `Average Restart Errors per Game`)
    )
  ) |>
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"),align = 'left'
      )),
    locations = cells_body(
      columns = c(Team)
    )
  ) |>
  tab_style(
    style = list(
      cell_text(font=google_font(
        name = "Roboto Condensed"
      ))),
    locations = cells_source_notes()
  ) |>
  tab_source_note(
    source_note = md("**Table:** Manasseh Oduor | #Rstats | #RugbyKE | #Christies7s2024"))


gtsave_extra(data=T1, filename='2024_christie7s_kick-0ff.png')

